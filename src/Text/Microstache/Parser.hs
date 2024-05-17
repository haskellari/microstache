-- |
-- Module      :  Text.Microstache.Parser
-- Copyright   :  © 2016–2017 Stack Builders
-- License     :  BSD 3 clause
--
-- Maintainer  :  Mark Karpov <markkarpov@openmailbox.org>
-- Stability   :  experimental
-- Portability :  portable
--
-- Megaparsec parser for Mustache templates. You don't usually need to
-- import the module, because "Text.Microstache" re-exports everything you may
-- need, import that module instead.

module Text.Microstache.Parser
  ( parseMustache )
where

import Control.Applicative   (Alternative (..), (<$), (<$>))
import Control.Monad         (unless, void)
import Data.Char             (isAlphaNum, isSpace)
import Data.Functor.Identity (Identity, runIdentity)
import Data.List             (intercalate)
import Data.Maybe            (catMaybes)
import Data.Text.Lazy        (Text)
import Data.Word             (Word)
import Text.Parsec
       (ParseError, ParsecT, Stream, anyChar, between, char, choice, eof,
       getPosition, getState, label, lookAhead, manyTill, notFollowedBy, oneOf,
       putState, runParserT, satisfy, sepBy1, sourceColumn, spaces, string, try,
       (<?>))
import Text.Parsec.Char ()

import Text.Microstache.Type

import qualified Data.Text as T

----------------------------------------------------------------------------
-- Parser

-- | Parse given Mustache template.

parseMustache
  :: FilePath
     -- ^ Location of file to parse
  -> Text
     -- ^ File contents (Mustache template)
  -> Either ParseError [Node]
     -- ^ Parsed nodes or parse error
parseMustache name contents = runIdentity (runParserT (pMustache eof) (Delimiters "{{" "}}") name contents)

pMustache :: Parser () -> Parser [Node]
pMustache = fmap catMaybes . manyTill (choice alts)
  where
    alts =
      [ Nothing <$  withStandalone pComment
      , Just    <$> pSection "#" Section
      , Just    <$> pSection "^" InvertedSection
      , Just    <$> pStandalone (pPartial Just)
      , Just    <$> pPartial (const Nothing)
      , Nothing <$  withStandalone pSetDelimiters
      , Just    <$> pUnescapedVariable
      , Just    <$> pUnescapedSpecial
      , Just    <$> pEscapedVariable
      , Just    <$> pTextBlock ]
{-# INLINE pMustache #-}

pTextBlock :: Parser Node
pTextBlock = do
  start <- gets openingDel
  (void . notFollowedBy . string') start
  let terminator = choice
        [ (void . lookAhead . string') start
        , pBol
        , eof ]
  TextBlock . T.pack <$> someTill anyChar terminator
{-# INLINE pTextBlock #-}

pUnescapedVariable :: Parser Node
pUnescapedVariable = UnescapedVar <$> pTag "&"
{-# INLINE pUnescapedVariable #-}

pUnescapedSpecial :: Parser Node
pUnescapedSpecial = do
  start <- gets openingDel
  end   <- gets closingDel
  between (symbol $ start ++ "{") (string $ "}" ++ end) $
    UnescapedVar <$> pKey
{-# INLINE pUnescapedSpecial #-}

pSection :: String -> (Key -> [Node] -> Node) -> Parser Node
pSection suffix f = do
  key   <- withStandalone (pTag suffix)
  nodes <- (pMustache . withStandalone . pClosingTag) key
  return (f key nodes)
{-# INLINE pSection #-}

pPartial :: (Word -> Maybe Word) -> Parser Node
pPartial f = do
  pos <- f <$> indentLevel
  key <- pTag ">"
  let pname = PName $ T.intercalate (T.pack ".") (unKey key)
  return (Partial pname pos)
{-# INLINE pPartial #-}

pComment :: Parser ()
pComment = void $ do
  start <- gets openingDel
  end   <- gets closingDel
  (void . symbol) (start ++ "!")
  manyTill anyChar (string end)
{-# INLINE pComment #-}

pSetDelimiters :: Parser ()
pSetDelimiters = void $ do
  start <- gets openingDel
  end   <- gets closingDel
  (void . symbol) (start ++ "=")
  start' <- pDelimiter <* scn
  end'   <- pDelimiter <* scn
  (void . string) ("=" ++ end)
  putState (Delimiters start' end')
{-# INLINE pSetDelimiters #-}

pEscapedVariable :: Parser Node
pEscapedVariable = EscapedVar <$> pTag ""
{-# INLINE pEscapedVariable #-}

withStandalone :: Parser a -> Parser a
withStandalone p = pStandalone p <|> p
{-# INLINE withStandalone #-}

pStandalone :: Parser a -> Parser a
pStandalone p = pBol *> try (between sc (sc <* (void eol <|> eof)) p)
{-# INLINE pStandalone #-}

pTag :: String -> Parser Key
pTag suffix = do
  start <- gets openingDel
  end   <- gets closingDel
  between (symbol $ start ++ suffix) (string end) pKey
{-# INLINE pTag #-}

pClosingTag :: Key -> Parser ()
pClosingTag key = do
  start <- gets openingDel
  end   <- gets closingDel
  let str = keyToString key
  void $ between (symbol $ start ++ "/") (string end) (symbol str)
{-# INLINE pClosingTag #-}

pKey :: Parser Key
pKey = (fmap Key . lexeme . flip label "key") (implicit <|> other)
  where
    implicit = [] <$ char '.'
    other    = sepBy1 (T.pack <$> some ch) (char '.')
    ch       = alphaNumChar <|> oneOf "-_"
{-# INLINE pKey #-}

pDelimiter :: Parser String
pDelimiter = some (satisfy delChar) <?> "delimiter"
  where delChar x = not (isSpace x) && x /= '='
{-# INLINE pDelimiter #-}

indentLevel :: Parser Word
indentLevel = fmap (fromIntegral . sourceColumn) getPosition

pBol :: Parser ()
pBol = do
  level <- indentLevel
  unless (level == 1) empty
{-# INLINE pBol #-}

----------------------------------------------------------------------------
-- Auxiliary types

-- | Type of Mustache parser monad stack.

type Parser = ParsecT Text Delimiters Identity

-- | State used in Mustache parser. It includes currently set opening and
-- closing delimiters.

data Delimiters = Delimiters
  { openingDel :: String
  , closingDel :: String }

----------------------------------------------------------------------------
-- Lexer helpers and other

-- TODO: OLEG inline
scn :: Parser ()
scn = spaces
{-# INLINE scn #-}

sc :: Parser ()
sc = void (many (oneOf " \t"))
{-# INLINE sc #-}

lexeme :: Parser a -> Parser a
lexeme p = p <* spaces
{-# INLINE lexeme #-}

eol :: Parser ()
eol = void (char '\n') <|> void (char '\r' >> char '\n')

string' :: String -> Parser String
string' = try . string

symbol :: String -> Parser String
symbol = lexeme . string'
{-# INLINE symbol #-}

keyToString :: Key -> String
keyToString (Key []) = "."
keyToString (Key ks) = intercalate "." (T.unpack <$> ks)
{-# INLINE keyToString #-}

someTill :: Stream s m t => ParsecT s u m a -> ParsecT s u m end -> ParsecT s u m [a]
someTill p end = (:) <$> p <*> manyTill p end

gets :: Monad m => (u -> a) -> ParsecT s u m a
gets f = fmap f getState

alphaNumChar :: Parser Char
alphaNumChar = satisfy isAlphaNum
