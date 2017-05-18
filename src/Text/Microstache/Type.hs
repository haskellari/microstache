-- |
-- Module      :  Text.Microstache.Type
-- Copyright   :  © 2016–2017 Stack Buliders
-- License     :  BSD 3 clause
--
-- Maintainer  :  Mark Karpov <markkarpov@openmailbox.org>
-- Stability   :  experimental
-- Portability :  portable
--
-- Types used by the package. You don't usually need to import the module,
-- because "Text.Microstache" re-exports everything you may need, import that
-- module instead.

{-# LANGUAGE CPP                        #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}

module Text.Microstache.Type
  ( Template (..)
  , Node (..)
  , Key (..)
  , showKey
  , PName (..)
  , MustacheException (..) )
where

import Data.Word (Word)
import Control.DeepSeq
import Control.Exception (Exception(..))
import Data.Data (Data)
import Data.Map (Map)
import Data.Semigroup
import Data.String (IsString (..))
import Data.Text (Text)
import Data.Typeable (Typeable)
import GHC.Generics
import Text.Parsec
import qualified Data.Map  as M
import qualified Data.Text as T

-- | Mustache template as name of “top-level” template and a collection of
-- all available templates (partials).
--
-- 'Template' is a 'Semigroup'. This means that you can combine 'Template's
-- (and their caches) using the @('<>')@ operator, the resulting 'Template'
-- will have the same currently selected template as the left one. Union of
-- caches is also left-biased.

data Template = Template
  { templateActual :: PName
    -- ^ Name of currently “selected” template (top-level one).
  , templateCache  :: Map PName [Node]
    -- ^ Collection of all templates that are available for interpolation
    -- (as partials). The top-level one is also contained here and the
    -- “focus” can be switched easily by modifying 'templateActual'.
  } deriving (Eq, Ord, Show, Data, Typeable, Generic)

instance Semigroup Template where
  (Template pname x) <> (Template _ y) = Template pname (M.union x y)

-- | Structural element of template.

data Node
  = TextBlock       Text       -- ^ Plain text contained between tags
  | EscapedVar      Key        -- ^ HTML-escaped variable
  | UnescapedVar    Key        -- ^ Unescaped variable
  | Section         Key [Node] -- ^ Mustache section
  | InvertedSection Key [Node] -- ^ Inverted section
  | Partial         PName (Maybe Word)
    -- ^ Partial with indentation level ('Nothing' means it was inlined)
  deriving (Eq, Ord, Show, Data, Typeable, Generic)

-- | Identifier for values to interpolate.
--
-- The representation is the following:
--
--     * @[]@ — empty list means implicit iterators;
--     * @[text]@ — single key is a normal identifier;
--     * @[text1, text2]@ — multiple keys represent dotted names.

newtype Key = Key { unKey :: [Text] }
  deriving (Eq, Ord, Show, Semigroup, Monoid, Data, Typeable, Generic)

instance NFData Key

-- | Pretty-print a key, this is helpful, for example, if you want to
-- display an error message.

showKey :: Key -> Text
showKey (Key []) = "<implicit>"
showKey (Key xs) = T.intercalate "." xs

-- | Identifier for partials. Note that with the @OverloadedStrings@
-- extension you can use just string literals to create values of this type.

newtype PName = PName { unPName :: Text }
  deriving (Eq, Ord, Show, Data, Typeable, Generic)

instance IsString PName where
  fromString = PName . T.pack

instance NFData PName

-- | Exception that is thrown when parsing of a template has failed or
-- referenced values were not provided.

data MustacheException
  = MustacheParserException ParseError
    -- ^ Template parser has failed. This contains the parse error.
    --
    -- /Before version 0.2.0 it was called 'MustacheException'./
  | MustacheRenderException PName Key
    -- ^ A referenced value was not provided. The exception provides info
    -- about partial in which the issue happened 'PName' and name of the
    -- missing key 'Key'.
  deriving (Eq, Show, Typeable, Generic)

#if MIN_VERSION_base(4,8,0)
instance Exception MustacheException where
  displayException (MustacheParserException e) = show e
  displayException (MustacheRenderException pname key) =
    "Referenced value was not provided in partial \"" ++ T.unpack (unPName pname) ++
    "\", key: " ++ T.unpack (showKey key)
#else
instance Exception MustacheException
#endif
