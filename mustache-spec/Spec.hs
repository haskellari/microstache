{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}

module Main (main) where

import Control.Monad
import Data.Aeson
import Data.ByteString (ByteString)
import Data.Map (Map, (!))
import Data.Text (Text)
import Test.Hspec
import Text.Parsec
import Text.Microstache
import Text.Microstache.Parser
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Map       as M
import qualified Data.Text      as T
import qualified Data.Text.Lazy as TL

-- | Representation of information contained in a Mustache spec file.

data SpecFile = SpecFile
  { specOverview :: Text   -- ^ Textual overview of this spec file
  , specTests    :: [Test] -- ^ The actual collection of tests
  }

instance FromJSON SpecFile where
  parseJSON = withObject "Mustache spec file" $ \o -> do
    specOverview <- o .: "overview"
    specTests    <- o .: "tests"
    return SpecFile {..}

-- | Representation of a single test.

data Test = Test
  { testName     :: String
  , testDesc     :: String
  , testData     :: Value
  , testTemplate :: TL.Text
  , testExpected :: TL.Text
  , testPartials :: Map Text TL.Text
  }

instance FromJSON Test where
  parseJSON = withObject "Test" $ \o -> do
    testName     <- o .: "name"
    testDesc     <- o .: "desc"
    testData     <- o .: "data"
    testTemplate <- o .: "template"
    testExpected <- o .: "expected"
    testPartials <- o .:? "partials" .!= M.empty
    return Test {..}

main :: IO ()
main = spec >>= hspec

spec :: IO Spec
spec = do
  s1 <- specData "Comments"      "specification/comments.json"
  s2 <- specData "Delimiters"    "specification/delimiters.json"
  s3 <- specData "Interpolation" "specification/interpolation.json"
  s4 <- specData "Inverted"      "specification/inverted.json"
  s5 <- specData "Partials"      "specification/partials.json"
  s6 <- specData "Sections"      "specification/sections.json"
  return $ s1 >> s2 >> s3 >> s4 >> s5 >> s6

specData :: String -> FilePath -> IO Spec
specData aspect name = do
    bytes <- BSL.readFile name
    return (specData' bytes)
  where
    specData' bytes = describe aspect $ do
      let handleError = expectationFailure . show
      case eitherDecode bytes of
        Left err ->
          it "should load YAML specs first" $
            expectationFailure (show err)
        Right SpecFile {..} ->
          forM_ specTests $ \Test {..} ->
            it (testName ++ ": " ++ testDesc) $
              case compileMustacheText (PName $ T.pack testName) testTemplate of
                Left perr -> handleError perr
                Right Template {..} -> do
                  ps1 <- forM (M.keys testPartials) $ \k -> do
                    let pname = PName k
                    case parseMustache (T.unpack k) (testPartials ! k) of
                      Left perr -> handleError perr >> undefined
                      Right ns  -> return (pname, ns)
                  let ps2 = M.fromList ps1 `M.union` templateCache
                  let (_ws, t) = renderMustacheW (Template templateActual ps2) testData
                  -- _ <- traverse print _ws
                  t `shouldBe` testExpected
