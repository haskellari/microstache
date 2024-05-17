{-# LANGUAGE OverloadedStrings #-}

module Text.Microstache.ParserSpec
  ( main
  , spec )
where

import Control.Monad (unless)
import Data.List.NonEmpty (NonEmpty (..))
import Test.Hspec
import Text.Parsec
import Text.Microstache.Parser
import Text.Microstache.Type
import qualified Data.List.NonEmpty as NE
import qualified Data.Set           as S

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "parseMustache" $ do
  let p = parseMustache ""
      key = Key . pure
  it "parses text" $
    p "test12356p0--=-34{}jnv,\n"
      `shouldParse` [TextBlock "test12356p0--=-34{}jnv,\n"]
  context "when parsing a variable" $ do
    context "with white space" $ do
      it "parses escaped {{ variable }}" $
        p "{{ name }}" `shouldParse` [EscapedVar (key "name")]
      it "parses unescaped {{{ variable }}}" $
        p "{{{ name }}}" `shouldParse` [UnescapedVar (key "name")]
      it "parses unescaped {{& variable }}" $
        p "{{& name }}" `shouldParse` [UnescapedVar (key "name")]
    context "without white space" $ do
      it "parses escaped {{variable}}" $
        p "{{name}}" `shouldParse` [EscapedVar (key "name")]
      it "parses unescaped {{{variable}}}" $
        p "{{{name}}}" `shouldParse` [UnescapedVar (key "name")]
      it "parses unescaped {{& variable }}" $
        p "{{&name}}" `shouldParse` [UnescapedVar (key "name")]
    it "allows '-' in variable names" $
      p "{{ var-name }}" `shouldParse` [EscapedVar (key "var-name")]
    it "allows '_' in variable names" $
      p "{{ var_name }}" `shouldParse` [EscapedVar (key "var_name")]
  context "when parsing a section" $ do
    it "parses empty section" $
      p "{{#section}}{{/section}}" `shouldParse` [Section (key "section") []]
    it "parses non-empty section" $
      p "{{# section }}Hi, {{name}}!\n{{/section}}" `shouldParse`
        [Section (key "section")
         [ TextBlock "Hi, "
         , EscapedVar (key "name")
         , TextBlock "!\n"]]
  context "when parsing an inverted section" $ do
    it "parses empty inverted section" $
      p "{{^section}}{{/section}}" `shouldParse`
        [InvertedSection (key "section") []]
    it "parses non-empty inverted section" $
      p "{{^ section }}No one here?!\n{{/section}}" `shouldParse`
        [InvertedSection (key "section") [TextBlock "No one here?!\n"]]
  context "when parsing a partial" $ do
    it "parses a partial with white space" $
      p "{{> that-s_my-partial }}" `shouldParse`
        [Partial "that-s_my-partial" (Just 1)]
    it "parses a partial without white space" $
      p "{{>that-s_my-partial}}" `shouldParse`
        [Partial "that-s_my-partial" (Just 1)]
    it "handles indented partial correctly" $
      p "   {{> next_one }}" `shouldParse`
        [Partial "next_one" (Just 4)]
  context "when running into delimiter change" $ do
    it "has effect" $
      p "{{=<< >>=}}<<var>>{{var}}" `shouldParse`
        [EscapedVar (key "var"), TextBlock "{{var}}"]
    it "handles whitespace just as well" $
      p "{{=<<   >>=}}<<  var >>{{ var  }}" `shouldParse`
        [EscapedVar (key "var"), TextBlock "{{ var  }}"]
    it "affects {{{s" $
      p "{{=<< >>=}}<<{var}>>" `shouldParse`
        [UnescapedVar (key "var")]
    it "parses two subsequent delimiter changes" $
      p "{{=(( ))=}}(( var ))((=-- $-=))--#section$---/section$-" `shouldParse`
        [EscapedVar (key "var"), Section (key "section") []]
    it "propagates delimiter change from a nested scope" $
      p "{{#section}}{{=<< >>=}}<</section>><<var>>" `shouldParse`
        [Section (key "section") [], EscapedVar (key "var")]
  context "when given malformed input" $ do
    it "rejects unclosed tags" $
      shouldFailParse $ p "{{ name"
    it "rejects unknown tags" $
      shouldFailParse $ p "{{? boo }}"

-------------------------------------------------------------------------------
-- Tools
-------------------------------------------------------------------------------

shouldParse :: (Eq a, Show a) => Either ParseError a -> a -> Expectation
shouldParse (Left e) v = expectationFailure $
    "expected: " ++ show v ++
    "\nbut parsing failed with error:\n" ++ show e
shouldParse (Right x) v = unless (x == v) $ expectationFailure $
    "expected: " ++ show v ++
    "\nbut got: " ++ show x

shouldFailParse :: (Eq a, Show a) => Either ParseError a -> Expectation
shouldFailParse (Left _) = pure ()
shoulwFailParse (Right x) = expectationFailure $ 
    "expected parse failure, got " ++ show x

