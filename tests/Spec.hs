module Main (main) where

import Test.Hspec

import qualified Text.Microstache.ParserSpec as P
import qualified Text.Microstache.RenderSpec as R
import qualified Text.Microstache.TypeSpec as T

main :: IO ()
main = hspec $ do
    P.spec
    R.spec
    T.spec
