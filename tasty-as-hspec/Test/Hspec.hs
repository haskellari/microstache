{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeOperators              #-}
-- | A @hspec@ like interface build on top of tasty.
module Test.Hspec (
    -- * Runner
    Spec,
    hspec,
    -- * Test trees
    describe,
    context,
    it,
    -- * Checks
    Expectation,
    expectationFailure,
    shouldBe,
    shouldContain,
    shouldNotContain,
    compareWith,
) where

import Control.Applicative (Applicative)
import Control.Monad       (unless)
import Data.List           (isInfixOf)
import Data.Orphans ()
import Prelude
       (Bool, Eq, Functor, IO, Monad, Show, String, flip, fst, id, not, show,
       ($), (++))
import Test.Tasty          (TestName, TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit
       (Assertion, HasCallStack, assertFailure, testCase, (@?=))

-------------------------------------------------------------------------------
-- Runner
-------------------------------------------------------------------------------

type Spec = TestTreeM ()

hspec :: TestTreeM () -> IO ()
hspec t = defaultMain (testGroup "X" (runTestTreeM t))

-------------------------------------------------------------------------------
-- Test trees
-------------------------------------------------------------------------------

newtype TestTreeM a = TestTreeM (Writer [TestTree] a)
  deriving (Functor, Applicative, Monad)

runTestTreeM :: TestTreeM () -> [TestTree]
runTestTreeM (TestTreeM m) = fst (runWriter m)

class Describe r where
    describe :: TestName -> TestTreeM () -> r

instance a ~ () =>  Describe (TestTreeM a) where
    describe n t = TestTreeM $ tell [ describe n t ]

instance Describe TestTree where
    describe n t = testGroup n $ runTestTreeM t

it :: TestName -> Assertion -> TestTreeM ()
it n assertion = TestTreeM $ tell [ testCase n assertion ]

context :: Describe r => TestName -> TestTreeM () -> r
context n t = describe n t

-------------------------------------------------------------------------------
-- Checks
-------------------------------------------------------------------------------

type Expectation = Assertion

expectationFailure :: String -> Expectation
expectationFailure = assertFailure

shouldBe :: (Eq a, Show a, HasCallStack) => a -> a -> Expectation
shouldBe = (@?=)

shouldContain :: (Eq a, Show a, HasCallStack) => [a] -> [a] -> Expectation
shouldContain = compareWith (flip isInfixOf) "does not contain"

shouldNotContain :: (Eq a, Show a, HasCallStack) => [a] -> [a] -> Expectation
shouldNotContain = compareWith (\x y -> not (isInfixOf y x)) "contains"

compareWith :: (Show a, Show b, HasCallStack) => (a -> b -> Bool) -> String -> a -> b -> Expectation
compareWith f msg x y = unless (f x y) $ assertFailure $
    show x ++ " " ++ msg ++ " " ++ show y

-------------------------------------------------------------------------------
-- Writer
-------------------------------------------------------------------------------

type Writer = (,)

runWriter :: Writer w a -> (w, a)
runWriter = id

tell :: w -> (w, ())
tell w = (w, ())
