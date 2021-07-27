-- |
-- This module defines helpers for the tests
-- |
module TestLib (shouldAllSatisfy, shouldSatisfyJust, shouldSatisfyRight) where

import Test.Hspec

-- | @shouldAllSatisfy l f@ succeeds if @f x@ for all members @x@ of 'l'
shouldAllSatisfy :: Show a => [a] -> (a -> Bool) -> Expectation
shouldAllSatisfy [] _ = pure ()
shouldAllSatisfy (a : rest) f | f a = shouldAllSatisfy rest f
shouldAllSatisfy (a : _) _ = expectationFailure (show a ++ " doesn't satisfy the predicate")

-- | @shouldSatisfyJust Nothing _@ fails. @shouldSatisfyJust (Just x) f@
-- succeeds if @f x@ holds.
shouldSatisfyJust :: Show a => Maybe a -> (a -> Bool) -> Expectation
shouldSatisfyJust Nothing _ = expectationFailure "Expected Just, got Nothing"
shouldSatisfyJust (Just x) f | f x = pure ()
shouldSatisfyJust (Just x) _ = expectationFailure (show x ++ " doesn't satisfy the predicate")

-- | @shouldSatisfyRight (Left _) _@ fails. @shouldSatisfyRight (Right x) f@
-- succeeds if @f x@ holds.
shouldSatisfyRight :: Show b => Either a b -> (b -> Bool) -> Expectation
shouldSatisfyRight (Left _) _ = expectationFailure "Expected Right, got Left"
shouldSatisfyRight (Right x) f | f x = pure ()
shouldSatisfyRight (Right x) _ = expectationFailure (show x ++ " doesn't satisfy the predicate")
