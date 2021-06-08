-- |
-- This module defines helpers for the tests
-- |
module TestLib (shouldAllSatisfy) where

import Test.Hspec

shouldAllSatisfy :: Show a => [a] -> (a -> Bool) -> Expectation
shouldAllSatisfy [] _ = pure ()
shouldAllSatisfy (a : rest) f | f a = shouldAllSatisfy rest f
shouldAllSatisfy (a : _) _ = expectationFailure (show a ++ " doesn't satisfy the predicate")
