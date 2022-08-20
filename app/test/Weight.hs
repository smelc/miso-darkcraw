{-# LANGUAGE LambdaCase #-}

-- |
-- This module contains data for checking the balance. This file
-- can be updated manually, or update automatically by running
-- 'Balance.update' in a repl (see the repl section of app/README.md).
module Weight
  ( balances,
    find,
  )
where

import Card (Team (..))
import Control.Monad (join)
import Data.Function ((&))
import Data.Functor ((<&>))
import qualified Data.Map.Strict as Map
import Nat

-- | All registered balances. First 'Nat' is the number of wins of the first
-- team. Second 'Nat' is the number of wins of the second team. Third 'Nat'
-- is the number of draws.
-- Each list in the values represent a different journey.
balances :: Map.Map Team [[(Team, Nat, Nat, Nat)]]
balances =
  Map.fromList
    [ ( Human, -- journeys start
        [ [ (Undead, 34, 30, 0),
            (Beastmen, 63, 1, 0),
            (Evil, 42, 22, 0)
          ],
          [ (Undead, 34, 30, 0),
            (Sylvan, 3, 60, 1),
            (ZKnights, 33, 29, 2)
          ]
        ]
      ),
      ( Evil, -- journeys start
        [ [ (Undead, 9, 54, 1),
            (Sylvan, 3, 60, 1),
            (ZKnights, 37, 26, 1)
          ]
        ]
      )
    ]

-- | 'find t1 ts' returns the balance of the journey of 't1' when it figths
-- successively against @ts !! 0, .., ts !! (length ts - 1)@
find :: Team -> [Team] -> Maybe [(Nat, Nat, Nat)]
find t1 ts =
  Map.lookup t1 balances
    <&> (filter (\tuples -> map select tuples == ts))
    <&> safeHead
    & join
    <&> map (\(_, x, y, z) -> (x, y, z))
  where
    select (x, _, _, _) = x
    safeHead = \case [] -> Nothing; x : _ -> Just x
