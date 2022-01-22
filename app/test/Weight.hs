-- |
-- This module contains data for checking the balance. This file
-- can be updated manually, or update automatically by running
-- 'Balance.update' in a repl (see the repl section of app/README.md).
module Weight
  ( balances,
    find,
  )
where

import qualified Campaign
import Card (Team (..))
import Data.Functor ((<&>))
import qualified Data.List
import Nat

-- | All registered balances. First 'Nat' is the number of wins of the first
-- team. Second 'Nat' is the number of wins of the second team. Third 'Nat'
-- is the number of draws.
balances :: [(Team, Campaign.Level, Team, Nat, Nat, Nat)]
balances =
  [ -- Level0
    (Human, Campaign.Level0, Evil, 44, 18, 2),
    (Human, Campaign.Level0, Undead, 47, 17, 0),
    (Human, Campaign.Level0, ZKnights, 27, 33, 4),
    (Evil, Campaign.Level0, Undead, 23, 39, 2),
    -- Level1
    (Human, Campaign.Level1, Evil, 147, 39, 6),
    (Human, Campaign.Level1, Undead, 290, 88, 6),
    (Evil, Campaign.Level1, Undead, 35, 90, 3)
  ]

-- | Returns the balance of the given matchup
find :: Team -> Campaign.Level -> Team -> Maybe (Nat, Nat, Nat)
find t1 level t2 =
  Data.List.find (\(t, l, t', _, _, _) -> t == t1 && l == level && t' == t2) balances
    <&> (\(_, _, _, topWins, botWins, draws) -> (topWins, botWins, draws))
