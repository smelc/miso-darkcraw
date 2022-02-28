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
    -- Endomatches: if starting team doesn't have an advantage this data should
    -- show roughly 50% win on each side. It doesn't seem to be the case now.
    (Human, Campaign.Level0, Human, 48, 15, 1),
    (Evil, Campaign.Level0, Evil, 48, 16, 0),
    (Undead, Campaign.Level0, Undead, 44, 17, 3),
    -- Matchups
    (Human, Campaign.Level0, Evil, 15, 48, 1),
    (Human, Campaign.Level0, Undead, 13, 49, 2),
    (Human, Campaign.Level0, ZKnights, 21, 42, 1),
    (Evil, Campaign.Level0, Undead, 22, 39, 3),
    -- Level1
    (Human, Campaign.Level1, Evil, 42, 148, 2),
    (Human, Campaign.Level1, Undead, 40, 342, 2),
    (Evil, Campaign.Level1, Undead, 31, 95, 2)
  ]

-- | Returns the balance of the given matchup
find :: Team -> Campaign.Level -> Team -> Maybe (Nat, Nat, Nat)
find t1 level t2 =
  Data.List.find (\(t, l, t', _, _, _) -> t == t1 && l == level && t' == t2) balances
    <&> (\(_, _, _, topWins, botWins, draws) -> (topWins, botWins, draws))
