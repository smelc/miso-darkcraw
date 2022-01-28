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
    (Human, Campaign.Level0, Human, 45, 17, 2),
    (Evil, Campaign.Level0, Evil, 34, 28, 2),
    (Undead, Campaign.Level0, Undead, 42, 19, 3),
    -- Matchups
    (Human, Campaign.Level0, Evil, 62, 2, 0),
    (Human, Campaign.Level0, Undead, 61, 2, 1),
    (Human, Campaign.Level0, ZKnights, 48, 15, 1),
    (Evil, Campaign.Level0, Undead, 35, 27, 2),
    -- Level1
    (Human, Campaign.Level1, Evil, 174, 17, 1),
    (Human, Campaign.Level1, Undead, 321, 60, 3),
    (Evil, Campaign.Level1, Undead, 66, 61, 1)
  ]

-- | Returns the balance of the given matchup
find :: Team -> Campaign.Level -> Team -> Maybe (Nat, Nat, Nat)
find t1 level t2 =
  Data.List.find (\(t, l, t', _, _, _) -> t == t1 && l == level && t' == t2) balances
    <&> (\(_, _, _, topWins, botWins, draws) -> (topWins, botWins, draws))
