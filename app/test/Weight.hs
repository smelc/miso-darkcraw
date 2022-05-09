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
    (Beastmen, Campaign.Level0, Beastmen, 57, 2, 5),
    (Human, Campaign.Level0, Human, 47, 13, 4),
    (Evil, Campaign.Level0, Evil, 43, 20, 1),
    (Sylvan, Campaign.Level0, Sylvan, 40, 23, 1),
    (Undead, Campaign.Level0, Undead, 42, 18, 4),
    -- Matchups
    (Human, Campaign.Level0, Beastmen, 61, 3, 0),
    (Human, Campaign.Level0, Evil, 18, 45, 1),
    (Human, Campaign.Level0, Sylvan, 6, 57, 1),
    (Human, Campaign.Level0, Undead, 27, 36, 1),
    (Human, Campaign.Level0, ZKnights, 40, 23, 1),
    (Evil, Campaign.Level0, Undead, 38, 26, 0),
    -- Level1
    (Human, Campaign.Level1, Evil, 38, 153, 1),
    (Human, Campaign.Level1, Undead, 85, 295, 4),
    (Evil, Campaign.Level1, Sylvan, 8, 178, 6),
    (Evil, Campaign.Level1, Undead, 41, 83, 4)
  ]

-- | Returns the balance of the given matchup
find :: Team -> Campaign.Level -> Team -> Maybe (Nat, Nat, Nat)
find t1 level t2 =
  Data.List.find (\(t, l, t', _, _, _) -> t == t1 && l == level && t' == t2) balances
    <&> (\(_, _, _, topWins, botWins, draws) -> (topWins, botWins, draws))
