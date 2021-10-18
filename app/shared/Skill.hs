{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}

-- |
-- A spinoff of 'Card'
-- |
module Skill where

import GHC.Generics (Generic)
import Nat

-- | The type of skills. Highly polymorphic, because we use
-- two different instances. Callers should only use
-- the concrete instances 'Skill' and 'State'.
data T blow drawCard fear source stupid terror
  = Blow blow
  | BreathIce
  | Charge
  | Discipline
  | DrawCard drawCard
  | -- | Creature causes fear
    Fear fear
  | King
  | -- | Identifier of knight
    Knight
  | LongReach
  | Ranged
  | -- | Creature creates mana at beginning of turn
    Source source -- Nat
  | -- | Knight in front (if any) gains +1 hp and +1 attack upon arrival
    Squire
  | Stupid4 stupid
  | -- | Creature causes terror
    Terror terror
  | Unique
  | -- | Upon arrival, every knight gains +1 hp and +1 attack
    Veteran
  | -- | Immune to fear
    Zealot
  deriving (Eq, Generic, Ord, Show)

type Skill =
  T
    () -- blow
    () -- drawCard
    () -- fear
    Nat -- source
    () -- stupid
    () -- terror

type State =
  T
    Bool -- blow, whether the skill is available (True) or used already (False)
    Bool -- drawCard
    Bool -- fear, creature causes fear, the Boolean indicates whether the skill
    -- is available (True) or used already (False)
    (Nat, Bool) -- source, creature create mana at beginning of turn. Boolean indicates
    -- whether the skill is available (True) or used already (False)
    Nat -- stupid, the turn, at 0, 1, or 2 not stupid; at 3 stupid; then back to 0
    Bool -- terror, creature causes terror, the Boolean indicates whether the skill
    -- is available (True) or used already (False)

isStupid :: State -> Bool
isStupid = \case Stupid4 3 -> True; _ -> False

data Pack = Pack
  { skill :: Skill,
    text :: String,
    title :: String
  }
  deriving (Eq, Generic, Show)

lift :: State -> Skill
lift skill =
  case skill of
    Blow {} -> Blow ()
    BreathIce -> BreathIce
    Charge -> Charge
    Discipline -> Discipline
    DrawCard {} -> DrawCard ()
    Fear {} -> Fear ()
    King -> King
    Knight -> Knight
    LongReach -> LongReach
    Ranged -> Ranged
    Squire -> Squire
    Source (n, _) -> Source n
    Stupid4 {} -> Stupid4 ()
    Terror {} -> Terror ()
    Unique -> Unique
    Veteran -> Veteran
    Zealot -> Zealot

-- | Because this function uses default values, it is NOT harmless! Use only
-- when initializing data.
unlift :: Skill -> State
unlift skill =
  case skill of
    Blow {} -> Blow True
    BreathIce -> BreathIce
    Discipline -> Discipline
    Charge -> Charge
    DrawCard {} -> DrawCard True
    Fear {} -> Fear True
    King -> King
    Knight -> Knight
    LongReach {} -> LongReach
    Ranged -> Ranged
    Source n -> Source (n, True)
    Squire -> Squire
    Stupid4 {} -> Stupid4 0
    Terror {} -> Terror True
    Unique -> Unique
    Veteran -> Veteran
    Zealot -> Zealot
