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
data T blow drawCard fame fear green growth regen slow source stupid terror
  = Ace
  | -- | At turn start, moves against ponent ennemy
    Assassin
  | Blow blow
  | -- | Moves to random empty spot at turn start
    Brainless
  | BreathIce
  | Charge
  | Discipline
  | DrawCard drawCard
  | -- | Contributes to score at turn beginning
    Fame fame
  | -- | Creature causes fear
    Fear fear
  | -- | Creatures moves to random free spot upon being attacked
    Flying
  | -- | Creates a forest at turn beginning
    GreenAffinity green
  | -- | +1 hp at turn beginning
    Growth growth
  | -- | Creature attacks a random spot
    Imprecise
  | King
  | -- | Identifier of knight
    Knight
  | -- | Can attack 2 cells away
    LongReach
  | -- | Undealt damage contributes to score
    Powerful
  | Rampage
  | Ranged
  | -- | Regenerates the number of hitpoints at beginning of turn
    Regeneration regen
  | -- | Upon a kill, killed neighbors get -1 attack
    Sadism
  | -- | -1 attack the first turn
    Slow slow
  | -- | Strength potion in action
    StrengthPot
  | -- | Creature creates mana at beginning of turn
    Source source -- Nat
  | -- | Knight in front (if any) gains +1 hp and +1 attack upon arrival
    Squire
  | Stupid4 stupid
  | Support
  | -- | +1 hp/+1 attack when in a forest
    Sylvan
  | -- | Creature causes terror
    Terror terror
  | Unique
  | -- | Immune to fear and terror
    Veteran
  | -- | Immune to fear
    Zealot
  deriving (Eq, Generic, Ord, Show)

type Skill =
  T
    () -- blow
    () -- drawCard
    Nat -- fame
    () -- fear
    () -- green
    () -- growth
    Nat -- regen
    () -- slow
    Nat -- source
    () -- stupid
    () -- terror

type State =
  T
    Bool -- blow, whether the skill is available (True) or used already (False)
    Bool -- drawCard
    (Nat, Bool) -- fame, creature contribute to score at beginning of turn. Boolean
    -- indicates whether skill is available (True) or consumed already (False)
    Bool -- fear, creature causes fear, the Boolean indicates whether the skill
    -- is available (True) or used already (False)
    Bool -- green affinity, whether the skill is available (True) or consumed (False)
    Bool -- growth, whether the skill is available (True) or consumed (False)
    Nat -- regeneration, number of hitpoints gained at beginning of turn
    Bool -- slow, whether creature is slow right now (True), or not (False)
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
    Ace -> Ace
    Assassin -> Assassin
    Blow {} -> Blow ()
    Brainless -> Brainless
    BreathIce -> BreathIce
    Charge -> Charge
    Discipline -> Discipline
    DrawCard {} -> DrawCard ()
    Fame (n, _) -> Fame n
    Fear {} -> Fear ()
    Flying {} -> Flying
    GreenAffinity {} -> GreenAffinity ()
    Growth {} -> Growth ()
    Imprecise -> Imprecise
    King -> King
    Knight -> Knight
    LongReach -> LongReach
    Powerful -> Powerful
    Rampage -> Rampage
    Ranged -> Ranged
    Regeneration n -> Regeneration n
    Sadism -> Sadism
    Slow {} -> Slow ()
    Squire -> Squire
    Source (n, _) -> Source n
    StrengthPot -> StrengthPot
    Stupid4 {} -> Stupid4 ()
    Support -> Support
    Sylvan -> Sylvan
    Terror {} -> Terror ()
    Unique -> Unique
    Veteran -> Veteran
    Zealot -> Zealot

-- | Because this function uses default values, it is NOT harmless! Use only
-- when initializing data.
unlift :: Skill -> State
unlift skill =
  case skill of
    Ace -> Ace
    Assassin -> Assassin
    Blow {} -> Blow True
    Brainless -> Brainless
    BreathIce -> BreathIce
    Discipline -> Discipline
    Charge -> Charge
    DrawCard {} -> DrawCard True
    Fame n -> Fame (n, True)
    Fear {} -> Fear True
    Flying {} -> Flying
    GreenAffinity {} -> GreenAffinity True
    Growth {} -> Growth True
    Imprecise -> Imprecise
    King -> King
    Knight -> Knight
    LongReach {} -> LongReach
    Powerful -> Powerful
    Rampage -> Rampage
    Ranged -> Ranged
    Regeneration n -> Regeneration n
    Sadism -> Sadism
    Slow {} -> Slow True
    Source n -> Source (n, True)
    Squire -> Squire
    StrengthPot -> StrengthPot
    Stupid4 {} -> Stupid4 0
    Support -> Support
    Sylvan -> Sylvan
    Terror {} -> Terror True
    Unique -> Unique
    Veteran -> Veteran
    Zealot -> Zealot
