{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}

-- | A spinoff of 'Card'
module Skill where

import GHC.Generics (Generic)
import Nat

data Skill
  = Blow
  | BreathIce
  | Discipline
  | DrawCard
  | -- | Creature causes fear
    Fear
  | LongReach
  | Ranged
  | -- | Creature creates mana at beginning of turn
    Source Nat
  | Stupid4
  | -- | Creature causes terror
    Terror
  | Unique
  deriving (Eq, Generic, Ord, Show)

data SkillCore
  = -- | Whether the skill is available (True) or used already (False)
    Blow' Bool
  | BreathIce'
  | Discipline'
  | -- | Whether the skill is available (True) or used already (False)
    DrawCard' Bool
  | -- | Creature causes fear, the Boolean indicates whether the skill
    -- is available (True) or used already (False)
    Fear' Bool
  | LongReach'
  | Ranged'
  | -- | Creature create mana at beginning of turn. Boolean indicates
    -- whether the skill is available (True) or used already (False)
    Source' Nat Bool
  | -- | The turn, at 0, 1, or 2 not stupide; at 3 stupid; then back to 0
    Stupid4' Int
  | -- | Creature causes terror, the Boolean indicates whether the skill
    -- is available (True) or used already (False)
    Terror' Bool
  | Unique'
  deriving (Eq, Generic, Ord, Show)

isStupid :: SkillCore -> Bool
isStupid = \case Stupid4' 3 -> True; _ -> False

data Pack = Pack
  { skill :: Skill,
    text :: String,
    title :: String
  }
  deriving (Eq, Generic, Show)

lift :: SkillCore -> Skill
lift skill =
  case skill of
    Blow' _ -> Blow
    BreathIce' -> BreathIce
    Discipline' -> Discipline
    DrawCard' _ -> DrawCard
    Fear' _ -> Fear
    LongReach' -> LongReach
    Ranged' -> Ranged
    Source' n _ -> Source n
    Stupid4' _ -> Stupid4
    Terror' _ -> Terror
    Unique' -> Unique

-- | Because this function uses default values, it is NOT harmless! Use only
-- when initializing data.
unlift :: Skill -> SkillCore
unlift skill =
  case skill of
    Blow -> Blow' True
    BreathIce -> BreathIce'
    Discipline -> Discipline'
    DrawCard -> DrawCard' True
    Fear -> Fear' True
    LongReach -> LongReach'
    Ranged -> Ranged'
    Source n -> Source' n True
    Stupid4 -> Stupid4' 0
    Terror -> Terror' True
    Unique -> Unique'
