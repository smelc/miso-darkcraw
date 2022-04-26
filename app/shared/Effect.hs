{-# LANGUAGE DeriveGeneric #-}

-- | Module defining core and UI effects. Core effects affect boards
-- while UI effects affect UI boards.
module Effect where

import qualified Constants
import qualified Data.Map.Strict as Map
import GHC.Generics
import Nat
import qualified Spots
import qualified Tile

-- | Animations recorded upon death. If you are looking for the same
-- thing in the more general case of the creature not dying, loot at
-- 'fadeOut'.
data DeathCause
  = -- | Creature was killed by fear
    DeathByFear
  | -- | Creature killed by 'BreathIce' attack
    DeathByBreathIce
  | -- | Creature was killed by terror
    DeathByTerror
  | -- | Creature was not killed
    NoDeath
  | -- | Creature was killed for a reason we do not track precisely
    UsualDeath
  deriving (Eq, Generic, Show)

-- | Whether this death cause represents a death
isDead :: DeathCause -> Bool
isDead NoDeath = False
isDead _ = True

instance Semigroup DeathCause where
  DeathByTerror <> _ = DeathByTerror
  _ <> DeathByTerror = DeathByTerror
  DeathByFear <> _ = DeathByFear
  _ <> DeathByFear = DeathByFear
  DeathByBreathIce <> _ = DeathByBreathIce
  _ <> DeathByBreathIce = DeathByBreathIce
  UsualDeath <> _ = UsualDeath
  NoDeath <> d = d

instance Monoid DeathCause where
  mempty = NoDeath

-- | A change in the value of the 'deco' map
data DecoChange
  = -- | No change
    NoDecoChange
  | -- | A new 'Deco' appears
    Appears Deco
  deriving (Eq, Generic, Show)

instance Semigroup DecoChange where
  NoDecoChange <> dc = dc
  dc <> NoDecoChange = dc
  Appears d1 <> Appears d2 | d1 == d2 = Appears d1
  Appears _ <> Appears _ = NoDecoChange

instance Monoid DecoChange where
  mempty = NoDecoChange

data Deco
  = -- Usually I would have NoDeco here, but because these values are stored
    -- in maps, it creates an ambiguity w.r.t. to absence in the map.
    Forest
  deriving (Bounded, Enum, Eq, Generic, Show)

instance Semigroup Deco where
  Forest <> Forest = Forest

-- It is a bit unfortunate to have these types defined here
-- as they are UI only. However we need them to define the InPlaceType family

-- | Initially this type was for displaying animations only. However
-- Game.hs also uses for Core stuff internally (see applyInPlaceEffectOnBoard).
-- Unfortunate :-( So be careful when changing related code.
data InPlaceEffect = InPlaceEffect
  { -- | Attack value changed
    attackChange :: Int,
    -- | Whether a 'Deco' changes
    decoChange :: DecoChange,
    -- | Did creature die? If yes, for what reason
    death :: DeathCause,
    -- | Creature attacked (value used solely for animations)
    attackBump :: Bool,
    -- | Hits points changed
    hitPointsChange :: Int,
    -- | Hit points that were not dealt because the defender died. Contributes
    -- to the score with 'Skill.Powerful'.
    extra :: Nat,
    -- | Card fade-in or fade-out
    fade :: Constants.Fade,
    -- | Tiles to fade out atop the card spot, both when there's a creature
    -- and when there's not.
    fadeOut :: [Tile.Tile],
    -- | Score changed
    scoreChange :: Int
  }
  deriving (Eq, Generic, Show)

instance Semigroup InPlaceEffect where
  InPlaceEffect {attackChange = ac1, death = d1, decoChange = dc1, attackBump = ab1, extra = e1, hitPointsChange = hp1, fade = fi1, fadeOut = fo1, scoreChange = c1}
    <> InPlaceEffect {attackChange = ac2, death = d2, decoChange = dc2, attackBump = ab2, extra = e2, hitPointsChange = hp2, fade = fi2, fadeOut = fo2, scoreChange = c2} =
      InPlaceEffect
        { attackChange = ac1 + ac2,
          death = d1 <> d2,
          decoChange = dc1 <> dc2,
          attackBump = ab1 || ab2,
          extra = e1 + e2,
          hitPointsChange = hp1 + hp2,
          fade = fi1 <> fi2,
          fadeOut = fo1 ++ fo2,
          scoreChange = c1 + c2
        }

instance Monoid InPlaceEffect where
  mempty =
    InPlaceEffect
      { attackChange = 0,
        death = mempty,
        decoChange = mempty,
        attackBump = False,
        extra = 0,
        hitPointsChange = 0,
        fade = mempty,
        fadeOut = [],
        scoreChange = 0
      }

type InPlaceEffects = Map.Map Spots.Card InPlaceEffect
