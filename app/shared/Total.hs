{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- This module defines getters for getting the complete stat of
-- a creature, boosts included. Functions of this module are meant
-- to be called qualified.
-- |
module Total where

import qualified Board
import Card
import qualified Constants
import Damage (Damage (..), (+^), (-^))
import Data.Function ((&))
import qualified Data.Map.Strict as Map
import Nat
import qualified Skill
import Spots

-- | Whether a creature is affected by fear. The first Boolean indicates
-- whether to consider the hitpoints ('True') or not ('False')
affectedByFear :: Bool -> Creature 'Core -> Bool
affectedByFear _ Creature {skills} | Skill.Brainless `elem` skills = False
affectedByFear _ Creature {skills} | Skill.Veteran `elem` skills = False
affectedByFear _ Creature {skills} | Skill.Zealot `elem` skills = False
affectedByFear True Creature {hp} | hp > 1 = False
affectedByFear _ c | causesFear c = False -- Creature causing fear are immune to fear
affectedByFear _ c | causesTerror c = False -- Creature causing terror are immune to fear
affectedByFear _ _ = True

-- | Whether a creature is affected by terror. The first Boolean indicates
-- whether to consider the hitpoints ('True') or not ('False')
affectedByTerror :: Bool -> Creature 'Core -> Bool
affectedByTerror _ Creature {skills} | Skill.Brainless `elem` skills = False
affectedByTerror _ Creature {skills} | Skill.Veteran `elem` skills = False
affectedByTerror True Creature {hp} | hp > 2 = False
affectedByTerror _ c | causesTerror c = False -- Creature causing terror are immune to terror
affectedByTerror _ _ = True

-- | Where a creature is on the board
data Place = Place {place :: Board.InPlaceType 'Core, pSpot :: Spots.Player, cardSpot :: Spots.Card}

-- | Builds a 'Place' from a 'Board'
mkPlace :: Board.T 'Core -> Spots.Player -> Spots.Card -> Place
mkPlace board pSpot cardSpot = Place {place = Board.toInPlace board pSpot, pSpot, cardSpot}

-- | The total attack of a creature, including boosts of skills and items.
-- The 'Place' indicates where the given creature is.
attack :: Maybe Place -> Creature 'Core -> Damage
attack place (c@Creature {Card.attack, creatureId = id, skills, items}) =
  -- Items adding attack are dealth with here, as opposed to items
  -- adding health, which are dealth with in 'Game'
  ( attack
      <> bonus CrushingMace (Damage {base = 0, variance = 2})
      <> bonus BowOfStrength (Damage {base = 0, variance = 2})
  )
    +^ (nbBlows * Constants.blowAmount)
    +^ nbSwordsOfMight
    +^ skBannerBonus
    +^ chargeBonus
    +^ squireBonus
    +^ strengthPotBonus
    -^ slowMalus
  where
    bonus searched damage =
      let nb = filter (== searched) items & length
       in mconcat (replicate nb damage)
    nbBlows =
      filter (\case Skill.Blow True -> True; _ -> False) skills & natLength
    nbSwordsOfMight =
      filter (== SwordOfMight) items & natLength
    skBannerBonus =
      case (Card.isSkeleton id, place) of
        (True, Just Place {place}) ->
          Map.elems place
            & map Card.items -- Count the number of banners, so
            -- that you have multiple bonuses
            -- if you put the banner more than once on a creature!
            & concat
            & filter ((==) SkBanner)
            & natLength
        _ -> 0
    chargeBonus =
      -- TODO @smelc change match order
      case (hasCharge c, place) of
        (True, Just Place {place, cardSpot}) ->
          let inhabitedSpots = Map.filterWithKey (\cSpot _ -> cSpot `elem` spots) place
           in if (length inhabitedSpots == 3) && all hasCharge inhabitedSpots
                then Constants.chargeAmount
                else 0
          where
            spots = Spots.toLine cardSpot
        _ -> 0
    hasCharge Creature {skills} = any ((==) Skill.Charge) skills
    slowMalus =
      filter (\case Skill.Slow True -> True; _ -> False) skills
        & natLength
    squireBonus =
      case (Skill.Knight `elem` skills, place) of -- A knight..
        (True, Just Place {place, cardSpot})
          | Spots.inFront cardSpot -> -- .. in the frontline
              case place Map.!? (Board.switchLine cardSpot) of -- Inspect creature in frontline
                Just Creature {skills = skills'} | Skill.Squire `elem` skills' -> Constants.squireAttackBonus
                _ -> 0
        _ -> 0
    strengthPotBonus =
      filter ((==) Skill.StrengthPot) skills
        & natLength
        & ((*) Constants.strengthPotAttackBonus)

-- | The amount of @Skill.Bleed@ that a creature has
bleed :: Creature 'Core -> Nat
bleed Creature {skills} =
  foldr (\s acc -> (case s of Skill.Bleed n -> n; _ -> 0) + acc) 0 skills

-- | Whether a creature causes fear
causesFear :: Creature 'Core -> Bool
-- We ignore the skill's Boolean, because it's for UI display only
causesFear Creature {skills} =
  any (\case Skill.Fear _ -> True; Skill.FearTmp -> True; _ -> False) skills

causesTerror :: Creature 'Core -> Bool
-- We ignore the skill's Boolean, because it's for UI display only
causesTerror Creature {skills} = any (\case Skill.Terror _ -> True; _ -> False) skills

-- | Core function for finding out about discipline
hasDiscipline :: [Skill.Skill] -> [Item] -> Bool
hasDiscipline skills items =
  Skill.Discipline `elem` skills || Card.Crown `elem` items

hasRampage :: Creature 'Core -> Bool
hasRampage Creature {skills} = Skill.Rampage `elem` skills

hasPowerful :: [Skill.State] -> [Item] -> Bool
hasPowerful skills items =
  Skill.Powerful `elem` skills || Card.AxeOfRage `elem` items

-- | Whether a creature has discipline
isDisciplined :: Creature 'Core -> Bool
isDisciplined Creature {items, skills} =
  hasDiscipline (map Skill.lift skills) items

isPowerful :: Creature 'Core -> Bool
isPowerful Creature {items, skills} = hasPowerful skills items

-- | The amount of bleed that a creature causes or @0@ if it doesn't cause bleeding
bleedCaused :: Creature 'Core -> Nat
bleedCaused Creature {items} = items & filter ((==) Card.SwordOfBlood) & Nat.natLength
