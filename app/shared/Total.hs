{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}

-- |
-- This module defines getters for getting the complete stat of
-- a creature, boosts included. Functions of this module are meant
-- to be called qualified.
-- |
module Total where

import Card
import qualified Constants
import Data.Function ((&))
import Nat
import qualified Skill

-- | Whether a creature is affected by fear. The first Boolean indicates
-- whether to consider the hitpoints ('True') or not ('False')
affectedByFear :: Bool -> Creature 'Core -> Bool
affectedByFear True Creature {hp} | hp > 1 = False
affectedByFear _ c | causesFear c = False -- Creature causing fear are immune to fear
affectedByFear _ c | causesTerror c = False -- Creature causing terror are immune to fear
affectedByFear _ _ = True

-- | Whether a creature is affected by terror. The first Boolean indicates
-- whether to consider the hitpoints ('True') or not ('False')
affectedByTerror :: Bool -> Creature 'Core -> Bool
affectedByTerror True Creature {hp} | hp > 2 = False
affectedByTerror _ c | causesTerror c = False -- Creature causing terror are immune to terror
affectedByTerror _ _ = True

type Part = [Creature 'Core]

-- | The total attack of a creature, including boosts of skills and items.
-- The first list is the part where the creature is (if applicable)
attack :: Maybe Part -> Creature 'Core -> Nat
attack part (Creature {Card.attack, creatureId = id, skills, items}) =
  -- Items adding attack are dealth with here, as opposed to items
  -- adding health, which are dealth with in 'Game'
  attack + (nbBlows * Constants.blowAmount) + nbSwordsOfMight + skBannerBonus
  where
    nbBlows =
      filter (\case Skill.Blow' True -> True; _ -> False) skills & natLength
    nbSwordsOfMight =
      filter (== SwordOfMight) items & natLength
    skBannerBonus =
      case (Card.isSkeleton id, part) of
        (False, _) -> 0
        (_, Nothing) -> 0
        (True, Just part) ->
          map Card.items part -- Count the number of banners, so
          -- that you have multiple bonuses
          -- if you put the banner more than once on a creature!
            & concat
            & filter ((==) SkBanner)
            & natLength

-- | Whether a creature causes fear
causesFear ::
  Creature 'Core ->
  Bool
-- We ignore the skill's Boolean, because it's for UI display only
causesFear Creature {skills} = any (\case Skill.Fear' _ -> True; _ -> False) skills

causesTerror ::
  Creature 'Core ->
  Bool
-- We ignore the skill's Boolean, because it's for UI display only
causesTerror Creature {skills} = any (\case Skill.Terror' _ -> True; _ -> False) skills

-- | Core function for finding out about discipline
hasDiscipline :: [Skill.Skill] -> [Item] -> Bool
hasDiscipline skills items =
  Skill.Discipline `elem` skills || Crown `elem` items

-- | Whether a creature has discipline
isDisciplined :: Creature 'Core -> Bool
isDisciplined Creature {items, skills} =
  hasDiscipline (map Skill.lift skills) items
