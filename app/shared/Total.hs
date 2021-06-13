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

-- | The total attack of a creature, including boosts of skills and items.
-- This would make more sense to be in 'Game', but alas this is more
-- convenient to have it here dependency-wise.
attack :: Creature 'Core -> Nat
attack Creature {Card.attack, skills, items} =
  -- Items adding attack are dealth with here, as opposed to items
  -- adding health, which are dealth with in 'Game'
  attack + (nbBlows * Constants.blowAmount) + nbSwordsOfMight
  where
    nbBlows =
      filter (\case Blow' True -> True; _ -> False) skills & natLength
    nbSwordsOfMight =
      filter (== SwordOfMight) items & natLength

-- | Whether a creature causes fear
causesFear ::
  Creature 'Core ->
  Bool
-- We ignore the skill's Boolean, because it's for UI display only
causesFear Creature {skills} = any (\case Fear' _ -> True; _ -> False) skills

causesTerror ::
  Creature 'Core ->
  Bool
-- We ignore the skill's Boolean, because it's for UI display only
causesTerror Creature {skills} = any (\case Terror' _ -> True; _ -> False) skills

-- | Core function for finding out about discipline
hasDiscipline :: [Skill] -> [Item] -> Bool
hasDiscipline skills items =
  Discipline `elem` skills || Crown `elem` items

-- | Whether a creature has discipline
isDisciplined :: Creature 'Core -> Bool
isDisciplined Creature {items, skills} =
  hasDiscipline (map liftSkill skills) items
