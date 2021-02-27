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

-- | The total attack of a creature, including boosts of skills and items.
-- This would make more sense to be in 'Game', but alas this is more
-- convenient to have it here dependency-wise.
attack :: Creature 'Core -> Int
attack Creature {Card.attack, skills, items} =
  -- Items adding attack are dealth with here, as opposed to items
  -- adding health, which are dealth with in 'Game'
  attack + (nbBlows * Constants.blowAmount) + nbSwordsOfMight
  where
    nbBlows =
      filter (\case Blow' True -> True; _ -> False) skills & length
    nbSwordsOfMight =
      filter (== SwordOfMight) items & length

-- | Whether a creature has discipline
hasDiscipline :: Creature 'Core -> Bool
hasDiscipline Creature {items, skills} =
  Discipline' `elem` skills
    || Crown `elem` items
