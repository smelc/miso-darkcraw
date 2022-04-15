{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeApplications #-}

-- | Spin-off 'Game', to avoid 'Game' to be gigantic
module Mechanics where

import qualified Board
import Card hiding (to)
import Control.Monad.State
import Data.Function ((&))
import Data.List ((\\))
import qualified Data.Map as Map
import qualified Random
import qualified Shared
import qualified Skill
import qualified Spots

data Move = Move
  { from :: (Spots.Player, Spots.Card),
    to :: (Spots.Player, Spots.Card)
  }

-- | A move within one 'Spots.Player'
mkEndoMove :: Spots.Player -> Spots.Card -> Spots.Card -> Move
mkEndoMove pSpot from to = Move {from = (pSpot, from), to = (pSpot, to)}

-- | Whether a creature with 'Skill.Flying' in the given place can fly to.
flySpot ::
  MonadState Shared.Model m =>
  Board.InPlaceType 'Core ->
  m (Maybe Spots.Card)
flySpot place = Random.pickM $ (Spots.allCards \\ (Map.keys place))

-- | FIXME @smelc use me for FillTheFrontline
isRanged :: Creature 'Core -> Bool
isRanged Creature {skills} = (Skill.Ace `elem` skills) || (Skill.Ranged `elem` skills)

-- | @move m board@ moves the creature at @move.from m@ to @move.to m@
-- Returns 'board' unchanged if the move is impossible
move :: Move -> Board.T 'Core -> Board.T 'Core
move Move {from, to} board =
  case (at from board, at to board) of
    (Just c, Nothing) ->
      board
        & Board.update @(Creature 'Core) (fst from) (snd from) (const Nothing)
        & Board.insert (fst to) (snd to) c
    _ -> board
  where
    at (p, c) b = Board.toInPlaceCreature b p c
