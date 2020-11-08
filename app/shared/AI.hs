{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- This module defines how the AI plays
-- |
module AI (aiPlay, placeCards) where

import Board
import Card
import Control.Exception
import Control.Lens hiding (snoc)
import Data.Either
import Data.Either.Extra
import Data.List
import qualified Data.Map.Strict as Map
import Data.Maybe
import qualified Data.Text as Text
import Debug.Trace (trace)
import Game
import Turn (Turn, turnToPlayerSpot)

placeCards ::
  Board Core ->
  Turn ->
  [GamePlayEvent]
placeCards board turn =
  -- Will fail once when we do more stuff in aiPlay. It's OK, I'll
  -- adapt when this happens.
  assert (all isPlaceEvent events) events
  where
    events = aiPlay board turn
    isPlaceEvent EndTurn {} = False
    isPlaceEvent NoPlayEvent = False
    isPlaceEvent Place {} = True

-- | Smart play events
aiPlay :: Board Core -> Turn -> [GamePlayEvent]
aiPlay board turn =
  case scores of
    [] -> []
    (_, events) : _ -> events
  where
    pSpot = turnToPlayerSpot turn
    hands :: [[(Int, Card Core)]] = permutations $ zip [0 ..] $ boardToHand board pSpot
    -- The map contains the translation of hand indices in [GamePlayEvent]
    -- that makes sense for the initial Board, i.e. 'board'
    possibles :: [(Map.Map Int Int, [GamePlayEvent])] =
      map
        ( \hand ->
            ( Map.fromList $ zip [0 ..] $ map fst hand,
              aiPlayHand (boardSetHand board pSpot (map snd hand)) turn
            )
        )
        hands
    subst map (Place pSpot cSpot (HandIndex i)) =
      Place pSpot cSpot $ HandIndex $ map Map.! i
    subst _ gpe = assert False gpe
    -- Apply substitution, make [GamePlayEvent] meaningful for 'board'
    possibles' = map (\(m, events) -> map (subst m) events) possibles
    scores :: [(Int, [GamePlayEvent])] =
      map
        ( \events ->
            case playAll board events of
              Left msg -> trace ("Unexpected case: " ++ Text.unpack msg) Nothing
              Right board' -> Just (boardScore board' turn, events)
        )
        possibles'
        & catMaybes
        & sortByFst
    playAll b events = Game.playAll b events <&> fst

-- | The score of the given player's in-place cards. 0 is the best.
boardScore :: Board Core -> Turn -> Int
boardScore board turn =
  sum scores
  where
    pSpot = turnToPlayerSpot turn
    cSpotAndMayCreatures =
      boardToHoleyInPlace board & filter (\(pSpot', _, _) -> pSpot == pSpot')
        & map (\(_, cSpot, mCreature) -> (cSpot, mCreature))
    scores =
      map
        ( \(cSpot, mCreature) ->
            case mCreature of
              Nothing -> 5 -- Empty spot: malus
              Just _ ->
                let score = scorePlace board pSpot cSpot
                 in assert (isJust score) (fromJust score)
        )
        cSpotAndMayCreatures

-- | Events for playing all cards of the hand, in order. Each card
-- is placed at an optimal position.
aiPlayHand :: Board Core -> Turn -> [GamePlayEvent]
aiPlayHand board turn =
  case aiPlayFirst board turn of
    Nothing -> []
    Just place ->
      case Game.play board place of
        Left msg -> error $ Text.unpack msg -- We shouldn't generate invalid Place actions
        Right (b', _) ->
          place : aiPlayHand b' turn

-- | Take the hand's first card (if any) and return a [Place] event
-- for best placing this card.
aiPlayFirst :: Board Core -> Turn -> Maybe GamePlayEvent
aiPlayFirst board turn =
  case boardToHand board pSpot of
    [] -> Nothing
    _ -> do
      let scores' = scores & sortByFst
      best <- listToMaybe scores'
      return $ Place pSpot (snd best) handIndex
  where
    handIndex = HandIndex 0
    pSpot = turnToPlayerSpot turn
    possibles = placements board pSpot
    scores =
      [ (scorePlace (fromRight' board' & fst) pSpot cSpot, cSpot)
        | cSpot <- possibles,
          let place = Place pSpot cSpot handIndex,
          let board' = Game.play board place,
          isRight board'
      ]

placements ::
  Board Core ->
  -- | The player placing a card
  PlayerSpot ->
  -- | All spots where the card can be put
  [CardSpot]
placements board pSpot =
  [cSpot | cSpot <- allCardsSpots, cSpot `Map.notMember` inPlace]
  where
    pLens = spotToLens pSpot
    inPlace :: Map.Map CardSpot (Creature Core) = board ^. pLens . #inPlace

-- | The score of placing a card at the given position
scorePlace ::
  Board Core ->
  PlayerSpot ->
  CardSpot ->
  -- | The score of the card at pSpot cSpot. 0 is the best.
  Maybe Int
scorePlace board pSpot cSpot =
  case inPlace of
    Just _ -> Just $ sum maluses
    _ -> Nothing
  where
    inPlace :: Maybe (Creature Core) = boardToInPlaceCreature board pSpot cSpot
    enemiesInPlace :: Map.Map CardSpot (Creature Core) =
      boardToInPlace board (otherPlayerSpot pSpot)
    cSkills :: [Skill] = inPlace >>= skills & fromMaybe []
    prefersBack = Ranged `elem` cSkills || LongReach `elem` cSkills
    lineMalus = if inTheBack cSpot == prefersBack then 0 else 1
    enemySpots' :: [CardSpot] = allEnemySpots cSpot
    enemiesInColumn = map (enemiesInPlace Map.!?) enemySpots'
    -- TODO @smelc instead, play EndTurn pSpot cSpot and look at the score
    -- increase. This will avoid doing an incorrect simulation.
    yieldsVictoryPoints = all isNothing enemiesInColumn
    victoryPointsMalus = if yieldsVictoryPoints then 0 else 1
    maluses = [lineMalus, victoryPointsMalus]

sortByFst l =
  sortBy sortFst l
  where
    sortFst (i, _) (j, _) = compare i j
