{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE RecordWildCards #-}
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
import Game hiding (Result)
import qualified Game
import SharedModel (SharedModel)
import qualified SharedModel
import Turn (Turn, turnToPlayerSpot)

placeCards ::
  SharedModel ->
  Board Core ->
  Turn ->
  [GamePlayEvent]
placeCards shared board turn =
  -- Will fail once when we do more stuff in aiPlay. It's OK, I'll
  -- adapt when this happens.
  assert (all isPlaceEvent events) events
  where
    events = aiPlay shared board turn
    isPlaceEvent EndTurn {} = False
    isPlaceEvent NoPlayEvent = False
    isPlaceEvent Place {} = True
    isPlaceEvent Place' {} = True

-- | Smart play events
aiPlay :: SharedModel -> Board Core -> Turn -> [GamePlayEvent]
aiPlay shared board turn =
  case scores of
    [] -> []
    (_, events) : _ -> events
  where
    pSpot = turnToPlayerSpot turn
    hands =
      boardToHand board pSpot
        & map (unliftCard . SharedModel.unsafeIdentToCard shared)
        & sortOn scoreCard
        & map cardToIdentifier
        & take 5 -- To keep complexity low for big hands
        & permutations -- Try cards in all orders (because aiPlayHand plays first card)
    possibles =
      map
        (\hand -> aiPlayHand shared (boardSetHand board pSpot hand) turn)
        hands
    scores :: [(Int, [GamePlayEvent])] =
      map
        ( \events ->
            case Game.playAll shared board events of
              Left msg -> trace ("Unexpected case: " ++ Text.unpack msg) Nothing
              Right (Game.Result board' _) -> Just (boardScore board' turn, events)
        )
        possibles
        & catMaybes
        & sortByFst

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
aiPlayHand :: SharedModel -> Board Core -> Turn -> [GamePlayEvent]
aiPlayHand shared board turn =
  case aiPlayFirst shared board turn of
    Nothing -> []
    Just place ->
      case Game.play shared board place of
        Left msg -> error $ Text.unpack msg -- We shouldn't generate invalid Place actions
        Right (Game.Result b' _) ->
          place : aiPlayHand shared b' turn

-- | Take the hand's first card (if any) and return a [Place] event
-- for best placing this card.
aiPlayFirst :: SharedModel -> Board Core -> Turn -> Maybe GamePlayEvent
aiPlayFirst shared board turn =
  case boardToHand board pSpot of
    [] -> Nothing
    IDC creatureID : _ -> do
      let scores' = scores & map liftFstMaybe & catMaybes & sortByFst
      best <- listToMaybe scores'
      return $ Place' (CardTarget pSpot (snd best)) creatureID
    i : _ -> error $ "Unsupported identifier: " ++ show i
  where
    handIndex = HandIndex 0
    pSpot = turnToPlayerSpot turn
    possibles = placements board pSpot
    scores =
      [ (scorePlace (fromRight' board' & takeBoard) pSpot cSpot, cSpot)
        | cSpot <- possibles,
          let place = Place (CardTarget pSpot cSpot) handIndex,
          let board' = Game.play shared board place,
          isRight board'
      ]
    liftFstMaybe (Nothing, _) = Nothing
    liftFstMaybe (Just i, a) = Just (i, a)
    takeBoard (Game.Result b _) = b

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
    cSkills = inPlace <&> skills & fromMaybe []
    prefersBack = Ranged `elem` cSkills || LongReach `elem` cSkills
    lineMalus = if inTheBack cSpot == prefersBack then 0 else 1
    enemySpots' :: [CardSpot] = allEnemySpots cSpot
    enemiesInColumn = map (enemiesInPlace Map.!?) enemySpots'
    -- TODO @smelc instead, play EndTurn pSpot cSpot and look at the score
    -- increase. This will avoid doing an incorrect simulation.
    yieldsVictoryPoints = all isNothing enemiesInColumn
    victoryPointsMalus = if yieldsVictoryPoints then 0 else 1
    maluses = [lineMalus, victoryPointsMalus]

-- | The score of a card. Most powerful cards return a smaller value.
-- Negative values returned.
scoreCard :: Card Core -> Int
scoreCard = \case
  CreatureCard Creature {..} ->
    sum $ [- hp, - attack, - (fromMaybe 0 moral)] ++ map scoreSkill skills
  NeutralCard _ -> error "NeutralCard unsupported"
  ItemCard _ -> error "ItemCard unsupported"

-- | The score of a skill, smaller values are better. Negative values returned.
scoreSkill :: Skill -> Int
scoreSkill = \case
  LongReach -> -1
  Leader -> -1
  Ranged -> -1
  Stubborn -> -1
  Unique -> -1

sortByFst :: [(Int, b)] -> [(Int, b)]
sortByFst l =
  sortBy sortFst l
  where
    sortFst (i, _) (j, _) = compare i j
