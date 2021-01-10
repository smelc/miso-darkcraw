{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- This module defines how the AI plays
-- |
-- boardScore is exported for tests
module AI (AI.play, boardScore, placeCards) where

import Board
import BoardInstances ()
import Card
import Control.Exception
import Control.Lens hiding (snoc)
import Data.Either
import Data.Either.Extra
import Data.List
import qualified Data.Map.Strict as Map
import Data.Maybe
import qualified Data.Text as Text
import Debug.Trace (trace, traceShow)
import Game hiding (Event, Result)
import qualified Game
import SharedModel (SharedModel)
import qualified SharedModel
import Turn (Turn, turnToPlayerSpot)

-- | Events that place creatures on the board. This function guarantees
-- that the returned events are solely placements (no neutral cards), so
-- that playing them with 'Game.playAll' returns an empty list of 'Game.Event'
placeCards ::
  SharedModel ->
  Board Core ->
  Turn ->
  [Game.Event]
placeCards shared board turn =
  -- Will fail once when we do more stuff in aiPlay. It's OK, I'll
  -- adapt when this happens.
  assert (all isPlaceEvent events) events
  where
    events = AI.play shared board turn
    isPlaceEvent Attack {} = False
    isPlaceEvent NoPlayEvent = False
    isPlaceEvent Place {} = True
    isPlaceEvent Place' {} = True

-- | Smart play events
play :: SharedModel -> Board Core -> Turn -> [Game.Event]
play shared board turn =
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
        (\hand -> playHand shared (boardSetHand board pSpot hand) turn)
        hands
    scores :: [(Int, [Game.Event])] =
      map
        ( \events ->
            case Game.playAll shared board events of
              Left msg -> trace ("Maybe unexpected? " ++ Text.unpack msg) Nothing
              Right (Game.Result board' () _) -> Just (boardScore board' pSpot, events)
        )
        possibles
        & catMaybes
        & sortByFst

-- | The score of a board, from the POV of the given player. Smaller is
-- the best
boardScore :: Board Core -> PlayerSpot -> Int
boardScore board pSpot =
  -- This is wrong (breaks testAIRanged), I don't get why.
  -- playerScore (otherPlayerSpot pSpot) - playerScore pSpot
  -- where
  --   playerScore pSpot =
  --     boardPlayerScore board pSpot + (boardToPart board pSpot & score)
  boardPlayerScore board pSpot

-- | The score of given player's in-place cards. 0 is the best.
boardPlayerScore :: Board Core -> PlayerSpot -> Int
boardPlayerScore board pSpot =
  sum scores
  where
    cSpotAndMayCreatures = boardToPlayerHoleyInPlace board pSpot
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
-- is played optimally.
playHand :: SharedModel -> Board Core -> Turn -> [Game.Event]
playHand shared board turn =
  case aiPlayFirst shared board turn of
    Nothing -> []
    Just event ->
      case Game.playAll shared board [event] of
        Left msg ->
          traceShow ("Cannot play first card of hand: " ++ Text.unpack msg) $
            playHand shared board' turn
          where
            pSpot = turnToPlayerSpot turn
            hand' = boardToHand board pSpot & tail
            board' = boardSetHand board pSpot hand' -- Skip first card
        Right (Game.Result b' () _) ->
          event : playHand shared b' turn

-- | Take the hand's first card (if any) and return a [Place] event
-- for best placing this card.
aiPlayFirst :: SharedModel -> Board Core -> Turn -> Maybe Game.Event
aiPlayFirst shared board turn =
  case boardToHand board pSpot of
    [] -> Nothing
    i@(IDI _) : _ -> error $ "Unsupported identifier: " ++ show i
    id : _ -> do
      let scores' = scores id & sortByFst
      best <- listToMaybe scores'
      return $ Place' (snd best) id
  where
    handIndex = HandIndex 0
    pSpot = turnToPlayerSpot turn
    possibles id = targets board pSpot id
    scores id =
      [ (boardScore (fromRight' board' & takeBoard) pSpot, target)
        | target <- possibles id,
          let board' = Game.play shared board $ Place target handIndex,
          isRight board'
      ]
    takeBoard (Game.Result b _ _) = b

targets ::
  Board Core ->
  -- | The player placing a card
  PlayerSpot ->
  -- | The card being played
  Card.ID ->
  -- | All spots where the card can be put
  [Target]
targets board playingPlayer id =
  case id of
    IDC _ ->
      -- Creatures can be placed in the playing player's free spots:
      cardTargets playingPlayer Hole
    IDN n ->
      case (Card.targetType n, neutralPlayerTargets n) of
        (CardTargetType ctk, Playing) ->
          cardTargets playingPlayer ctk
        (CardTargetType ctk, Opponent) ->
          cardTargets (otherPlayerSpot playingPlayer) ctk
        (PlayerTargetType, Playing) ->
          [PlayerTarget playingPlayer]
        (PlayerTargetType, Opponent) ->
          [PlayerTarget $ otherPlayerSpot playingPlayer]
    _ -> error $ "Unsupported Card.ID: " ++ show id
  where
    cardTargets pSpot ctk =
      boardToPlayerCardSpots board pSpot ctk & map (CardTarget pSpot)

-- | Whether the AI tries to play a neutral card on the playing player
-- or the  opponent. We could even try both, but we don't do that.
data NeutralPlayerTarget = Playing | Opponent

neutralPlayerTargets :: Neutral -> NeutralPlayerTarget
neutralPlayerTargets = \case
  Health -> Playing
  InfernalHaste -> Playing
  Life -> Playing

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
    prefersBack = Ranged' `elem` cSkills || LongReach' `elem` cSkills
    lineMalus = if inTheBack cSpot == prefersBack then 0 else 1
    enemySpots' :: [CardSpot] = allEnemySpots cSpot
    enemiesInColumn = map (enemiesInPlace Map.!?) enemySpots'
    -- TODO @smelc instead, play EndTurn pSpot cSpot and look at the score
    -- increase. This will avoid doing an incorrect simulation.
    yieldsVictoryPoints = all isNothing enemiesInColumn
    victoryPointsMalus = if yieldsVictoryPoints then 0 else 1
    maluses = [lineMalus, victoryPointsMalus]

-- | The score of a card. Most powerful cards return a smaller value.
-- Negative values returned. FIXME @smelc Return positive values
scoreCard :: Card Core -> Int
scoreCard = \case
  CreatureCard Creature {..} ->
    sum $ [- hp, - attack, - (fromMaybe 0 moral)] ++ map scoreSkill skills
  NeutralCard NeutralObject {neutral} ->
    case neutral of
      Health -> -1
      InfernalHaste -> -10
      Life -> -3
  ItemCard _ -> error "ItemCard unsupported"

-- | The score of a skill, smaller values are better. Negative values returned.
scoreSkill :: SkillCore -> Int
scoreSkill s =
  case s of
    Blow' b -> if b then -1 else 0
    Discipline' -> -1
    DrawCard' b -> if b then -1 else 0
    LongReach' -> -1
    Ranged' -> -1
    Stupid4' _ -> if isStupid s then 2 else 1
    Unique' -> 0

sortByFst :: [(Int, b)] -> [(Int, b)]
sortByFst l =
  sortBy sortFst l
  where
    sortFst (i, _) (j, _) = compare i j
