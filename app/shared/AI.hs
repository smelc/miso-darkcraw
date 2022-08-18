{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

-- | An AI that is tightly bound to the existing creatures and rules of the game.
-- It doesn't run the game; so it is fast. This AI is not super
-- smart but makes the game feel good playing, because it plays "logically".
module AI (play) where

import qualified Board
import Card
import qualified Constants
import Contains (with)
import Control.Category hiding ((.))
import Control.Monad.Except
import Data.Either.Extra
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.List.Extra
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Data.Set as Set hiding (filter, map, partition)
import Data.Tuple.Extra
import qualified Game
import qualified Mana
import qualified Move
import qualified Random
import qualified Shared
import qualified Spots
import qualified Total
import qualified Turn

-- | Executes the AI.
play ::
  Constants.Difficulty ->
  Shared.Model ->
  Board.T 'Core ->
  -- | The playing player
  Spots.Player ->
  Turn.T ->
  -- | Events generated for player 'pSpot'
  [Game.Place]
play difficulty shared board pSpot turn =
  ([], shuffleHand board)
    --> first
    --> second
    --> third
    --> reverse first
    --> reverse second
    --> reverse third & fst
  where
    shuffleHand = Board.mappk @'Board.Hand (fst . Random.shuffle shared) pSpot
    first = [AI.Creature FrontFighter, AI.Creature Shooter, AI.Creature Support]
    second = [AI.Creature Support, AI.Creature Shooter, AI.Creature FrontOrBackFighter]
    third = [Item, Neutral]
    (-->) (prev, b) whats =
      case playWhat difficulty shared pSpot $ Game.Playable {board = b, event = whats, turn} of
        Nothing -> (prev, b)
        Just (places, b') -> (prev ++ places, b')

playWhat ::
  Constants.Difficulty ->
  Shared.Model ->
  -- | The playing player
  Spots.Player ->
  Game.Playable [What] ->
  -- | Events generated for player 'pSpot', with the resulting board
  Maybe ([Game.Place], Board.T 'Core)
playWhat diff shared pSpot p@Game.Playable {event = what} =
  case what of
    [] -> Nothing
    w : wrest ->
      case playFirst diff shared pSpot (p {Game.event = w}) & Random.pick shared & fst of
        Nothing -> playWhat diff shared pSpot (p `with` wrest)
        Just (place, board') ->
          case playWhat diff shared pSpot (p `with` (board', wrest)) of
            Nothing -> Just ([place], board')
            Just (x, board'') -> Just (place : x, board'')

-- | What to play
data What
  = Creature Class
  | Item
  | Neutral

playFirst ::
  Constants.Difficulty ->
  Shared.Model ->
  Spots.Player ->
  Game.Playable What ->
  [(Game.Place, Board.T 'Core)]
playFirst diff shared pSpot p@Game.Playable {board, event = what, turn} =
  case (Board.getpk @'Board.Hand pSpot board, availMana) of
    ([], _) -> []
    (_, 0) -> []
    (card : rest, availMana) ->
      case (card, mana card, what) of
        (_, Nothing, _) -> error ("Mana of card not found: " ++ show card)
        (_, Just requiredMana, _)
          | (Mana.>) turn requiredMana availMana ->
              -- Skip card for which we don't have enough mana
              continueWithHand rest
        (Card.IDC cid _, _, AI.Creature clazz)
          | toClass cid /= clazz ->
              -- Skip card not matching the desired class
              continueWithHand rest
        (Card.IDC {}, _, AI.Creature clazz) ->
          let tgs :: [[Game.Target]] = targets clazz pSpot board
              places :: [[Game.Place]] = map (map (\target -> Game.Place' pSpot target card)) tgs
              pairs = map ((map ((diff, shared, board, turn) ~>)) >>> catMaybes) places
           in concat pairs
        (Card.IDI i, _, Item) ->
          playFirstItem diff shared pSpot (Game.mkPlayable board i turn)
        (Card.IDN n, _, Neutral) ->
          playFirstNeutral diff shared pSpot (Game.mkPlayable board n turn)
        (_, _, _) -> continueWithHand rest
  where
    availMana = Board.toPart board pSpot & Board.mana
    mana id = Shared.keyToCardCommon shared id <&> Card.mana
    withHand cards = Board.setpk @'Board.Hand pSpot cards
    continueWithHand rest = playFirst diff shared pSpot (p `with` (withHand rest board))

-- | Given a state, run one event on this state
(~>) ::
  (Constants.Difficulty, Shared.Model, Board.T 'Core, Turn.T) ->
  Game.Place ->
  Maybe (Game.Place, Board.T 'Core)
(~>) (diff, shared, board, turn) p =
  place diff shared p board turn <&> (p,)

-- | Returns the first non-empty list
firstNE :: [[a]] -> [a]
firstNE = \case [] -> []; ([] : rest) -> firstNE rest; (l : _) -> l

-- | Possible placements of an item that is known already to belong to the hand.
-- A part of 'playFirst' to avoid 'playFirst' to be gigantic.
playFirstItem ::
  Constants.Difficulty ->
  Shared.Model ->
  Spots.Player ->
  Game.Playable Item ->
  [(Game.Place, Board.T 'Core)]
playFirstItem diff shared pSpot Game.Playable {board, event = item, turn} =
  case (item, itemToPrefClass item <&> Random.shuffle shared <&> fst) of
    (Crown, Nothing) ->
      -- Crown case, look for a fighter, ideally centered; that doesn't have discipline
      let spots :: [Spots.Card] =
            Board.toInPlace board pSpot
              & Map.filter (\c -> toClass c `elem` [FrontFighter, FrontOrBackFighter])
              & Map.filter (not . Total.isDisciplined)
              & Map.keys
          order :: ([Spots.Card], [Spots.Card]) =
            -- Centered spots are preferred: put them first. They are more likely
            -- to receive a neighbor.
            partition Spots.isCentered spots
          places :: ([Game.Place], [Game.Place]) =
            both (map toEvent) order
          mresult = both (map ((diff, shared, board, turn) ~>)) places
       in both catMaybes mresult & (\(a, b) -> [a, b]) & firstNE
    (_, Just classes)
      | notNull classes ->
          -- generic case, classes are unordered
          let inPlace :: Map Spots.Card (Creature 'Core) =
                Board.toInPlace board pSpot
              candidatesm :: [Map Spots.Card (Creature 'Core)] =
                map (\clazz -> Map.filter (\c -> toClass c == clazz) inPlace) classes
              candidates :: [Map Spots.Card (Creature 'Core)] =
                filter (not . Map.null) candidatesm
              places :: [[Game.Place]] =
                map (Map.toList >>> map (fst >>> toEvent)) candidates
              mresult = map (map ((diff, shared, board, turn) ~>)) places
           in -- Shuffle because classes are unordered
              map catMaybes mresult & Random.shuffle shared & fst & firstNE
    (_, pref) -> error $ "Unexpected item/itemToPrefClass combination: " ++ show item ++ "/" ++ show pref
  where
    toEvent cspot = Game.Place' pSpot (Game.CardTarget pSpot cspot) (Card.IDI item)

-- | Possible placements of a neutral that is known already to belong to the hand.
-- A part of 'playFirst' to avoid 'playFirst' to be gigantic.
playFirstNeutral ::
  Constants.Difficulty ->
  Shared.Model ->
  Spots.Player ->
  Game.Playable Neutral ->
  [(Game.Place, Board.T 'Core)]
playFirstNeutral diff shared pSpot Game.Playable {board, event = neutral, turn} =
  let tgs :: [Game.Target] = targets' board pSpot id
      places :: [Game.Place] = map (\target -> Game.Place' pSpot target id) tgs
      result :: [Maybe (Game.Place, Board.T 'Core)] = map ((diff, shared, board, turn) ~>) places
   in catMaybes result
  where
    id = Card.IDN neutral

-- | Possible targets for playing a card of the given 'Class'. Returned lists
-- are ordered by "best first" and all elements of an inner list are equivalent
-- (so can be randomly picked from).
targets :: Class -> Spots.Player -> Board.T 'Core -> [[Game.Target]]
targets clazz pSpot board =
  case clazz of
    -- TODO @smelc, prefer spots with enemies in front
    FrontFighter -> [free Spots.Front]
    FrontOrBackFighter -> [free Spots.Front ++ free Spots.Back]
    -- TODO @smelc, prefer spots with an ally in front
    Shooter -> [free Spots.Back]
    -- TODO @smelc, prefer spots with an ally in front
    Support -> [free Spots.Back]
  where
    free line = freeSpots line pSpot board & Set.toList & map (Game.CardTarget pSpot)

targets' ::
  Board.T 'Core ->
  -- | The player placing a card
  Spots.Player ->
  -- | The card being played
  Card.ID ->
  -- | All spots where the card can be put
  [Game.Target]
targets' board playingPlayer id =
  case (Card.targetType id, Game.whichPlayerTarget id) of
    (CardTargetType ctk, Game.Playing) ->
      cardTargets playingPlayer ctk
    (CardTargetType ctk, Game.Opponent) ->
      cardTargets (Spots.other playingPlayer) ctk
    (PlayerTargetType, Game.Playing) ->
      [Game.PlayerTarget playingPlayer]
    (PlayerTargetType, Game.Opponent) ->
      [Game.PlayerTarget $ Spots.other playingPlayer]
  where
    cardTargets pSpot ctk =
      Board.toPlayerCardSpots board pSpot ctk & map (Game.CardTarget pSpot)

freeSpots :: Spots.Line -> Spots.Player -> Board.T 'Core -> Set Spots.Card
freeSpots l pSpot board =
  (Spots.line l & Set.fromList)
    Set.\\ (Board.toInPlace board pSpot & Map.keys & Set.fromList)

-- | Plays one event and plays following events if any. The point of
-- this function is to get the next state after playing one card. If the
-- card cannot be played, 'Nothing' is returned. We cannot use 'Game.maybePlay'
-- because it doesn't enqueue the created events. We cannot use 'Game.playAll'
-- because it doesn't distinguish if the first event succeeded. If it didn't,
-- we MUST know it, so that the AI skips this event.
place :: Constants.Difficulty -> Shared.Model -> Game.Place -> Board.T 'Core -> Turn.T -> Maybe (Board.T 'Core)
place difficulty shared place board turn =
  case Game.maybePlay shared (Game.Playable board (Game.PEvent place) turn) of
    Nothing -> Nothing
    Just (_, board', Nothing) -> Just board'
    Just (shared', board', Just nextEvent) ->
      Move.simRunAllMaybe (nextEvent & Move.Play & Just & Move.nextify) kernel
        & runExcept
        & eitherToMaybe
        <&> Move.board
      where
        kernel = mkKernel difficulty shared' (Game.toSpot place) board'

-- | Make a 'Kernel' value for simulating the given board
mkKernel :: Constants.Difficulty -> Shared.Model -> Spots.Player -> Board.T 'Core -> Move.Kernel ()
mkKernel difficulty shared pSpot =
  Move.mkSimKernel difficulty shared turn
  where
    turn = Turn.initial & (\t -> if Turn.toPlayerSpot t == pSpot then t else Turn.next t)

-- | The class of creatures
data Class
  = -- | Fighters that should be put in front
    FrontFighter
  | -- | Fighters that can go anywhere
    FrontOrBackFighter
  | -- | Shooters: must be put in the back, especially behind a fighter
    Shooter
  | -- | Support : must be put in the back, especially behind a fighter
    Support
  deriving (Eq, Show)

class ToClass a where
  toClass :: a -> Class

instance ToClass Card.CreatureID where
  toClass id@Card.CreatureID {team, creatureKind = kind} =
    case team of
      Beastmen ->
        case kind of
          Defender -> FrontFighter
          Minotaur -> FrontFighter
          _ -> error $ msg team
      Evil ->
        case kind of
          Abomination -> FrontFighter
          Assassin -> FrontOrBackFighter -- FIXME @smelc make me BackFighter
          Beholder -> Support
          Daemon -> FrontFighter
          Knight -> FrontFighter
          Priest -> Support
          Spearman -> FrontOrBackFighter
          Troll -> FrontFighter
          _ -> error $ msg team
      Human ->
        case kind of
          Archer -> Shooter
          Church -> Support
          General -> FrontFighter
          Knight -> FrontFighter
          Ogre -> FrontFighter
          Priest -> Support
          Spearman -> FrontOrBackFighter
          Swordsman -> FrontFighter
          _ -> error $ msg team
      Sylvan ->
        case kind of
          Archer -> Shooter
          Bear -> FrontFighter
          Falcon -> FrontFighter
          Falconer -> FrontFighter
          Guardian -> FrontFighter
          Priest -> Support
          Ranger -> Shooter
          Spearman -> FrontOrBackFighter
          Tree -> FrontFighter
          Worm -> FrontFighter
          _ -> error $ msg team
      Undead ->
        case kind of
          Archer -> Shooter
          Ghost -> Support
          Necromancer -> Support
          Mummy -> FrontFighter
          Shade -> FrontFighter
          Skeleton -> FrontFighter
          Specter -> FrontFighter
          Warrior -> FrontFighter
          Vampire -> FrontFighter
          _ -> error $ msg team
      ZKnights ->
        case kind of
          Bird -> FrontFighter
          Captain -> FrontFighter
          King -> Support
          Knight -> FrontFighter
          Priest -> Support
          Squire -> Support
          Trebuchet -> Support
          Veteran -> FrontFighter
          _ -> error $ msg team
    where
      msg t = "cannot classify " ++ show id ++ " in team " ++ show t

instance ToClass (Creature 'Core) where
  toClass = creatureId >>> toClass

-- | Order doesn't matter. To which class of creatures an item should be applied to.
itemToPrefClass :: Item -> Maybe [Class]
itemToPrefClass =
  \case
    AxeOfRage -> Just [FrontFighter]
    BannerFeather -> Just [Support]
    BowOfGaia -> Just [Shooter]
    BowOfStrength -> Just [Shooter]
    CloakOfGaia -> Just [FrontFighter, Shooter, Support]
    Crown -> Nothing
    CrushingMace -> Just [FrontFighter]
    FlailOfTheDamned -> Just [FrontFighter]
    SkBanner -> Just [Support, Shooter]
    SpikyMace -> Just [FrontFighter]
    SwordOfBlood -> Just [FrontFighter]
    SwordOfMight -> Just [FrontFighter]
