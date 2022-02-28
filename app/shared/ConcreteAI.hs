{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

-- | An AI that is tightly bound to the existing creatures and rules of the game.
-- Like 'HeuristicAI', it doesn't run the game; so it is fast. This AI is not super
-- smart but makes the game feel good playing, because it plays "logically".
module ConcreteAI (play) where

import Board
import Card
import qualified Constants
import Control.Category hiding ((.))
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.List.Extra
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Data.Set as Set hiding (filter, map, partition)
import Data.Tuple.Extra
import qualified Game
import qualified HeuristicAI
import qualified MCTSAI
import SharedModel
import qualified Spots
import qualified Total

-- | Executes the AI.
play ::
  Constants.Difficulty ->
  SharedModel ->
  Board 'Core ->
  -- | The playing player
  Spots.Player ->
  -- | Events generated for player 'pSpot'
  [Game.Place]
play difficulty shared board pSpot =
  ([], shuffleHand board)
    --> first
    --> second
    --> third
    --> reverse first
    --> reverse second
    --> reverse third & fst
  where
    shuffleHand b =
      let hand' = SharedModel.shuffle shared (Board.toHand b pSpot) & snd
       in Board.setHand b pSpot hand'
    first = [ConcreteAI.Creature FrontFighter, ConcreteAI.Creature Shooter, ConcreteAI.Creature Support]
    second = [ConcreteAI.Creature Support, ConcreteAI.Creature Shooter, ConcreteAI.Creature FrontOrBackFighter]
    third = [Item, Neutral]
    (-->) (prev, b) whats =
      case playWhat difficulty shared pSpot whats b of
        Nothing -> (prev, b)
        Just (places, b') -> (prev ++ places, b')

playWhat ::
  Constants.Difficulty ->
  SharedModel ->
  -- | The playing player
  Spots.Player ->
  [What] ->
  Board 'Core ->
  -- | Events generated for player 'pSpot', with the resulting board
  Maybe ([Game.Place], Board 'Core)
playWhat diff shared pSpot what board =
  case what of
    [] -> Nothing
    w : wrest ->
      case playFirst diff shared pSpot board w & SharedModel.pick shared & snd of
        Nothing -> playWhat diff shared pSpot wrest board
        Just (place, board') ->
          case playWhat diff shared pSpot wrest board' of
            Nothing -> Just ([place], board')
            Just (x, board'') -> Just (place : x, board'')

-- | What to play
data What
  = Creature Class
  | Item
  | Neutral

playFirst ::
  Constants.Difficulty ->
  SharedModel ->
  Spots.Player ->
  Board 'Core ->
  What ->
  [(Game.Place, Board 'Core)]
playFirst diff shared pSpot board what =
  case (Board.toHand board pSpot, availMana) of
    ([], _) -> []
    (_, 0) -> []
    (card : rest, availMana) ->
      case (card, mana card, what) of
        (_, Nothing, _) -> error ("Mana of card not found: " ++ show card)
        (_, Just requiredMana, _)
          | availMana < requiredMana ->
              -- Skip card for which we don't have enough mana
              continueWithHand rest
        (Card.IDC cid _, _, ConcreteAI.Creature clazz)
          | toClass cid /= clazz ->
              -- Skip card not matching the desired class
              continueWithHand rest
        (Card.IDC {}, _, ConcreteAI.Creature clazz) ->
          let tgs :: [[Game.Target]] = targets clazz pSpot board
              places :: [[Game.Place]] = map (map (\target -> Game.Place' pSpot target card)) tgs
              pairs = map ((map ((diff, shared, board) ~>)) >>> catMaybes) places
           in concat pairs
        (Card.IDI i, _, Item) ->
          playFirstItem diff shared pSpot board i
        (Card.IDN n, _, Neutral) ->
          playFirstNeutral diff shared pSpot board n
        (_, _, _) -> continueWithHand rest
  where
    availMana = Board.toPart board pSpot & Board.mana
    mana id = SharedModel.toCardCommon shared id <&> Card.mana
    withHand cards b = Board.setHand b pSpot cards
    continueWithHand rest = playFirst diff shared pSpot (withHand rest board) what

-- | Given a state, run one event on this state
(~>) ::
  (Constants.Difficulty, SharedModel, Board 'Core) ->
  Game.Place ->
  Maybe (Game.Place, Board 'Core)
(~>) (diff, shared, board) place =
  MCTSAI.place diff shared place board <&> (place,)

-- | Returns the first non-empty list
firstNE :: [[a]] -> [a]
firstNE = \case [] -> []; ([] : rest) -> firstNE rest; (l : _) -> l

-- | Possible placements of an item that is known already to belong to the hand.
-- A part of 'playFirst' to avoid 'playFirst' to be gigantic.
playFirstItem ::
  Constants.Difficulty ->
  SharedModel ->
  Spots.Player ->
  Board 'Core ->
  Item ->
  [(Game.Place, Board 'Core)]
playFirstItem diff shared pSpot board item =
  case (item, itemToPrefClass item <&> SharedModel.shuffle shared <&> snd) of
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
          mresult = both (map ((diff, shared, board) ~>)) places
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
              mresult = map (map ((diff, shared, board) ~>)) places
           in -- Shuffle because classes are unordered
              map catMaybes mresult & SharedModel.shuffle shared & snd & firstNE
    (_, pref) -> error $ "Unexpected item/itemToPrefClass combination: " ++ show item ++ "/" ++ show pref
  where
    toEvent cspot = Game.Place' pSpot (Game.CardTarget pSpot cspot) (Card.IDI item)

-- | Possible placements of a neutral that is known already to belong to the hand.
-- A part of 'playFirst' to avoid 'playFirst' to be gigantic.
playFirstNeutral ::
  Constants.Difficulty ->
  SharedModel ->
  Spots.Player ->
  Board 'Core ->
  Neutral ->
  [(Game.Place, Board 'Core)]
playFirstNeutral diff shared pSpot board neutral =
  let tgs :: [Game.Target] = HeuristicAI.targets board pSpot id
      places :: [Game.Place] = map (\target -> Game.Place' pSpot target id) tgs
      result :: [Maybe (Game.Place, Board 'Core)] = map ((diff, shared, board) ~>) places
   in catMaybes result
  where
    id = Card.IDN neutral

-- | Possible targets for playing a card of the given 'Class'. Returned lists
-- are ordered by "best first" and all elements of an inner list are equivalent
-- (so can be randomly picked from).
targets :: Class -> Spots.Player -> Board 'Core -> [[Game.Target]]
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

freeSpots :: Spots.Line -> Spots.Player -> Board 'Core -> Set Spots.Card
freeSpots l pSpot board =
  (Spots.line l & Set.fromList)
    Set.\\ (Board.toInPlace board pSpot & Map.keys & Set.fromList)

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
          _ -> error msg
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
          _ -> error msg
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
          _ -> error msg
      ZKnights ->
        case kind of
          Captain -> FrontFighter
          King -> Support
          Knight -> FrontFighter
          Priest -> Support
          Squire -> Support
          Trebuchet -> Support
          Veteran -> FrontFighter
          _ -> error msg
    where
      msg = "cannot classify " ++ show id

instance ToClass (Creature 'Core) where
  toClass = creatureId >>> toClass

-- | Order doesn't matter. To which class of creatures an item should be applied to.
itemToPrefClass :: Item -> Maybe [Class]
itemToPrefClass =
  \case
    AxeOfRage -> Just [FrontFighter]
    Crown -> Nothing
    CrushingMace -> Just [FrontFighter]
    FlailOfTheDamned -> Just [FrontFighter]
    SkBanner -> Just [Support, Shooter]
    SpikyMace -> Just [FrontFighter]
    SwordOfMight -> Just [FrontFighter]
