{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Board
  ( Teams (..),
    InPlaceEffect (..),
    InPlaceEffects,
    InPlaceType,
    bottomSpotOfTopVisual,
    setPart,
    toHoleyInPlace,
    toInPlaceCreature,
    toHand,
    toPart,
    Board (..),
    DeathCause (..),
    initial,
    HandIndex (..),
    InHandType (),
    lookupHand,
    PlayerPart (..),
    StackType (),
    small,
    spotToLens,
    toStack,
    setStack,
    setHand,
    addToHand,
    addToDiscarded,
    empty,
    increaseScore,
    setCreature,
    toDiscarded,
    setDiscarded,
    setInPlace,
    toInPlace,
    line,
    botSpots,
    StackKind (..),
    Board.appliesTo,
    toScore,
    mapMana,
    mapScore,
    neighbors,
    Neighborhood (..),
    toPlayerHoleyInPlace,
    toNeighbors,
    toPlayerCardSpots,
    isDead,
    toData,
    setMana,
    Board.ManaType,
    switchLine,
    mapInPlace,
  )
where

import Card hiding (ID)
import qualified Card
import Constants
import Control.Lens
import Control.Monad.Except (MonadError, throwError)
import Data.Generics.Labels ()
import Data.Kind (Constraint, Type)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromJust, mapMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import GHC.Generics (Generic)
import Nat
import SharedModel (SharedModel, idToCreature)
import qualified SharedModel
import Spots hiding (Card)
import qualified Spots
import Tile (Tile)

type CardsOnTable = Map.Map Spots.Card (Creature 'Core)

-- | Animations recorded upon death. If you are looking for the same
-- thing in the more general case of the creature not dying, loot at
-- 'fadeOut'.
data DeathCause
  = -- | Creature was killed by fear
    DeathByFear
  | -- | Creature killed by 'BreathIce' attack
    DeathByBreathIce
  | -- | Creature was killed by terror
    DeathByTerror
  | -- | Creature was not killed
    NoDeath
  | -- | Creature was killed for a reason we do not track precisely
    UsualDeath
  deriving (Eq, Generic, Show)

-- | Whether this death cause represents a death
isDead :: DeathCause -> Bool
isDead NoDeath = False
isDead _ = True

instance Semigroup DeathCause where
  DeathByTerror <> _ = DeathByTerror
  _ <> DeathByTerror = DeathByTerror
  DeathByFear <> _ = DeathByFear
  _ <> DeathByFear = DeathByFear
  DeathByBreathIce <> _ = DeathByBreathIce
  _ <> DeathByBreathIce = DeathByBreathIce
  UsualDeath <> _ = UsualDeath
  NoDeath <> d = d

instance Monoid DeathCause where
  mempty = NoDeath

-- It is a bit unfortunate to have these types defined here
-- as they are UI only. However we need them to define the InPlaceType family

-- | Initially this type was for displaying animations only. However
-- Game.hs also uses for Core stuff internally (see applyInPlaceEffectOnBoard).
-- Unfortunate :-( So be careful when changing related code.
data InPlaceEffect = InPlaceEffect
  { -- | Attack value changed
    attackChange :: Int,
    -- | Did creature die? If yes, for what reason
    death :: DeathCause,
    -- | Creature attacked (value used solely for animations)
    attackBump :: Bool,
    -- | Hits points changed
    hitPointsChange :: Int,
    -- | Hit points that were not dealt because the defender died. Contributes
    -- to the score with 'Skill.Powerful'.
    extra :: Nat,
    -- | Card fades-in
    fadeIn :: Bool,
    -- | Tiles to fade out atop the card spot, both when there's a creature
    -- and when there's not.
    fadeOut :: [Tile.Tile],
    -- | Score changed
    scoreChange :: Int
  }
  deriving (Eq, Generic, Show)

instance Semigroup InPlaceEffect where
  InPlaceEffect {attackChange = ac1, death = d1, attackBump = ab1, extra = e1, hitPointsChange = hp1, fadeIn = fi1, fadeOut = fo1, scoreChange = c1}
    <> InPlaceEffect {attackChange = ac2, death = d2, attackBump = ab2, extra = e2, hitPointsChange = hp2, fadeIn = fi2, fadeOut = fo2, scoreChange = c2} =
      InPlaceEffect
        { attackChange = ac1 + ac2,
          death = d1 <> d2,
          attackBump = ab1 || ab2,
          extra = e1 + e2,
          hitPointsChange = hp1 + hp2,
          fadeIn = fi1 || fi2,
          fadeOut = fo1 ++ fo2,
          scoreChange = c1 + c2
        }

instance Monoid InPlaceEffect where
  mempty =
    InPlaceEffect
      { attackChange = 0,
        death = mempty,
        attackBump = False,
        extra = 0,
        hitPointsChange = 0,
        fadeIn = False,
        fadeOut = [],
        scoreChange = 0
      }

type InPlaceEffects = Map.Map Spots.Card InPlaceEffect

-- TODO @smelc Move the type parameter inside the map, to share code
-- between the two cases.
type family InPlaceType (p :: Phase) where
  InPlaceType 'Core = CardsOnTable
  InPlaceType 'UI = InPlaceEffects

type family HandElemType (p :: Phase) where
  HandElemType 'Core = Card.ID
  HandElemType 'UI = Int

type family InHandType (p :: Phase) where
  InHandType p = [HandElemType p]

type family ManaType (p :: Phase) where
  Board.ManaType 'Core = Nat -- Actual mana
  Board.ManaType 'UI = Int -- Difference with previous state

type family ScoreType (p :: Phase) where
  ScoreType 'Core = Nat
  ScoreType 'UI = ()

type family StackType (p :: Phase) where
  StackType 'Core = [Card.ID]
  StackType 'UI = Int -- Discarded->Stack transfer

type family DiscardedType (p :: Phase) where
  DiscardedType 'Core = [Card.ID]
  DiscardedType 'UI = Int -- Board->Discarded transfer

type family TeamType (p :: Phase) where
  TeamType 'Core = Team
  TeamType 'UI = ()

type Forall (c :: Type -> Constraint) (p :: Phase) =
  ( c (HandElemType p),
    c (InPlaceType p),
    c (InHandType p),
    c (Board.ManaType p),
    c (ScoreType p),
    c (StackType p),
    c (DiscardedType p),
    c (TeamType p)
  )

data PlayerPart (p :: Phase) = PlayerPart
  { -- | Cards on the board
    inPlace :: InPlaceType p,
    -- | Cards in hand
    inHand :: InHandType p,
    mana :: Board.ManaType p,
    -- | The score of this player
    score :: ScoreType p,
    stack :: StackType p,
    discarded :: DiscardedType p,
    team :: TeamType p
  }
  deriving (Generic)

deriving instance Board.Forall Eq p => Eq (PlayerPart p)

deriving instance Board.Forall Ord p => Ord (PlayerPart p)

deriving instance Board.Forall Show p => Show (PlayerPart p)

instance Semigroup (PlayerPart 'UI) where
  PlayerPart inPlace1 inHand1 mana1 () s1 d1 () <> PlayerPart inPlace2 inHand2 mana2 () s2 d2 () =
    PlayerPart (inPlace1 <> inPlace2) (inHand1 <> inHand2) (mana1 + mana2) () (s1 + s2) (d1 + d2) ()

instance Monoid (PlayerPart 'UI) where
  mempty = PlayerPart {inPlace = mempty, inHand = mempty, mana = 0, score = (), stack = 0, discarded = 0, team = ()}

-- FIXME @smelc make me a Nat
newtype HandIndex = HandIndex {unHandIndex :: Int}
  deriving (Eq, Show, Generic, Enum)

data StackKind
  = -- | Button to display the hand (only used for the enemy, in Debug mode)
    Handed
  | -- | Button to display the stack
    Stacked
  | -- | Button to display discarded cards
    Discarded
  deriving (Bounded, Enum)

lookupHand ::
  MonadError Text m =>
  [a] ->
  Int ->
  m a
lookupHand hand i
  | i < 0 = throwError $ Text.pack $ "Invalid hand index: " ++ show i
  | i >= handLength =
    throwError $
      Text.pack $
        "Invalid hand index: " ++ show i ++ ". Hand has " ++ show handLength ++ " card(s)."
  | otherwise = return $ hand !! i
  where
    handLength = length hand

-- TODO @smelc rename me to T
data Board (p :: Phase) = Board
  { playerTop :: PlayerPart p,
    playerBottom :: PlayerPart p
  }
  deriving (Generic)

deriving instance Board.Forall Eq p => Eq (Board p)

deriving instance Board.Forall Ord p => Ord (Board p)

deriving instance Board.Forall Show p => Show (Board p)

instance Semigroup (Board 'UI) where
  Board top1 bot1 <> Board top2 bot2 =
    Board (top1 <> top2) (bot1 <> bot2)

instance Monoid (Board 'UI) where
  mempty = Board mempty mempty

addToDiscarded :: Board 'Core -> Spots.Player -> DiscardedType 'Core -> Board 'Core
addToDiscarded board pSpot addition =
  setDiscarded board pSpot $ discarded ++ addition
  where
    discarded = toDiscarded board pSpot

addToHand :: Board p -> Spots.Player -> HandElemType p -> Board p
addToHand board pSpot handElem =
  setPart board pSpot $ part {inHand = hand'}
  where
    part = toPart board pSpot
    hand = inHand part
    hand' = snoc hand handElem

-- TODO @smelc replace by calls to 'mapScore'
increaseScore :: p ~ 'Core => Board p -> Spots.Player -> Nat -> Board p
increaseScore board pSpot change =
  Board.setScore board pSpot (score + change)
  where
    score = Board.toScore board pSpot

-- | Changes the mana at the given 'Spots.Player', applying a function
-- on the existing mana.
mapMana :: Spots.Player -> (Board.ManaType p -> Board.ManaType p) -> Board p -> Board p
mapMana pSpot f board =
  setPart board pSpot $ part {Board.mana = f mana}
  where
    part@PlayerPart {mana} = toPart board pSpot

-- | Map 'f' over creatures in the given 'Spots.Card' in the given 'Spots.Player'
mapInPlace :: (Creature 'Core -> Creature 'Core) -> Spots.Player -> [Spots.Card] -> Board 'Core -> Board 'Core
mapInPlace f pSpot cSpots board =
  setPart board pSpot (part {inPlace = inPlace'})
  where
    part@PlayerPart {inPlace} = Board.toPart board pSpot
    (changed, untouched) = Map.partitionWithKey (\k _ -> k `elem` cSpots) inPlace
    inPlace' = Map.union (Map.map f changed) untouched

mapScore :: Board p -> Spots.Player -> (ScoreType p -> ScoreType p) -> Board p
mapScore board pSpot f =
  Board.setScore board pSpot (f (Board.toScore board pSpot))

-- | Puts a creature, replacing the existing one if any
setCreature :: Spots.Player -> Spots.Card -> Creature 'Core -> Board 'Core -> Board 'Core
setCreature pSpot cSpot creature board =
  setPart board pSpot part'
  where
    part@PlayerPart {inPlace = existing} = toPart board pSpot
    part' = part {inPlace = Map.insert cSpot creature existing}

setDiscarded :: Board p -> Spots.Player -> DiscardedType p -> Board p
setDiscarded board pSpot discarded =
  setPart board pSpot $ part {discarded = discarded}
  where
    part = toPart board pSpot

setInPlace :: Board p -> Spots.Player -> InPlaceType p -> Board p
setInPlace board pSpot inPlace =
  setPart board pSpot $ part {inPlace = inPlace}
  where
    part = toPart board pSpot

setHand :: Board p -> Spots.Player -> InHandType p -> Board p
setHand board pSpot hand =
  setPart board pSpot $ part {inHand = hand}
  where
    part = toPart board pSpot

setMana :: Board.ManaType p -> Spots.Player -> Board p -> Board p
setMana mana pSpot board =
  setPart board pSpot $ part {Board.mana = mana}
  where
    part = toPart board pSpot

setPart :: Board p -> Spots.Player -> PlayerPart p -> Board p
setPart board PlayerTop part = board {playerTop = part}
setPart board PlayerBot part = board {playerBottom = part}

setStack :: Board p -> Spots.Player -> StackType p -> Board p
setStack board pSpot stack =
  setPart board pSpot $ part {stack = stack}
  where
    part = toPart board pSpot

-- TODO @smelc replace by mapScore
setScore :: Board p -> Spots.Player -> ScoreType p -> Board p
setScore board pSpot score =
  setPart board pSpot $ part {score}
  where
    part = toPart board pSpot

toHoleyInPlace :: Board 'Core -> [(Spots.Player, Spots.Card, Maybe (Creature 'Core))]
toHoleyInPlace board =
  [ (pSpot, cSpot, maybeCreature)
    | pSpot <- Spots.allPlayers,
      cSpot <- Spots.allCards,
      let maybeCreature = toInPlaceCreature board pSpot cSpot
  ]

toPlayerCardSpots :: Board 'Core -> Spots.Player -> CardTargetKind -> [Spots.Card]
toPlayerCardSpots board pSpot ctk =
  toPlayerHoleyInPlace board pSpot
    & filter
      ( \(_, maybeCreature) ->
          case (ctk, maybeCreature) of
            (Hole, Nothing) -> True
            (Occupied, Just _) -> True
            _ -> False
      )
    & map fst

toPlayerHoleyInPlace ::
  Board 'Core -> Spots.Player -> [(Spots.Card, Maybe (Creature 'Core))]
toPlayerHoleyInPlace board pSpot =
  [ (cSpot, maybeCreature)
    | cSpot <- Spots.allCards,
      let maybeCreature = toInPlaceCreature board pSpot cSpot
  ]

toDiscarded :: Board p -> Spots.Player -> DiscardedType p
toDiscarded Board {playerTop} PlayerTop = discarded playerTop
toDiscarded Board {playerBottom} PlayerBot = discarded playerBottom

toHand :: Board p -> Spots.Player -> InHandType p
toHand Board {playerTop} PlayerTop = inHand playerTop
toHand Board {playerBottom} PlayerBot = inHand playerBottom

toInPlace :: Board p -> Spots.Player -> InPlaceType p
toInPlace Board {playerTop} PlayerTop = inPlace playerTop
toInPlace Board {playerBottom} PlayerBot = inPlace playerBottom

-- | The neighbors of the card at the given spot, for the given player,
-- and for the given kind of neighbors.
toNeighbors ::
  Board 'Core ->
  Spots.Player ->
  Spots.Card ->
  Neighborhood ->
  [(Spots.Card, Creature 'Core)]
toNeighbors board pSpot cSpot neighborhood =
  [ (cSpot, maybeCreature)
    | cSpot <- neighbors neighborhood cSpot,
      let maybeCreature = toInPlaceCreature board pSpot cSpot
  ]
    & mapMaybe liftJust
  where
    liftJust (f, Just s) = Just (f, s)
    liftJust _ = Nothing

toPart :: Board p -> Spots.Player -> PlayerPart p
toPart Board {playerTop} PlayerTop = playerTop
toPart Board {playerBottom} PlayerBot = playerBottom

toScore :: Board p -> Spots.Player -> ScoreType p
toScore board pSpot = toPart board pSpot & score

toStack :: Board p -> Spots.Player -> StackType p
toStack Board {playerTop} PlayerTop = stack playerTop
toStack Board {playerBottom} PlayerBot = stack playerBottom

toInPlaceCreature ::
  Board 'Core ->
  Spots.Player ->
  Spots.Card ->
  Maybe (Creature 'Core)
toInPlaceCreature board pSpot cSpot = inPlace Map.!? cSpot
  where
    inPlace = toInPlace board pSpot

-- | Class for types that have a default pristine state
class Empty a b where
  empty :: a -> b

instance Empty (Teams Team) (Board 'Core) where
  empty Teams {topTeam, botTeam} =
    Board {playerTop = empty topTeam, playerBottom = empty botTeam}

instance Empty Team (PlayerPart 'Core) where
  empty team = PlayerPart {..}
    where
      inPlace = mempty
      inHand = mempty
      mana = initialMana
      score = 0
      stack = mempty
      discarded = mempty

data Teams a = Teams
  { topTeam :: a,
    botTeam :: a
  }
  deriving (Generic, Show, Functor)

toData :: Spots.Player -> Teams a -> a
toData PlayerTop Teams {topTeam} = topTeam
toData PlayerBot Teams {botTeam} = botTeam

-- | The initial board, appropriately shuffled with 'SharedModel' rng,
-- and the starting decks of both teams.
initial ::
  -- | The shared model, only used for shuffling
  SharedModel ->
  -- | The initial decks
  (Teams (Team, [Card 'Core])) ->
  -- | The shared model, with its RNG updated; and the initial board
  (SharedModel, Board 'Core)
initial shared Teams {topTeam = (topTeam, topDeck), botTeam = (botTeam, botDeck)} =
  (smodel'', Board topPart botPart)
  where
    part team smodel deck = (smodel', (empty team) {inHand = hand', stack = stack'})
      where
        (smodel', deck') = SharedModel.shuffle smodel deck
        (hand, stack) = splitAt initialHandSize deck'
        hand' = map cardToIdentifier hand
        stack' = map cardToIdentifier stack
    (smodel', topPart) = part topTeam shared topDeck
    (smodel'', botPart) = part botTeam smodel' botDeck

-- | A board with a single creature in place. Hands are empty. Handy for
-- debugging for example.
small :: SharedModel -> Teams Team -> CreatureID -> [Item] -> Spots.Player -> Spots.Card -> Board 'Core
small shared teams cid items pSpot cSpot =
  setCreature pSpot cSpot c (empty teams)
  where
    c =
      SharedModel.idToCreature shared cid items
        & fromJust
        & Card.unlift

-- FIXME @smelc remove me
spotToLens :: Spots.Player -> Lens' (Board p) (PlayerPart p)
spotToLens =
  \case
    PlayerBot -> #playerBottom
    PlayerTop -> #playerTop

appliesTo :: Card.ID -> Board 'Core -> Spots.Player -> Spots.Card -> Bool
appliesTo id board pSpot cSpot =
  case (Card.targetType id, toInPlaceCreature board pSpot cSpot) of
    (Card.CardTargetType Occupied, Just _) -> True
    (Card.CardTargetType Hole, Nothing) -> True
    (Card.PlayerTargetType, Nothing) -> True
    _ -> False
