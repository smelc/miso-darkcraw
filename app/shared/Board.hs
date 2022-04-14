{-# LANGUAGE AllowAmbiguousTypes #-}
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
  ( Board.appliesTo,
    Teams (..),
    InPlaceEffect (..),
    InPlaceEffects,
    InPlaceType,
    bottomSpotOfTopVisual,
    setPart,
    toHoleyInPlace,
    toInPlaceCreature,
    toPart,
    DeathCause (..),
    Deco (..),
    initial,
    HandIndex (..),
    InHandType (),
    Kind (..),
    lookupHand,
    lookupHandM,
    Mappable (..),
    PlayerKindIndexed (..),
    PlayerPart (..),
    StackType (),
    small,
    empty,
    setInPlace,
    toInPlace,
    line,
    StackKind (..),
    neighbors,
    Neighborhood (..),
    T (..),
    toPlayerHoleyInPlace,
    toNeighbors,
    toPlayerCardSpots,
    isDead,
    toData,
    Board.ManaType,
    switchLine,
  )
where

import Card hiding (ID)
import qualified Card
import qualified Constants
import Control.Monad.Except (MonadError, throwError)
import Data.Function ((&))
import Data.Generics.Labels ()
import Data.Kind (Constraint, Type)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromJust, mapMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import GHC.Generics (Generic)
import Nat
import qualified Random
import qualified Shared
import Spots hiding (Card)
import qualified Spots
import System.Random (RandomGen)
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
    -- | Card fade-in or fade-out
    fade :: Constants.Fade,
    -- | Tiles to fade out atop the card spot, both when there's a creature
    -- and when there's not.
    fadeOut :: [Tile.Tile],
    -- | Score changed
    scoreChange :: Int
  }
  deriving (Eq, Generic, Show)

instance Semigroup InPlaceEffect where
  InPlaceEffect {attackChange = ac1, death = d1, attackBump = ab1, extra = e1, hitPointsChange = hp1, fade = fi1, fadeOut = fo1, scoreChange = c1}
    <> InPlaceEffect {attackChange = ac2, death = d2, attackBump = ab2, extra = e2, hitPointsChange = hp2, fade = fi2, fadeOut = fo2, scoreChange = c2} =
      InPlaceEffect
        { attackChange = ac1 + ac2,
          death = d1 <> d2,
          attackBump = ab1 || ab2,
          extra = e1 + e2,
          hitPointsChange = hp1 + hp2,
          fade = fi1 <> fi2,
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
        fade = mempty,
        fadeOut = [],
        scoreChange = 0
      }

type InPlaceEffects = Map.Map Spots.Card InPlaceEffect

data Deco
  = -- Usually I would have NoDeco here, but because these values are stored
    -- in maps, it creates an ambiguity w.r.t. to absence in the map.
    Forest
  deriving (Bounded, Enum, Eq, Generic, Show)

instance Semigroup Deco where
  Forest <> Forest = Forest

type family DecoType (p :: Phase) where
  DecoType 'Core = Deco
  DecoType 'UI = Constants.Fade

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
  ( c (DecoType p),
    c (HandElemType p),
    c (InPlaceType p),
    c (InHandType p),
    c (Board.ManaType p),
    c (ScoreType p),
    c (StackType p),
    c (DiscardedType p),
    c (TeamType p)
  )

data PlayerPart (p :: Phase) = PlayerPart
  { deco :: Map.Map Spots.Card (DecoType p),
    -- | Cards on the board
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
  PlayerPart deco1 inPlace1 inHand1 mana1 () s1 d1 () <> PlayerPart deco2 inPlace2 inHand2 mana2 () s2 d2 () =
    PlayerPart (deco1 <> deco2) (inPlace1 <> inPlace2) (inHand1 <> inHand2) (mana1 + mana2) () (s1 + s2) (d1 + d2) ()

instance Monoid (PlayerPart 'UI) where
  mempty = PlayerPart {..}
    where
      (deco, inPlace, inHand) = (mempty, mempty, mempty)
      (discarded, mana, stack) = (0, 0, 0)
      (score, team) = ((), ())

-- FIXME @smelc make me a Nat
newtype HandIndex = HandIndex {unHandIndex :: Int}
  deriving (Eq, Show, Generic, Enum)

data StackKind
  = -- | Button to display the hand (only used for the enemy, in Debug mode)
    Handed
  | -- | Button to display the stack
    Stacked
  | -- | Button to display discarded cards
    Discarded'
  deriving (Bounded, Enum)

lookupHandM ::
  MonadError Text m =>
  [a] ->
  Int ->
  m a
lookupHandM hand i =
  case lookupHand hand i of
    Left msg -> throwError msg
    Right elem -> return elem

lookupHand ::
  [a] ->
  Int ->
  Either Text a
lookupHand hand i
  | i < 0 = Left $ Text.pack $ "Invalid hand index: " ++ show i
  | i >= handLength =
      Left $
        Text.pack $
          "Invalid hand index: " ++ show i ++ ". Hand has " ++ show handLength ++ " card(s)."
  | otherwise = Right (hand !! i)
  where
    handLength = length hand

-- | Things on the board (hand excluded)
data T (p :: Phase) = T
  { playerTop :: PlayerPart p,
    playerBottom :: PlayerPart p
  }
  deriving (Generic)

deriving instance Board.Forall Eq p => Eq (T p)

deriving instance Board.Forall Ord p => Ord (T p)

deriving instance Board.Forall Show p => Show (T p)

instance Semigroup (T 'UI) where
  T top1 bot1 <> T top2 bot2 =
    T (top1 <> top2) (bot1 <> bot2)

instance Monoid (T 'UI) where
  mempty = T mempty mempty

-- | Functions to update content within 'Board.T' values
class Mappable p a where
  -- | Maps over the data at the given 'Spots.Player' and 'Spots.Card'
  adjust :: Spots.Player -> Spots.Card -> (a -> a) -> T p -> T p

  -- | Maps over the data at the given 'Spots.Player' and 'Spots.Card'
  adjustMany :: Spots.Player -> [Spots.Card] -> (a -> a) -> T p -> T p

  -- | Sets the data at the given 'Spots.Player' and 'Spots.Card'
  insert :: Spots.Player -> Spots.Card -> a -> T p -> T p

  -- | Updates the data at the given 'Spots.Player' and 'Spots.Card'. Updates
  -- if the function returns @Just _@ or removes the data if @f@ returns @Nothing@.
  update :: Spots.Player -> Spots.Card -> (a -> Maybe a) -> T p -> T p

-- | Lenses for data that belongs to a player
class PlayerIndexed a where
  getp :: Spots.Player -> Board.T 'Core -> a
  setp :: a -> Spots.Player -> Board.T 'Core -> Board.T 'Core

-- | Generic access to the creatures.
instance PlayerIndexed (Map.Map Spots.Card (Creature 'Core)) where
  getp pSpot b = Board.toInPlace b pSpot
  setp inPlace pSpot b = Board.setPart b pSpot ((Board.toPart b pSpot) {inPlace})

-- | Generic access to the deco.
instance PlayerIndexed (Map.Map Spots.Card Deco) where
  getp pSpot b = Board.toPart b pSpot & Board.deco
  setp deco pSpot b = Board.setPart b pSpot ((Board.toPart b pSpot) {deco})

-- | Lift lenses over maps to the 'Mappable' class. Used with @a@ being
-- @Creature 'Core@ (to map over 'inPlace') and with @a@ being @Deco@
-- (to map over 'deco').
instance PlayerIndexed (Map.Map Spots.Card a) => Mappable 'Core a where
  adjust pSpot cSpot f b =
    let m = getp pSpot b
     in setp (Map.adjust f cSpot m) pSpot b
  adjustMany pSpot cSpots f b =
    setp (Map.union (Map.map f changed) untouched) pSpot b
    where
      m = getp pSpot b
      (changed, untouched) = Map.partitionWithKey (\k _ -> k `elem` cSpots) m
  insert pSpot cSpot x b =
    let m = getp pSpot b
     in setp (Map.insert cSpot x m) pSpot b
  update pSpot cSpot f b =
    let m = getp pSpot b
     in setp (Map.update f cSpot m) pSpot b

-- | Data whose only purpose is to be lifted as a type, for disambiguating
-- classes instances.
data Kind = Discarded | Hand | Mana | Score | Stack

-- | Class to generically get/map/set over the data of a player
class PlayerKindIndexed (k :: Kind) p a where
  getpk :: Spots.Player -> Board.T p -> a
  mappk :: (a -> a) -> Spots.Player -> Board.T p -> Board.T p
  setpk :: Spots.Player -> a -> Board.T p -> Board.T p

-- | Instance of 'PlayerKindIndexed' to generically get/map/set over the discarded stack
instance (Board.DiscardedType p ~ fam) => PlayerKindIndexed 'Discarded p fam where
  getpk pSpot b = Board.toPart b pSpot & Board.discarded
  mappk f pSpot b =
    let part@PlayerPart {Board.discarded = x} = Board.toPart b pSpot
     in Board.setPart b pSpot (part {Board.discarded = f x})
  setpk pSpot x b = Board.setPart b pSpot $ (Board.toPart b pSpot) {Board.discarded = x}

-- | Instance of 'PlayerKindIndexed' to generically get/map/set over the hand
instance (Board.InHandType p ~ fam) => PlayerKindIndexed 'Hand p fam where
  getpk pSpot b = Board.toPart b pSpot & Board.inHand
  mappk f pSpot b =
    let part@PlayerPart {Board.inHand = x} = Board.toPart b pSpot
     in Board.setPart b pSpot (part {Board.inHand = f x})
  setpk pSpot x b = Board.setPart b pSpot $ (Board.toPart b pSpot) {Board.inHand = x}

-- | Instance of 'PlayerKindIndexed' to generically get/map/set over mana
instance (Board.ManaType p ~ fam) => PlayerKindIndexed 'Mana p fam where
  getpk pSpot b = Board.toPart b pSpot & Board.mana
  mappk f pSpot b =
    let part@PlayerPart {Board.mana = x} = Board.toPart b pSpot
     in Board.setPart b pSpot (part {Board.mana = f x})
  setpk pSpot x b = Board.setPart b pSpot $ (Board.toPart b pSpot) {Board.mana = x}

-- | Instance of 'PlayerKindIndexed' to generically get/map/set over score
instance (Board.ScoreType p ~ fam) => PlayerKindIndexed 'Score p fam where
  getpk pSpot b = Board.toPart b pSpot & Board.score
  mappk f pSpot b =
    let part@PlayerPart {Board.score = x} = Board.toPart b pSpot
     in Board.setPart b pSpot (part {Board.score = f x})
  setpk pSpot x b = Board.setPart b pSpot $ (Board.toPart b pSpot) {Board.score = x}

-- | Instance of 'PlayerKindIndexed' to generically get/map/set over the stack
instance (Board.StackType p ~ fam) => PlayerKindIndexed 'Stack p fam where
  getpk pSpot b = Board.toPart b pSpot & Board.stack
  mappk f pSpot b =
    let part@PlayerPart {Board.stack = x} = Board.toPart b pSpot
     in Board.setPart b pSpot (part {Board.stack = f x})
  setpk pSpot x b = Board.setPart b pSpot $ (Board.toPart b pSpot) {Board.stack = x}

setInPlace :: Spots.Player -> InPlaceType p -> T p -> T p
setInPlace pSpot inPlace board =
  setPart board pSpot $ part {inPlace = inPlace}
  where
    part = toPart board pSpot

setPart :: T p -> Spots.Player -> PlayerPart p -> T p
setPart board PlayerTop part = board {playerTop = part}
setPart board PlayerBot part = board {playerBottom = part}

toHoleyInPlace :: T 'Core -> [(Spots.Player, Spots.Card, Maybe (Creature 'Core))]
toHoleyInPlace board =
  [ (pSpot, cSpot, maybeCreature)
    | pSpot <- Spots.allPlayers,
      cSpot <- Spots.allCards,
      let maybeCreature = toInPlaceCreature board pSpot cSpot
  ]

-- | Returns the spots available for playing a card with the given target kind.
-- For creature cards for example it returns empty spots. For item cards
-- it returns occupied spots.
toPlayerCardSpots :: T 'Core -> Spots.Player -> CardTargetKind -> [Spots.Card]
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
  T 'Core -> Spots.Player -> [(Spots.Card, Maybe (Creature 'Core))]
toPlayerHoleyInPlace board pSpot =
  [ (cSpot, maybeCreature)
    | cSpot <- Spots.allCards,
      let maybeCreature = toInPlaceCreature board pSpot cSpot
  ]

toInPlace :: T p -> Spots.Player -> InPlaceType p
toInPlace T {playerTop} PlayerTop = inPlace playerTop
toInPlace T {playerBottom} PlayerBot = inPlace playerBottom

-- | The neighbors of the card at the given spot, for the given player,
-- and for the given kind of neighbors.
toNeighbors ::
  T 'Core ->
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

-- TODO @smelc change parameters order
toPart :: T p -> Spots.Player -> PlayerPart p
toPart T {playerTop} PlayerTop = playerTop
toPart T {playerBottom} PlayerBot = playerBottom

toInPlaceCreature ::
  T 'Core ->
  Spots.Player ->
  Spots.Card ->
  Maybe (Creature 'Core)
toInPlaceCreature board pSpot cSpot = inPlace Map.!? cSpot
  where
    inPlace = toInPlace board pSpot

-- | Class for types that have a default pristine state
class Empty a b where
  empty :: a -> b

instance Empty (Teams Team) (T 'Core) where
  empty Teams {topTeam, botTeam} =
    T {playerTop = empty topTeam, playerBottom = empty botTeam}

instance Empty Team (PlayerPart 'Core) where
  empty team = PlayerPart {..}
    where
      (deco, inPlace, inHand) = (mempty, mempty, mempty)
      mana = Constants.initialMana
      score = 0
      (stack, discarded) = (mempty, mempty)

data Teams a = Teams
  { topTeam :: a,
    botTeam :: a
  }
  deriving (Generic, Show, Functor)

toData :: Spots.Player -> Teams a -> a
toData PlayerTop Teams {topTeam} = topTeam
toData PlayerBot Teams {botTeam} = botTeam

-- | The initial board, appropriately shuffled with a rng,
-- and the starting decks of both teams.
initial ::
  RandomGen r =>
  r ->
  -- | The initial decks
  (Teams (Team, [Card 'Core])) ->
  -- | The shared model, with its RNG updated; and the initial board
  (r, T 'Core)
initial shared Teams {topTeam = (topTeam, topDeck), botTeam = (botTeam, botDeck)} =
  (smodel'', T topPart botPart)
  where
    part team smodel deck = (smodel', (empty team) {inHand = hand', stack = stack'})
      where
        (deck', smodel') = Random.shuffle smodel deck
        (hand, stack) = splitAt Constants.initialHandSize deck'
        hand' = map cardToIdentifier hand
        stack' = map cardToIdentifier stack
    (smodel', topPart) = part topTeam shared topDeck
    (smodel'', botPart) = part botTeam smodel' botDeck

-- | A board with a single creature in place. Hands are empty. Handy for
-- debugging for example.
small :: Shared.Model -> Teams Team -> CreatureID -> [Item] -> Spots.Player -> Spots.Card -> T 'Core
small shared teams cid items pSpot cSpot =
  let c = Shared.idToCreature shared cid items & fromJust & Card.unlift
   in insert pSpot cSpot c (empty teams)

appliesTo :: Card.ID -> T 'Core -> Spots.Player -> Spots.Card -> Bool
appliesTo id board pSpot cSpot =
  case (Card.targetType id, toInPlaceCreature board pSpot cSpot) of
    (Card.CardTargetType Occupied, Just _) -> True
    (Card.CardTargetType Hole, Nothing) -> True
    (Card.PlayerTargetType, Nothing) -> True
    _ -> False
