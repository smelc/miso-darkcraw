{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
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
  ( allCardsSpots,
    allPlayersSpots,
    Teams (..),
    InPlaceEffect (..),
    InPlaceEffects (..),
    bottomSpotOfTopVisual,
    setPart,
    toHoleyInPlace,
    toInPlaceCreature,
    toHand,
    toPart,
    Board (..),
    CardSpot (..),
    DeathCause (..),
    endingPlayerSpot,
    initial,
    HandIndex (..),
    inTheBack,
    InHandType (),
    lookupHand,
    otherPlayerSpot,
    PlayerPart (..),
    PlayerSpot (..),
    StackType (),
    small,
    startingPlayerSpot,
    spotToLens,
    toStack,
    setStack,
    setHand,
    addToHand,
    addToDiscarded,
    empty,
    setCreature,
    toDiscarded,
    setDiscarded,
    setInPlace,
    toInPlace,
    topSpots,
    botSpots,
    toASCII,
    StackKind (..),
    Board.appliesTo,
    toScore,
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
import Data.List (intercalate, intersperse)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromJust, fromMaybe, mapMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import GHC.Generics (Generic)
import Nat
import SharedModel (SharedModel, idToCreature)
import qualified SharedModel
import Tile (Tile)
import qualified Total

-- | The spot of a card, as visible from the top of the screen. For the
-- | bottom part, think as if it was in the top, turning the board
-- | 180 degrees clockwise; or use these values and map [bottomSpotOfTopVisual].
data CardSpot
  = TopLeft
  | Top
  | TopRight
  | BottomLeft
  | Bottom
  | BottomRight
  deriving (Bounded, Enum, Eq, Ord, Show, Generic)

allCardsSpots :: [CardSpot]
allCardsSpots = [minBound ..]

type CardsOnTable = Map.Map CardSpot (Creature 'Core)

-- | A convenience constructor to create the bottom part of a board
-- | by using the CardSpot that you see instead of having to consider
-- | the 180 degrees rotation mentioned in CardSpot
_makeBottomCardsOnTable :: CardsOnTable -> CardsOnTable
_makeBottomCardsOnTable = Map.mapKeys bottomSpotOfTopVisual

-- | Returns a bottom position, by taking a position that makes sense visually
-- | I.e. if you give this method [TopLeft], it'll correspond to the [TopLeft]
-- | bottom position that you SEE; even if positions make sense for the top
-- | part. This method takes care of translating correctly.
bottomSpotOfTopVisual :: CardSpot -> CardSpot
bottomSpotOfTopVisual = \case
  TopLeft -> BottomRight
  Top -> Bottom
  TopRight -> BottomLeft
  BottomLeft -> TopRight
  Bottom -> Top
  BottomRight -> TopLeft

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
    -- | Card fades-in
    fadeIn :: Bool,
    -- | Tiles to fade out atop the card
    fadeOut :: [Tile.Tile],
    -- | Score changed
    scoreChange :: Int
  }
  deriving (Eq, Generic, Show)

instance Semigroup InPlaceEffect where
  InPlaceEffect {attackChange = ac1, death = d1, attackBump = ab1, hitPointsChange = hp1, fadeIn = fi1, fadeOut = fo1, scoreChange = c1}
    <> InPlaceEffect {attackChange = ac2, death = d2, attackBump = ab2, hitPointsChange = hp2, fadeIn = fi2, fadeOut = fo2, scoreChange = c2} =
      InPlaceEffect
        { attackChange = ac1 + ac2,
          death = d1 <> d2,
          attackBump = ab1 || ab2,
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
        hitPointsChange = 0,
        fadeIn = False,
        fadeOut = [],
        scoreChange = 0
      }

newtype InPlaceEffects = InPlaceEffects {unInPlaceEffects :: Map.Map CardSpot InPlaceEffect}
  deriving (Eq, Generic, Show)

instance Semigroup InPlaceEffects where
  InPlaceEffects m1 <> InPlaceEffects m2 = InPlaceEffects (Map.unionWith (<>) m1 m2)

instance Monoid InPlaceEffects where
  mempty = InPlaceEffects mempty

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
  ScoreType 'Core = Int
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

newtype HandIndex = HandIndex {unHandIndex :: Int}
  deriving (Eq, Show, Generic, Enum)

data StackKind
  = Stacked
  | Discarded

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

data PlayerSpot = PlayerBot | PlayerTop
  deriving (Bounded, Enum, Eq, Ord, Show, Generic)

allPlayersSpots :: [PlayerSpot]
allPlayersSpots = [minBound ..]

startingPlayerSpot :: PlayerSpot
startingPlayerSpot = PlayerBot

endingPlayerSpot :: PlayerSpot
endingPlayerSpot = PlayerTop

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

-- | The various kinds of neighbors
data Neighborhood
  = -- | Neighbors to the left and the right
    Cardinal
  | -- | Neighbors in diagonals
    Diagonal
  | -- | Cardinal + diagnoal neighbors
    All
  deriving (Eq, Generic, Show)

neighbors :: Neighborhood -> CardSpot -> [CardSpot]
neighbors All pSpot = neighbors Diagonal pSpot ++ neighbors Cardinal pSpot
neighbors Cardinal pSpot =
  case pSpot of
    TopLeft -> [Top, BottomLeft]
    Top -> [TopLeft, TopRight, Bottom]
    TopRight -> [Top, BottomRight]
    BottomLeft -> [TopLeft, Bottom]
    Bottom -> [BottomLeft, Top, BottomRight]
    BottomRight -> [Bottom, TopRight]
neighbors Diagonal pSpot =
  case pSpot of
    TopLeft -> [Bottom]
    Top -> [BottomLeft, BottomRight]
    TopRight -> [Bottom]
    BottomLeft -> [Top]
    Bottom -> [TopLeft, TopRight]
    BottomRight -> [Top]

addToDiscarded :: Board 'Core -> PlayerSpot -> DiscardedType 'Core -> Board 'Core
addToDiscarded board pSpot addition =
  setDiscarded board pSpot $ discarded ++ addition
  where
    discarded = toDiscarded board pSpot

addToHand :: Board p -> PlayerSpot -> HandElemType p -> Board p
addToHand board pSpot handElem =
  setPart board pSpot $ part {inHand = hand'}
  where
    part = toPart board pSpot
    hand = inHand part
    hand' = snoc hand handElem

-- TODO @smelc Use this function more often

-- | Map 'f' over creatures in the given 'CardSpot' in the tiven 'PlayerSpot'
mapInPlace :: (Creature 'Core -> Creature 'Core) -> PlayerSpot -> [CardSpot] -> Board 'Core -> Board 'Core
mapInPlace f pSpot cSpots board =
  setPart board pSpot (part {inPlace = inPlace'})
  where
    part@PlayerPart {inPlace} = Board.toPart board pSpot
    (changed, untouched) = Map.partitionWithKey (\k _ -> k `elem` cSpots) inPlace
    inPlace' = Map.union (Map.map f changed) untouched

setCreature :: Board 'Core -> PlayerSpot -> CardSpot -> Creature 'Core -> Board 'Core
setCreature board pSpot cSpot creature =
  setPart board pSpot part'
  where
    part = toPart board pSpot
    part' = part & #inPlace . at cSpot ?~ creature

setDiscarded :: Board p -> PlayerSpot -> DiscardedType p -> Board p
setDiscarded board pSpot discarded =
  setPart board pSpot $ part {discarded = discarded}
  where
    part = toPart board pSpot

setInPlace :: Board p -> PlayerSpot -> InPlaceType p -> Board p
setInPlace board pSpot inPlace =
  setPart board pSpot $ part {inPlace = inPlace}
  where
    part = toPart board pSpot

setHand :: Board p -> PlayerSpot -> InHandType p -> Board p
setHand board pSpot hand =
  setPart board pSpot $ part {inHand = hand}
  where
    part = toPart board pSpot

setMana :: Board.ManaType p -> PlayerSpot -> Board p -> Board p
setMana mana pSpot board =
  setPart board pSpot $ part {Board.mana = mana}
  where
    part = toPart board pSpot

setPart :: Board p -> PlayerSpot -> PlayerPart p -> Board p
setPart board PlayerTop part = board {playerTop = part}
setPart board PlayerBot part = board {playerBottom = part}

setStack :: Board p -> PlayerSpot -> StackType p -> Board p
setStack board pSpot stack =
  setPart board pSpot $ part {stack = stack}
  where
    part = toPart board pSpot

toHoleyInPlace :: Board 'Core -> [(PlayerSpot, CardSpot, Maybe (Creature 'Core))]
toHoleyInPlace board =
  [ (pSpot, cSpot, maybeCreature)
    | pSpot <- allPlayersSpots,
      cSpot <- allCardsSpots,
      let maybeCreature = toInPlaceCreature board pSpot cSpot
  ]

toPlayerCardSpots :: Board 'Core -> PlayerSpot -> CardTargetKind -> [CardSpot]
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
  Board 'Core -> PlayerSpot -> [(CardSpot, Maybe (Creature 'Core))]
toPlayerHoleyInPlace board pSpot =
  [ (cSpot, maybeCreature)
    | cSpot <- allCardsSpots,
      let maybeCreature = toInPlaceCreature board pSpot cSpot
  ]

toDiscarded :: Board p -> PlayerSpot -> DiscardedType p
toDiscarded Board {playerTop} PlayerTop = discarded playerTop
toDiscarded Board {playerBottom} PlayerBot = discarded playerBottom

toHand :: Board p -> PlayerSpot -> InHandType p
toHand Board {playerTop} PlayerTop = inHand playerTop
toHand Board {playerBottom} PlayerBot = inHand playerBottom

toInPlace :: Board p -> PlayerSpot -> InPlaceType p
toInPlace Board {playerTop} PlayerTop = inPlace playerTop
toInPlace Board {playerBottom} PlayerBot = inPlace playerBottom

-- | The neighbors of the card at the given spot, for the given player,
-- and for the given kind of neighbors.
toNeighbors ::
  Board 'Core ->
  PlayerSpot ->
  CardSpot ->
  Neighborhood ->
  [(CardSpot, Creature 'Core)]
toNeighbors board pSpot cSpot neighborhood =
  [ (cSpot, maybeCreature)
    | cSpot <- neighbors neighborhood cSpot,
      let maybeCreature = toInPlaceCreature board pSpot cSpot
  ]
    & mapMaybe liftJust
  where
    liftJust (f, Just s) = Just (f, s)
    liftJust _ = Nothing

toPart :: Board p -> PlayerSpot -> PlayerPart p
toPart Board {playerTop} PlayerTop = playerTop
toPart Board {playerBottom} PlayerBot = playerBottom

toScore :: Board p -> PlayerSpot -> ScoreType p
toScore board pSpot = toPart board pSpot & score

toStack :: Board p -> PlayerSpot -> StackType p
toStack Board {playerTop} PlayerTop = stack playerTop
toStack Board {playerBottom} PlayerBot = stack playerBottom

toInPlaceCreature ::
  Board 'Core ->
  PlayerSpot ->
  CardSpot ->
  Maybe (Creature 'Core)
toInPlaceCreature board pSpot cSpot = inPlace Map.!? cSpot
  where
    inPlace = toInPlace board pSpot

empty :: Teams Team -> Board 'Core
empty Teams {topTeam, botTeam} =
  Board {playerTop = emptyPart topTeam, playerBottom = emptyPart botTeam}

emptyPart :: Team -> PlayerPart 'Core
emptyPart team = PlayerPart {..}
  where
    inPlace = Map.empty
    inHand = []
    mana = initialMana
    score = 0
    stack = []
    discarded = []

data Teams a = Teams
  { topTeam :: a,
    botTeam :: a
  }
  deriving (Generic, Show, Functor)

toData :: PlayerSpot -> Teams a -> a
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
    part team smodel deck = (smodel', (emptyPart team) {inHand = hand', stack = stack'})
      where
        (smodel', deck') = SharedModel.shuffle smodel deck
        (hand, stack) = splitAt initialHandSize deck'
        hand' = map cardToIdentifier hand
        stack' = map cardToIdentifier stack
    (smodel', topPart) = part topTeam shared topDeck
    (smodel'', botPart) = part botTeam smodel' botDeck

-- | A board with a single creature in place. Hands are empty. Handy for
-- debugging for example.
small :: SharedModel -> Teams Team -> CreatureID -> [Item] -> PlayerSpot -> CardSpot -> Board 'Core
small shared teams cid items pSpot cSpot =
  setCreature (empty teams) pSpot cSpot c
  where
    c =
      SharedModel.idToCreature shared cid items
        & fromJust
        & Card.unlift

-- Whether a spot is in the back line
inTheBack :: CardSpot -> Bool
inTheBack TopLeft = True
inTheBack Top = True
inTheBack TopRight = True
inTheBack _ = False

-- | Given a frontline spot, the corresponding backline spot. Given
-- a backline spot, the corresponding frontline spot.
switchLine :: CardSpot -> CardSpot
switchLine TopLeft = BottomLeft
switchLine Top = Bottom
switchLine TopRight = BottomRight
switchLine BottomLeft = TopLeft
switchLine Bottom = Top
switchLine BottomRight = TopRight

botSpots :: [CardSpot]
botSpots = filter (not . inTheBack) allCardsSpots

topSpots :: [CardSpot]
topSpots = filter inTheBack allCardsSpots

-- | The other spot
otherPlayerSpot :: PlayerSpot -> PlayerSpot
otherPlayerSpot PlayerBot = PlayerTop
otherPlayerSpot PlayerTop = PlayerBot

spotToLens :: PlayerSpot -> Lens' (Board p) (PlayerPart p)
spotToLens =
  \case
    PlayerBot -> #playerBottom
    PlayerTop -> #playerTop

appliesTo :: Card.ID -> Board 'Core -> PlayerSpot -> CardSpot -> Bool
appliesTo id board pSpot cSpot =
  case (Card.targetType id, toInPlaceCreature board pSpot cSpot) of
    (Card.CardTargetType Occupied, Just _) -> True
    (Card.CardTargetType Hole, Nothing) -> True
    (Card.PlayerTargetType, Nothing) -> True
    _ -> False

------------------------------
-- Now onto fancy rendering --
------------------------------

toASCII :: Board 'Core -> String
toASCII board =
  (intersperse "\n" lines & concat) ++ "\n"
  where
    lines =
      stackLines board PlayerTop ++ []
        ++ [handLine board PlayerTop]
        ++ [scoreLine board PlayerTop]
        ++ cardsLines board PlayerTop topSpots
        ++ ["\n"] -- vertical space between AI lines
        ++ cardsLines board PlayerTop botSpots
        ++ ["\n", "\n"] -- vertical space between players
        ++ cardsLines board PlayerBot botSpots
        ++ ["\n"] -- vertical space between player lines
        ++ cardsLines board PlayerBot topSpots
        ++ []
        ++ [scoreLine board PlayerBot]
        ++ [handLine board PlayerBot]
        ++ stackLines board PlayerBot

stackLines :: Board 'Core -> PlayerSpot -> [String]
stackLines board pSpot =
  map (\s -> replicate 4 ' ' ++ s) $ reverse $ go 0 []
  where
    discarded = toDiscarded board pSpot
    stack = toStack board pSpot
    hspace = replicate 8 ' '
    stackWidth = 16
    justify s | length s < stackWidth = s ++ replicate (stackWidth - length s) ' '
    justify s | length s > stackWidth = take stackWidth s
    justify s = s
    go i acc =
      case (stackLine Discarded discarded i, stackLine Stacked stack i) of
        (Nothing, Nothing) -> acc
        (d, st) ->
          let line =
                justify (fromMaybe blanks d)
                  ++ hspace
                  ++ justify (fromMaybe blanks st)
           in go (i + 1) (line : acc)
          where
            blanks = replicate stackWidth ' '

handLine :: Board 'Core -> PlayerSpot -> String
handLine board pSpot =
  "Hand: " ++ intercalate ", " (map showID hand)
  where
    hand = toHand board pSpot

scoreLine :: Board 'Core -> PlayerSpot -> String
scoreLine board pSpot =
  replicate cardWidth ' ' ++ " Score: " ++ show (toScore board pSpot)

stackLine :: StackKind -> [Card.ID] -> LineNumber -> Maybe String
stackLine Discarded _ 0 = Just "Discarded"
stackLine Stacked _ 0 = Just "Stack"
stackLine _ cards i | i > length cards = Nothing
stackLine _ cards i = Just $ showID $ cards !! (i - 1)

showID :: Card.ID -> String
showID (IDC CreatureID {creatureKind, team} _) =
  showTeamShort team ++ " " ++ show creatureKind
showID (IDI i) = show i
showID (IDN n) = show n

showTeamShort :: Team -> String
showTeamShort = \case
  Evil -> "E"
  Human -> "H"
  Undead -> "UD"
  ZKnights -> "Z"

cardsLines :: Board 'Core -> PlayerSpot -> [CardSpot] -> [String]
cardsLines board pSpot cSpots =
  map f [0 .. cardHeight - 1]
  where
    f lineNb =
      let pieces = map (\cSpot -> cardLine board pSpot cSpot lineNb) cSpots
       in intersperse " " pieces & concat -- horizontal space between cards

-- The height of a card, in number of lines
cardHeight :: Int
cardHeight = 1 + 1 + 5

-- The width of a card, in number of characters
cardWidth :: Int
cardWidth = 16

type LineNumber = Int

-- | The line number must be in [0, cardHeight)
cardLine :: Board 'Core -> PlayerSpot -> CardSpot -> LineNumber -> String
cardLine board pSpot cSpot lineNb =
  case length base of
    i | i < cardWidth -> base ++ replicate (cardWidth - i) '.'
    i | i > cardWidth -> take cardWidth base
    _ -> base
  where
    maybeCreature = toInPlace board pSpot Map.!? cSpot
    emptyLine :: String = replicate cardWidth '.'
    base = case maybeCreature of
      Nothing -> if lineNb == 0 then show cSpot else emptyLine
      Just creature -> fromMaybe emptyLine $ creatureToAscii part creature lineNb
    part = Board.toInPlace board pSpot & Map.elems & Just

-- | The n-th line of a creature card, or None
creatureToAscii :: Maybe Total.Part -> Creature 'Core -> LineNumber -> Maybe String
creatureToAscii _ (Creature {creatureId = CreatureID {..}}) 0 =
  Just $ show team ++ " " ++ show creatureKind
creatureToAscii part (c@Creature {..}) 1 =
  Just $ show hp ++ "<3 " ++ show (Total.attack part c) ++ "X"
creatureToAscii _ _ _ = Nothing
