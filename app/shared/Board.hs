{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
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
    boardSetPart,
    boardToHoleyInPlace,
    boardToInPlaceCreature,
    boardToHand,
    boardToPart,
    Board (..),
    CardSpot (..),
    DeathCause (..),
    endingPlayerSpot,
    initialBoard,
    HandIndex (..),
    inTheBack,
    InHandType (..),
    lookupHand,
    otherPlayerSpot,
    PlayerPart (..),
    PlayerSpot (..),
    StackType (..),
    smallBoard,
    startingPlayerSpot,
    spotToLens,
    boardToStack,
    boardSetStack,
    boardSetHand,
    boardAddToHand,
    boardAddToDiscarded,
    emptyBoard,
    boardSetCreature,
    boardToDiscarded,
    boardSetDiscarded,
    boardSetInPlace,
    boardToInPlace,
    topSpots,
    botSpots,
    boardToASCII,
    StackKind (..),
    Board.appliesTo,
    boardToScore,
    neighbors,
    Neighborhood (..),
    boardToPlayerHoleyInPlace,
    boardToNeighbors,
    boardToPlayerCardSpots,
    isDead,
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
import Data.Maybe (fromJust, fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import GHC.Generics (Generic)
import SharedModel (SharedModel, idToCreature)
import qualified SharedModel
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
  deriving (Enum, Eq, Ord, Show, Generic)

allCardsSpots :: [CardSpot]
allCardsSpots = [TopLeft ..]

type CardsOnTable = Map.Map CardSpot (Creature Core)

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
  UsualDeath <> _ = UsualDeath
  NoDeath <> d = d

instance Monoid DeathCause where
  mempty = NoDeath

-- It is a bit unfortunate to have these types defined here
-- as they are UI only. However we need them to define the InPlaceType family

-- | Initially this type was for displaying animations only. However
-- Game.hs also uses for Core stuff internally. Unfortunate :-(
-- So be careful when changing related code.
data InPlaceEffect = InPlaceEffect
  { -- | Attack value changed
    attackChange :: Int,
    -- | Did creature die? If yes, for what reason
    death :: DeathCause,
    -- | Creature attacked (value used solely for animations)
    attackBump :: Bool,
    -- | Hits points changed
    hitPointsChange :: Int,
    -- | Fade-in card
    fadeIn :: Bool,
    -- | Score changed
    scoreChange :: Int
  }
  deriving (Eq, Generic, Show)

instance Semigroup InPlaceEffect where
  InPlaceEffect {attackChange = ac1, death = d1, attackBump = ab1, hitPointsChange = hp1, fadeIn = f1, scoreChange = c1}
    <> InPlaceEffect {attackChange = ac2, death = d2, attackBump = ab2, hitPointsChange = hp2, fadeIn = f2, scoreChange = c2} =
      InPlaceEffect
        { attackChange = ac1 + ac2,
          death = d1 <> d2,
          attackBump = ab1 || ab2,
          hitPointsChange = hp1 + hp2,
          fadeIn = f1 || f2,
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
        scoreChange = 0
      }

newtype InPlaceEffects = InPlaceEffects {unInPlaceEffects :: Map.Map CardSpot InPlaceEffect}
  deriving (Eq, Generic, Show)

instance Semigroup InPlaceEffects where
  InPlaceEffects m1 <> InPlaceEffects m2 = InPlaceEffects (Map.unionWith (<>) m1 m2)

instance Monoid InPlaceEffects where
  mempty = InPlaceEffects mempty

type family InPlaceType (p :: Phase) where
  InPlaceType Core = CardsOnTable
  InPlaceType UI = InPlaceEffects

type family HandElemType (p :: Phase) where
  HandElemType Core = Card.ID
  HandElemType UI = Int

type family InHandType (p :: Phase) where
  InHandType p = [HandElemType p]

type family ScoreType (p :: Phase) where
  ScoreType Core = Int
  ScoreType UI = ()

type family StackType (p :: Phase) where
  StackType Core = [Card.ID]
  StackType UI = Int -- Discarded->Stack transfer

type family DiscardedType (p :: Phase) where
  DiscardedType Core = [Card.ID]
  DiscardedType UI = Int -- Board->Discarded transfer

type family TeamType (p :: Phase) where
  TeamType 'Core = Team
  TeamType 'UI = ()

type Forall (c :: Type -> Constraint) (p :: Phase) =
  ( c (HandElemType p),
    c (InPlaceType p),
    c (InHandType p),
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

instance Semigroup (PlayerPart UI) where
  PlayerPart inPlace1 inHand1 () s1 d1 () <> PlayerPart inPlace2 inHand2 () s2 d2 () =
    PlayerPart (inPlace1 <> inPlace2) (inHand1 <> inHand2) () (s1 + s2) (d1 + d2) ()

instance Monoid (PlayerPart UI) where
  mempty = PlayerPart mempty mempty mempty 0 0 mempty

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
  deriving (Enum, Eq, Ord, Show, Generic)

allPlayersSpots :: [PlayerSpot]
allPlayersSpots = [PlayerBot ..]

startingPlayerSpot :: PlayerSpot
startingPlayerSpot = PlayerBot

endingPlayerSpot :: PlayerSpot
endingPlayerSpot = PlayerTop

data Board (p :: Phase) = Board
  { playerTop :: PlayerPart p,
    playerBottom :: PlayerPart p
  }
  deriving (Generic)

deriving instance Board.Forall Eq p => Eq (Board p)

deriving instance Board.Forall Ord p => Ord (Board p)

deriving instance Board.Forall Show p => Show (Board p)

instance Semigroup (Board UI) where
  Board top1 bot1 <> Board top2 bot2 =
    Board (top1 <> top2) (bot1 <> bot2)

instance Monoid (Board UI) where
  mempty = Board mempty mempty

data Neighborhood = Cardinal | Diagonal | All
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

boardAddToDiscarded :: Board 'Core -> PlayerSpot -> DiscardedType 'Core -> Board 'Core
boardAddToDiscarded board pSpot addition =
  boardSetDiscarded board pSpot $ discarded ++ addition
  where
    discarded = boardToDiscarded board pSpot

boardAddToHand :: Board p -> PlayerSpot -> HandElemType p -> Board p
boardAddToHand board pSpot handElem =
  boardSetPart board pSpot $ part {inHand = hand'}
  where
    part = boardToPart board pSpot
    hand = inHand part
    hand' = snoc hand handElem

boardSetCreature :: Board Core -> PlayerSpot -> CardSpot -> Creature Core -> Board Core
boardSetCreature board pSpot cSpot creature =
  boardSetPart board pSpot part'
  where
    part = boardToPart board pSpot
    part' = part & #inPlace . at cSpot ?~ creature

boardSetDiscarded :: Board p -> PlayerSpot -> DiscardedType p -> Board p
boardSetDiscarded board pSpot discarded =
  boardSetPart board pSpot $ part {discarded = discarded}
  where
    part = boardToPart board pSpot

boardSetInPlace :: Board p -> PlayerSpot -> InPlaceType p -> Board p
boardSetInPlace board pSpot inPlace =
  boardSetPart board pSpot $ part {inPlace = inPlace}
  where
    part = boardToPart board pSpot

boardSetHand :: Board p -> PlayerSpot -> InHandType p -> Board p
boardSetHand board pSpot hand =
  boardSetPart board pSpot $ part {inHand = hand}
  where
    part = boardToPart board pSpot

boardSetPart :: Board p -> PlayerSpot -> PlayerPart p -> Board p
boardSetPart board PlayerTop part = board {playerTop = part}
boardSetPart board PlayerBot part = board {playerBottom = part}

boardSetStack :: Board p -> PlayerSpot -> StackType p -> Board p
boardSetStack board pSpot stack =
  boardSetPart board pSpot $ part {stack = stack}
  where
    part = boardToPart board pSpot

boardToHoleyInPlace :: Board Core -> [(PlayerSpot, CardSpot, Maybe (Creature Core))]
boardToHoleyInPlace board =
  [ (pSpot, cSpot, maybeCreature)
    | pSpot <- allPlayersSpots,
      cSpot <- allCardsSpots,
      let maybeCreature = boardToInPlaceCreature board pSpot cSpot
  ]

boardToPlayerCardSpots :: Board 'Core -> PlayerSpot -> CardTargetKind -> [CardSpot]
boardToPlayerCardSpots board pSpot ctk =
  boardToPlayerHoleyInPlace board pSpot
    & filter
      ( \(_, maybeCreature) ->
          case (ctk, maybeCreature) of
            (Hole, Nothing) -> True
            (Occupied, Just _) -> True
            _ -> False
      )
    & map fst

boardToPlayerHoleyInPlace ::
  Board Core -> PlayerSpot -> [(CardSpot, Maybe (Creature Core))]
boardToPlayerHoleyInPlace board pSpot =
  [ (cSpot, maybeCreature)
    | cSpot <- allCardsSpots,
      let maybeCreature = boardToInPlaceCreature board pSpot cSpot
  ]

boardToDiscarded :: Board p -> PlayerSpot -> DiscardedType p
boardToDiscarded Board {playerTop} PlayerTop = discarded playerTop
boardToDiscarded Board {playerBottom} PlayerBot = discarded playerBottom

boardToHand :: Board p -> PlayerSpot -> InHandType p
boardToHand Board {playerTop} PlayerTop = inHand playerTop
boardToHand Board {playerBottom} PlayerBot = inHand playerBottom

boardToInPlace :: Board p -> PlayerSpot -> InPlaceType p
boardToInPlace Board {playerTop} PlayerTop = inPlace playerTop
boardToInPlace Board {playerBottom} PlayerBot = inPlace playerBottom

-- | The neighbors of the card at the given spot, for the given player,
-- and for the given kind of neighbors.
boardToNeighbors ::
  Board 'Core ->
  PlayerSpot ->
  CardSpot ->
  Neighborhood ->
  [(CardSpot, Maybe (Creature 'Core))]
boardToNeighbors board pSpot cSpot neighborhood =
  [ (cSpot, maybeCreature)
    | cSpot <- neighbors neighborhood cSpot,
      let maybeCreature = boardToInPlaceCreature board pSpot cSpot
  ]

boardToPart :: Board p -> PlayerSpot -> PlayerPart p
boardToPart Board {playerTop} PlayerTop = playerTop
boardToPart Board {playerBottom} PlayerBot = playerBottom

boardToScore :: Board p -> PlayerSpot -> ScoreType p
boardToScore board pSpot = boardToPart board pSpot & score

boardToStack :: Board p -> PlayerSpot -> StackType p
boardToStack Board {playerTop} PlayerTop = stack playerTop
boardToStack Board {playerBottom} PlayerBot = stack playerBottom

boardToInPlaceCreature ::
  Board Core ->
  PlayerSpot ->
  CardSpot ->
  Maybe (Creature Core)
boardToInPlaceCreature board pSpot cSpot = inPlace Map.!? cSpot
  where
    inPlace = boardToInPlace board pSpot

emptyBoard :: Teams -> Board Core
emptyBoard Teams {topTeam, botTeam} =
  Board {playerTop = emptyPlayerPart topTeam, playerBottom = emptyPlayerPart botTeam}

emptyPlayerPart :: Team -> PlayerPart Core
emptyPlayerPart team = PlayerPart {..}
  where
    inPlace = Map.empty
    inHand = []
    score = 0
    stack = []
    discarded = []

data Teams = Teams
  { topTeam :: Team,
    botTeam :: Team
  }
  deriving (Generic, Show)

-- | The initial board, appropriately shuffled with 'SharedModel' rng
initialBoard :: SharedModel -> Teams -> (SharedModel, Board 'Core)
initialBoard shared Teams {..} =
  (smodel'', Board topPart botPart)
  where
    part team smodel = (smodel', PlayerPart Map.empty hand' 0 stack' [] team)
      where
        deck = teamDeck (SharedModel.getCards shared) team
        (smodel', deck') = SharedModel.shuffle smodel deck
        (hand, stack) = splitAt initialHandSize deck'
        hand' = map cardToIdentifier hand
        stack' = map cardToIdentifier stack
    (smodel', topPart) = part topTeam shared
    (smodel'', botPart) = part botTeam smodel'

-- | A board with a single creature in place. Hands are empty. Handy for
-- debugging for example.
smallBoard :: SharedModel -> Teams -> CreatureID -> [Item] -> PlayerSpot -> CardSpot -> Board 'Core
smallBoard shared teams cid items pSpot cSpot =
  boardSetCreature (emptyBoard teams) pSpot cSpot c
  where
    c =
      SharedModel.idToCreature shared cid []
        & fromJust
        & Card.unliftCreature

-- Whether a spot is in the back line
inTheBack :: CardSpot -> Bool
inTheBack TopLeft = True
inTheBack Top = True
inTheBack TopRight = True
inTheBack _ = False

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

appliesTo :: Card.ID -> Board Core -> PlayerSpot -> CardSpot -> Bool
appliesTo id board pSpot cSpot =
  case (Card.targetType id, boardToInPlaceCreature board pSpot cSpot) of
    (Card.CardTargetType Occupied, Just _) -> True
    (Card.CardTargetType Hole, Nothing) -> True
    (Card.PlayerTargetType, Nothing) -> True
    _ -> False

------------------------------
-- Now onto fancy rendering --
------------------------------

boardToASCII :: Board Core -> String
boardToASCII board =
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

stackLines :: Board Core -> PlayerSpot -> [String]
stackLines board pSpot =
  map (\s -> replicate 4 ' ' ++ s) $ reverse $ go 0 []
  where
    discarded = boardToDiscarded board pSpot
    stack = boardToStack board pSpot
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

handLine :: Board Core -> PlayerSpot -> String
handLine board pSpot =
  "Hand: " ++ intercalate ", " (map showID hand)
  where
    hand = boardToHand board pSpot

scoreLine :: Board 'Core -> PlayerSpot -> String
scoreLine board pSpot =
  replicate cardWidth ' ' ++ " Score: " ++ show (boardToScore board pSpot)

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

showTeamShort Human = "H"
showTeamShort Undead = "UD"

cardsLines :: Board Core -> PlayerSpot -> [CardSpot] -> [String]
cardsLines board pSpot cSpots =
  map f [0 .. cardHeight - 1]
  where
    f lineNb =
      let pieces = map (\cSpot -> cardLine board pSpot cSpot lineNb) cSpots
       in intersperse " " pieces & concat -- horizontal space between cards

-- The height of a card, in number of lines
cardHeight = 1 + 1 + 5

-- The width of a card, in number of characters
cardWidth = 16

type LineNumber = Int

-- | The line number must be in [0, cardHeight)
cardLine :: Board Core -> PlayerSpot -> CardSpot -> LineNumber -> String
cardLine board pSpot cSpot lineNb =
  case length base of
    i | i < cardWidth -> base ++ replicate (cardWidth - i) '.'
    i | i > cardWidth -> take cardWidth base
    _ -> base
  where
    maybeCreature = boardToInPlace board pSpot Map.!? cSpot
    emptyLine :: String = replicate cardWidth '.'
    base = case maybeCreature of
      Nothing -> if lineNb == 0 then show cSpot else emptyLine
      Just creature -> fromMaybe emptyLine $ creatureToAscii creature lineNb

-- | The n-th line of a creature card, or None
creatureToAscii :: Creature Core -> LineNumber -> Maybe String
creatureToAscii Creature {creatureId = CreatureID {..}} 0 =
  Just $ show team ++ " " ++ show creatureKind
creatureToAscii c@Creature {..} 1 =
  Just $ show hp ++ "<3 " ++ show (Total.attack c) ++ "X"
creatureToAscii _ _ = Nothing
