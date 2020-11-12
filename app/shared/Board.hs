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
    boardToInHandCreaturesToDraw,
    boardToInPlaceCreature,
    boardToHand,
    boardToPart,
    Board (..),
    CardSpot (..),
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
    startingPlayerSpot,
    spotToLens,
    boardToStack,
    boardSetStack,
    boardSetHand,
    boardAddToHand,
    emptyBoard,
    boardSetCreature,
    boardToDiscarded,
    boardSetDiscarded,
    boardSetInPlace,
    boardToInPlace,
    topSpots,
    botSpots,
    boardToASCII,
  )
where

import Card
import Constants
import Control.Lens
import Control.Monad.Except (MonadError, throwError)
import Data.Generics.Labels ()
import Data.Kind (Constraint, Type)
import Data.List (intersperse)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Formatting (hex, sformat, (%))
import GHC.Generics (Generic)
import SharedModel

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

-- It is a bit unfortunate to have these types defined here
-- as they are UI only. However we need them to define the InPlaceType family

-- | Initially this type was for displaying animations only. However
-- Game.hs also uses for Core stuff internally. Unfortunate :-(
-- So be careful when changing related code.
data InPlaceEffect = InPlaceEffect
  { -- | Creature dies
    death :: Bool,
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
  InPlaceEffect {death = d1, attackBump = ab1, hitPointsChange = hp1, fadeIn = f1, scoreChange = c1}
    <> InPlaceEffect {death = d2, attackBump = ab2, hitPointsChange = hp2, fadeIn = f2, scoreChange = c2} =
      InPlaceEffect
        { death = d1 || d2,
          attackBump = ab1 || ab2,
          hitPointsChange = hp1 + hp2,
          fadeIn = f1 || f2,
          scoreChange = c1 + c2
        }

instance Monoid InPlaceEffect where
  mempty =
    InPlaceEffect
      { death = False,
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
  HandElemType Core = Card Core
  HandElemType UI = Int

type family InHandType (p :: Phase) where
  InHandType p = [HandElemType p]

type family ScoreType (p :: Phase) where
  ScoreType Core = Int
  ScoreType UI = ()

type family StackType (p :: Phase) where
  StackType Core = [CardIdentifier]
  StackType UI = Int -- Discarded->Stack transfer

type family DiscardedType (p :: Phase) where
  DiscardedType Core = [CardIdentifier]
  DiscardedType UI = Int -- Discarded->Stack transfer

type Forall (c :: Type -> Constraint) (p :: Phase) =
  ( c (HandElemType p),
    c (InPlaceType p),
    c (InHandType p),
    c (ScoreType p),
    c (StackType p),
    c (DiscardedType p)
  )

data PlayerPart (p :: Phase) = PlayerPart
  { -- | Cards on the board
    inPlace :: InPlaceType p,
    -- | Cards in hand
    inHand :: InHandType p,
    -- | The score of this player
    score :: ScoreType p,
    stack :: StackType p,
    discarded :: DiscardedType p
  }
  deriving (Generic)

deriving instance Board.Forall Eq p => Eq (PlayerPart p)

deriving instance Board.Forall Show p => Show (PlayerPart p)

instance Semigroup (PlayerPart UI) where
  PlayerPart inPlace1 inHand1 () s1 d1 <> PlayerPart inPlace2 inHand2 () s2 d2 =
    PlayerPart (inPlace1 <> inPlace2) (inHand1 <> inHand2) () (s1 + s2) (d1 + d2)

instance Monoid (PlayerPart UI) where
  mempty = PlayerPart mempty mempty mempty 0 0

newtype HandIndex = HandIndex {unHandIndex :: Int}
  deriving (Eq, Show, Generic, Enum)

lookupHand ::
  MonadError Text m =>
  [a] ->
  Int ->
  m a
lookupHand hand i
  | i < 0 = throwError $ sformat ("Invalid hand index: " % hex) i
  | i >= handLength =
    throwError $
      sformat
        ("Invalid hand index: " % hex % ". Hand has " % hex % " card(s).")
        i
        handLength
  | otherwise = return $ hand !! i
  where
    handLength = length hand

data PlayerSpot = PlayerBottom | PlayerTop
  deriving (Enum, Eq, Ord, Show, Generic)

allPlayersSpots :: [PlayerSpot]
allPlayersSpots = [PlayerBottom ..]

startingPlayerSpot :: PlayerSpot
startingPlayerSpot = PlayerBottom

endingPlayerSpot :: PlayerSpot
endingPlayerSpot = PlayerTop

data Board (p :: Phase) = Board
  { playerTop :: PlayerPart p,
    playerBottom :: PlayerPart p
  }
  deriving (Generic)

deriving instance Board.Forall Eq p => Eq (Board p)

deriving instance Board.Forall Show p => Show (Board p)

instance Semigroup (Board UI) where
  Board top1 bot1 <> Board top2 bot2 =
    Board (top1 <> top2) (bot1 <> bot2)

instance Monoid (Board UI) where
  mempty = Board mempty mempty

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
boardSetPart board PlayerBottom part = board {playerBottom = part}

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

boardToInHandCreaturesToDraw :: Board Core -> Lens' (Board Core) (PlayerPart Core) -> [Creature Core]
boardToInHandCreaturesToDraw board player =
  board ^.. player . #inHand . folded . #_CreatureCard

boardToDiscarded :: Board p -> PlayerSpot -> DiscardedType p
boardToDiscarded Board {playerTop} PlayerTop = discarded playerTop
boardToDiscarded Board {playerBottom} PlayerBottom = discarded playerBottom

boardToHand :: Board p -> PlayerSpot -> InHandType p
boardToHand Board {playerTop} PlayerTop = inHand playerTop
boardToHand Board {playerBottom} PlayerBottom = inHand playerBottom

boardToInPlace :: Board p -> PlayerSpot -> InPlaceType p
boardToInPlace Board {playerTop} PlayerTop = inPlace playerTop
boardToInPlace Board {playerBottom} PlayerBottom = inPlace playerBottom

boardToPart :: Board p -> PlayerSpot -> PlayerPart p
boardToPart Board {playerTop} PlayerTop = playerTop
boardToPart Board {playerBottom} PlayerBottom = playerBottom

boardToStack :: Board p -> PlayerSpot -> StackType p
boardToStack Board {playerTop} PlayerTop = stack playerTop
boardToStack Board {playerBottom} PlayerBottom = stack playerBottom

boardToInPlaceCreature ::
  Board Core ->
  PlayerSpot ->
  CardSpot ->
  Maybe (Creature Core)
boardToInPlaceCreature board pSpot cSpot = inPlace Map.!? cSpot
  where
    inPlace = boardToInPlace board pSpot

emptyBoard :: Board Core
emptyBoard =
  Board {playerTop = emptyPlayerPart, playerBottom = emptyPlayerPart}

emptyPlayerPart :: PlayerPart Core
emptyPlayerPart = PlayerPart {..}
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

initialBoard :: SharedModel -> Teams -> (SharedModel, Board Core)
initialBoard shared@SharedModel {sharedCards = cards} Teams {..} =
  (smodel'', Board topPart botPart)
  where
    part team smodel = (smodel', PlayerPart Map.empty hand 0 stack' [])
      where
        deck = teamDeck cards team
        (smodel', deck') = shuffle smodel deck
        (hand, stack) = splitAt initialHandSize deck'
        stack' = map cardToIdentifier stack
    (smodel', topPart) = part topTeam shared
    (smodel'', botPart) = part botTeam smodel'

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
otherPlayerSpot PlayerBottom = PlayerTop
otherPlayerSpot PlayerTop = PlayerBottom

spotToLens :: PlayerSpot -> Lens' (Board p) (PlayerPart p)
spotToLens =
  \case
    PlayerBottom -> #playerBottom
    PlayerTop -> #playerTop

------------------------------
-- Now onto fancy rendering --
------------------------------

boardToASCII :: Board Core -> String
boardToASCII board =
  (intersperse "\n" lines & concat) ++ "\n"
  where
    lines =
      cardsLines board PlayerTop topSpots
        ++ ["\n"] -- vertical space between AI lines
        ++ cardsLines board PlayerTop botSpots
        ++ ["\n", "\n"] -- vertical space between players
        ++ cardsLines board PlayerBottom botSpots
        ++ ["\n"] -- vertical space between player lines
        ++ cardsLines board PlayerBottom topSpots

cardsLines :: Board Core -> PlayerSpot -> [CardSpot] -> [String]
cardsLines board pSpot cSpots =
  map f [0 .. cardHeight - 1]
  where
    f lineNb =
      let pieces = map (\cSpot -> cardLine board pSpot cSpot lineNb) cSpots
       in intersperse " " pieces & concat -- horizontal space between cards

-- The height of a card, in number of lines
cardHeight = 1 + 1 + 5

-- The height of a card, in number of characters
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
creatureToAscii Creature {..} 1 =
  Just $ show hp ++ "<3 " ++ show attack ++ "X"
creatureToAscii _ _ = Nothing
