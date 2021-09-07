{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module LootView where

import Card
import qualified Constants
import qualified Data.Bifunctor
import Data.Function ((&))
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe
import Debug.Trace (traceShow)
import Miso
import Miso.String hiding (drop, length, map, take, zip)
import Model (LootModel (..), Picked (..))
import Nat
import PCWViewInternal (DisplayLocation (..), cardPositionStyle')
import qualified PCWViewInternal
import SharedModel (SharedModel)
import qualified SharedModel
import Update (Action)
import ViewInternal

-- |
-- Module to display the view reached after a successful end of 'GameView'.
-- This view sits between two games, where the player can pick new
-- cards to augment its deck.
-- |

-- | Top-level rendering function
view :: LootModel -> Styled (View Action)
view LootModel {..} = do
  rewards <- rewardsView ctxt zpppp rewards
  deck <- deckView ctxt zpppp deck
  return $
    div_
      [style_ $ bgStyle z]
      [ rewards,
        rewardsLegend,
        deck,
        deckLegend,
        div_
          [style_ $ "position" =: "absolute" <> "top" =: "0" <> "right" =: "0" <> "z-index" =: ms zpp]
          [img_ [src_ $ Constants.assetsPath "loot-top-sand.png"]]
      ]
  where
    (z, zpp, zpppp) = (0, z + 1, z + 2)
    bgStyle :: Int -> Map.Map MisoString MisoString
    bgStyle z =
      zpltwh z Relative 0 0 Constants.lobbiesPixelWidth Constants.boardPixelHeight
        <> "background-image" =: Constants.assetsUrl "loot-bot-forest.png"
    ctxt = Context {..}
    rewardsLegend =
      div_
        [ legendStyle
            ((rewardsViewTopMargin + rewardsViewHeight) + (Constants.cps `div` 4)) -- below rewards + margin
        ]
        [div_ [legendTextStyle] [text rewardsTextLegend]]
    rewardsTextLegend :: MisoString =
      ms $
        "Rewards: pick " ++ show nbRewards ++ if nbRewards > 1 then " of them" else ""
    deckLegend =
      div_
        [ legendStyle
            ((deckViewTopMargin + deckViewHeight) + (Constants.cps `div` 4)) -- below deck + margin
        ]
        [div_ [legendTextStyle] [text "Your deck"]]
    legendStyle top =
      style_ $
        zpltwh
          z
          Absolute
          0 -- left
          top
          Constants.lobbiesPixelWidth -- width
          Constants.cps -- height
    legendTextStyle =
      style_ $
        textStyle
          <> flexLineStyle -- Horizontal layouting
          <> "width" =: px Constants.lobbiesPixelWidth -- Needed to center horizontally
          <> "justify-content" =: "center" -- Center horizontally

-- | Context that is common to all calls to 'deckView'
data Context = Context
  { shared :: SharedModel,
    team :: Team
  }

rewardsViewTopMargin :: Int
rewardsViewTopMargin = Constants.cps * 8

rewardsViewHeight :: Int
rewardsViewHeight = Constants.cardPixelHeight

-- | The div showing the rewards. It's displayed above 'deckView'
rewardsView :: Context -> Int -> [(Card.ID, Model.Picked)] -> Styled (View Action)
rewardsView Context {shared, LootView.team} z cards = do
  cardViews :: [View Action] <- traverse divCard $ zip [0 ..] cards'
  return $
    div_
      [ style_ $
          zpltwh
            z
            Absolute
            viewLeft -- left margin
            rewardsViewTopMargin -- top margin
            viewWidth -- width
            rewardsViewHeight -- height
      ]
      cardViews
  where
    nbCards = length cards
    viewWidth =
      (nbCards * Constants.cardPixelWidth) -- Cards horizontally
        + ((max 0 (nbCards - 1)) * Constants.cps) -- 1 cell gap between cards
    viewLeft =
      Constants.cps
        * ( case nbCards of
              1 -> 9
              2 -> 7
              3 -> 5
              _ -> traceShow ("Unexpected number of rewards cards: " ++ show nbCards) 5
          )
    cards' :: [(Card 'Core, Model.Picked)] =
      map (Data.Bifunctor.first $ SharedModel.identToCard shared) cards
        & mapMaybe liftNothing
        & map (Data.Bifunctor.first unlift)
      where
        liftNothing = \case
          (Nothing, _) -> Nothing
          (Just x, y) -> Just (x, y)
    divCard :: (Nat, (Card 'Core, Model.Picked)) -> Styled (View Action)
    divCard (i, (card, picked)) = do
      inner <- PCWViewInternal.cardView LootLoc z shared team card mempty
      return $
        div_
          [ style_ $
              cardPositionStyle'
                (Constants.cps * x)
                (Constants.cps * y)
                <> "z-index" =: ms z
                <> ( case picked of
                       Model.Picked -> "filter" =: "brightness(50%)"
                       Model.NotPicked -> mempty
                   )
          ]
          [inner]
      where
        xCellsOffset = Constants.cardCellWidth + 1 -- card width + horizontal margin
        yCellsOffset = Constants.cardCellHeight + 1 -- card height + vertical margin
        x = (natToInt i `mod` deckCardsPerRow) * xCellsOffset
        y = (natToInt i `div` deckCardsPerRow) * yCellsOffset

deckViewTopMargin :: Int
deckViewTopMargin = Constants.cps * 14

deckViewHeight :: Int
deckViewHeight =
  (Constants.cardPixelHeight * 2)
    + Constants.cps -- 2 cards high + vertical gap
    + (Constants.cps `div` 2) -- Give a bit more space, so that we don't have
    -- a scrollbar if there are less than 8 stacks

deckCardsPerRow :: Int
deckCardsPerRow = 4

deckViewWidth :: Int
deckViewWidth =
  (Constants.cardPixelWidth * deckCardsPerRow) -- Cards
    + (max 0 (deckCardsPerRow - 1)) * Constants.cps -- Gaps between cards
    + Constants.cps -- Space for scrollbar

-- | The div showing the deck
deckView :: Context -> Int -> [Card.ID] -> Styled (View Action)
deckView Context {shared, LootView.team} _z cards = do
  cardViews :: [View Action] <- traverse divStack (zip [0 ..] $ Map.toList cards'')
  return $
    div_
      [ style_ $
          zpltwh
            z
            Absolute
            (Constants.cps * 3)
            deckViewTopMargin
            deckViewWidth
            deckViewHeight
            <> "overflow-y" =: "auto"
            <> PCWViewInternal.scrollbarStyle
      ]
      cardViews
  where
    z = 64
    cards'' :: Map (Card 'Core) Int
    cards'' = Map.fromListWith (+) (zip cards' (repeat 1))
    cards' :: [Card 'Core] =
      map (SharedModel.identToCard shared) cards
        & catMaybes
        & map unlift
    divStack :: (Nat, (Card 'Core, Int)) -> Styled (View Action)
    divStack (i, (card, cardinal)) = do
      cards <-
        traverse
          (\offset -> divCard (i, card) offset)
          $ [(cardinal - 1), (cardinal - 2) .. 0]
      return $ div_ [] cards
    divCard :: (Nat, Card 'Core) -> Int -> Styled (View Action)
    divCard (i, card) stackIdx = do
      inner <- PCWViewInternal.cardView LootLoc z shared team card mempty
      return $
        div_
          [ style_ $
              cardPositionStyle'
                ((Constants.cps * x) + (stackIdx * 4))
                ((Constants.cps * y) + (stackIdx * 4))
                <> "z-index" =: ms (z - (stackIdx * 3))
          ]
          [inner]
      where
        xCellsOffset = Constants.cardCellWidth + 1 -- card width + horizontal margin
        yCellsOffset = Constants.cardCellHeight + 1 -- card height + vertical margin
        x = (natToInt i `mod` deckCardsPerRow) * xCellsOffset
        y = (natToInt i `div` deckCardsPerRow) * yCellsOffset
