{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module LootView where

import Card
import qualified Constants
import Data.Function ((&))
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe
import Miso
import Miso.String hiding (drop, length, map, take, zip)
import Model (LootModel (..))
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
view :: LootModel -> Styled (View Action)
view LootModel {..} = do
  deck <- deckView ctxt zpppp deck
  return $
    div_
      [style_ $ bgStyle z]
      [ deck,
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
    deckLegend =
      div_
        [ style_ $
            zpltwh
              z
              Absolute
              0 -- left
              ((deckViewTopMargin + deckViewHeight) + (Constants.cps `div` 4)) -- below deck + margin
              Constants.lobbiesPixelWidth -- width
              Constants.cps -- height
        ]
        [ div_
            [ style_ $
                textStyle
                  <> flexLineStyle -- Horizontal layouting
                  <> "width" =: px Constants.lobbiesPixelWidth -- Needed to center horizontally
                  <> "justify-content" =: "center" -- Center horizontally
            ]
            [text "Your deck"]
        ]

-- | Context that is common to all calls to 'deckView'
data Context = Context
  { shared :: SharedModel,
    team :: Team
  }

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
    + (deckCardsPerRow - 1) * Constants.cps -- Gaps between cards
    + Constants.cps -- Space for scrollbar

-- | The div for the < card1 card2 card3 > view, the left and right
-- angle brackets being buttons.
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
