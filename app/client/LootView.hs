{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module LootView where

import Card
import qualified Cinema
import qualified Constants
import Data.Function ((&))
import Data.Functor ((<&>))
import qualified Data.Map.Strict as Map
import Data.Maybe
import Miso
import Miso.String hiding (drop, length, map, take, zip)
import Model (LootModel (..))
import Nat
import PCWViewInternal (DisplayLocation (..), cardPositionStyle)
import qualified PCWViewInternal
import SharedModel (SharedModel)
import qualified SharedModel
import Update (Action (LootAction'), LootAction (DeckTo))
import ViewInternal

-- |
-- Module to display the view reached after a successful end of 'GameView'.
-- This view sits between two games, where the player can pick new
-- cards to augment its deck.
-- |
view :: LootModel -> Styled (View Action)
view LootModel {firstVisibleCard = first, ..} = do
  deck <- deckView ctxt zpppp deck first
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
deckViewHeight = (Constants.cardPixelHeight * 2) + Constants.cps -- 2 cards high + vertical gap

deckCardsPerRow :: Int
deckCardsPerRow = 4

deckViewWidth :: Int
deckViewWidth =
  (Constants.cardPixelWidth * deckCardsPerRow) -- Cards
    + (deckCardsPerRow - 1) * Constants.cps -- Gaps between cards
    + Constants.cps -- Space for scrollbar

-- | The div for the < card1 card2 card3 > view, the left and right
-- angle brackets being buttons.
deckView :: Context -> Int -> [Card.ID] -> Maybe Nat -> Styled (View Action)
deckView Context {shared, LootView.team} z cards first = do
  cardViews :: [View Action] <- traverse divCards (zip [0 ..] cardsShown)
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
      $ [bracket True]
        ++ cardViews
        ++ [bracket False]
  where
    bracket leftBracket =
      div_
        [ style_ $
            "position" =: "absolute"
              <> margin =: (ms (px (Constants.cps * 3)))
              <> "height" =: px deckViewHeight -- Needed to center vertically
              <> flexColumnStyle -- Vertical layouting
              <> "justify-content" =: "center" -- Center vertically
        ]
        [button_ attrs [text (if leftBracket then "<" else ">")]] -- We need an extra div, so that
        -- the flex style applies (it doesn't apply to inline text).
      where
        margin :: MisoString = if leftBracket then "left" else "right"
        attrs =
          -- Attributes of the angle bracket buttons
          (action <&> onClick & maybeToList)
            ++ [ style_ $
                   "color" =: bracketColor
                     <> "background-color" =: "transparent" -- no background
                     <> "border" =: "none"
                     <> "font-size" =: px 48
               ]
        bracketColor =
          case action of
            Nothing -> Constants.greyHTML
            Just _ -> Constants.greenHTML
        action :: Maybe Action =
          Update.LootAction'
            <$> case (first, leftBracket) of
              (Nothing, _) -> Nothing
              (Just first, True) | first <= 0 -> Nothing
              (Just first, False) | natToInt first >= (length cards - 1) -> Nothing
              (Just first, True) | first >= 1 -> Just $ Update.DeckTo Cinema.ToLeft
              (Just first, False) | natToInt first < (length cards - 1) -> Just $ Update.DeckTo Cinema.ToRight
              _ -> Nothing
    cards' :: [Card 'Core] =
      map (SharedModel.identToCard shared) cards
        & catMaybes
        & map unlift
    cardsShown =
      case first of
        Nothing -> []
        Just first -> drop (natToInt first) cards'
    divCards :: (Nat, Card 'Core) -> Styled (View Action)
    divCards (i, card) = do
      inner <- PCWViewInternal.cardView LootLoc z shared team card mempty
      return $
        div_
          [ style_ $
              cardPositionStyle
                x
                y
                <> "z-index" =: ms z
          ]
          [inner]
      where
        xCellsOffset = Constants.cardCellWidth + 1 -- card width + horizontal margin
        yCellsOffset = Constants.cardCellHeight + 1 -- card height + vertical margin
        x = (natToInt i `mod` deckCardsPerRow) * xCellsOffset
        y = (natToInt i `div` deckCardsPerRow) * yCellsOffset
