{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module LootView where

import Card
import qualified Constants
import Data.Function ((&))
import qualified Data.Map.Strict as Map
import Data.Maybe
import Miso
import Miso.String hiding (drop, map, take, zip)
import Model (LootModel (..))
import Nat
import PCWViewInternal (DisplayLocation (..), cardPositionStyle)
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
view LootModel {firstVisibleCard = first, lootDeck = deck, lootTeam = team, ..} = do
  deck <- deckView ctxt zpppp deck first
  return $
    div_
      [style_ $ bgStyle z]
      [ deck,
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

-- | Context that is common to all calls to 'deckView'
data Context = Context
  { shared :: SharedModel,
    team :: Team
  }

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
            Relative
            0
            topMargin
            Constants.lobbiesPixelWidth
            height
      ]
      $ [bracket True]
        ++ cardViews
        ++ [bracket False]
  where
    height = Constants.cps * Constants.cardCellHeight
    topMargin = Constants.cps * 17
    bracket leftBracket =
      div_
        [ style_ $
            "font-size" =: px 48
              <> "position" =: "absolute"
              <> "left" =: px left
              <> "height" =: px height -- Needed to center vertically
              <> flexColumnStyle
              <> "justify-content" =: "center" -- Center vertically
        ]
        [div_ [] [text (if leftBracket then "<" else ">")]] -- We need an extra div, so that
        -- the flex style applies (it doesn't apply to inline text).
      where
        left =
          Constants.cps
            * ( if leftBracket
                  then (Constants.boardToLeftCardCellsOffset - 2)
                  else
                    ( Constants.boardToLeftCardCellsOffset
                        + (Constants.cardCellWidth * maxNbCards) -- cards
                        + (maxNbCards - 1) -- gap between cards
                        + 1 -- margin from rightmost card
                    )
              )
    cards' :: [Card 'Core] =
      map (SharedModel.identToCard shared) cards
        & catMaybes
        & map unlift
    maxNbCards = 3
    cardsShown =
      case first of
        Nothing -> []
        Just first -> drop (natToInt first) cards' & take maxNbCards
    divCards :: (Nat, Card 'Core) -> Styled (View Action)
    divCards (i, card) = do
      inner <- PCWViewInternal.cardView LootLoc z shared team card mempty
      return $
        div_
          [ style_ $
              cardPositionStyle
                (Constants.boardToLeftCardCellsOffset + ((Constants.cardCellWidth + 1) * (natToInt i)))
                0
                <> "z-index" =: ms z
          ]
          [inner]
