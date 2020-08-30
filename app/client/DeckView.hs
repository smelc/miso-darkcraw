{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module to display a list of cards.
-- |
module DeckView (viewDeck) where

import Board (PlayerSpot (..))
import Card (groupCards, unsafeCardToCreature)
import Constants
import Data.Function ((&))
import Data.List
import qualified Data.Map.Strict as Map
import Debug.Trace (trace, traceId, traceShowId)
import Miso (View, div_, onClick, style_)
import Miso.String (ms)
import Miso.Util ((=:))
import Model (DeckModel (..))
import PCWViewInternal (cardCreature, cardPositionStyle)
import Update (Action (DeckBack), Action)
import ViewBlocks (ButtonState (..), gui, textButton)
import ViewInternal (Position (..), Styled (..), imgCell, px, textStyle, zpltwh)

viewDeck :: DeckModel -> Styled (View Action)
viewDeck DeckModel {deck, deckBack, deckPlayer} = do
  backDiv <- backDivM
  return
    $ div_
      [style_ bgStyle]
    $ [backDiv] ++ cardsDiver 0 0 cards
  where
    (z, zpp) = (0, z + 1)
    bgStyle =
      zpltwh z Relative 0 0 lobbiesPixelWidth lobbiesPixelHeight
        <> "background-image" =: assetsUrl "deck.png"
    cards = groupCards deck & Map.toList & sort
    -- Terminal case
    cardsDiver x y _ | x == 4 && y == 3 = []
    -- Go to new line
    cardsDiver x y cards | x == 4 = cardsDiver 0 (y + 1) cards
    -- No more cards, draw slot; then iterate
    cardsDiver x y [] = slotDiver x y : cardsDiver (x + 1) y []
    -- No card associated to ID, should not happen; but needed for completness
    cardsDiver x y ((_, []) : rest) = cardsDiver x y rest
    -- Regular cards to draw case; then iterate
    cardsDiver x y ((ident, card : _) : rest) =
      cardDiver x y card : cardsDiver (x + 1) y rest
    xoffset x = 3 + (x * (cardCellWidth + 1))
    yoffset y = 3 + (y * (cardCellHeight + 1))
    -- Create div of a single card
    cardDiver x y card =
      div_
        [style_ $ cardPositionStyle (xoffset x) (yoffset y)]
        [cardCreature z (Just $ unsafeCardToCreature card) False]
    -- Create background of slot
    slotDiver x y =
      div_
        [style_ $ cardPositionStyle (xoffset x) (yoffset y)]
        [imgCell slotPath]
    slotPath = case deckPlayer of
      PlayerTop -> assetSlotRed
      PlayerBottom -> assetSlotBlue
    subtitleStyle = textStyle <> "font-size" =: px subtitleFontSize
    backDivM = do
      button <-
        textButton
          gui
          zpp
          Enabled
          [style_ subtitleStyle, onClick DeckBack]
          "Back"
      let sty =
            "z-index" =: ms zpp
              <> "position" =: "absolute"
              -- Center horizontally
              <> "margin-left" =: "50%"
              <> "margin-right" =: "50%"
              -- And tell the element to center horizontally, not to its left
              <> "transform" =: "translate(-50%, 0%)"
              -- Finally shift element down
              <> "margin-top" =: px ((boardCellHeight - 3) * cps)
      return $ div_ [style_ sty] [button]