{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module to display a list of cards.
-- |
module DeckView
  ( DeckModel (..),
    GenericModel (..),
    viewDeck,
    viewGeneric,
  )
where

import Card (Card, Phase (Core), Team, groupCards)
import Constants
import Data.Function ((&))
import Data.List
import qualified Data.Map.Strict as Map
import Miso (View, div_, onClick, style_)
import Miso.String (MisoString, ms)
import Miso.Util ((=:))
import Model
import PCWViewInternal
import SharedModel
import Spots
import Update (Action (DeckBack))
import ViewBlocks (ButtonState (..), gui, textButton)
import ViewInternal (Position (..), Styled (..), imgCell, px, textStyle, zpltwh)

data GenericModel = GenericModel
  { -- | The background to use
    background :: MisoString,
    -- | The deck to show
    deck :: [Card 'Core],
    -- | To which player 'gDeck' belongs
    player :: Spots.Player,
    -- | To which team the deck being shown belongs
    team :: Team,
    -- | Part of the model shared among all pages
    shared :: SharedModel
  }

viewDeck :: DeckModel -> Styled (View Action)
viewDeck DeckModel {..} =
  viewGeneric GenericModel {background = "deck.png", ..}

viewGeneric :: GenericModel -> Styled (View Action)
viewGeneric GenericModel {background = bg, ..} = do
  backDiv <- backDivM
  cardsDiv <- cardsDiver 0 0 cards
  return $
    div_
      [style_ bgStyle]
      $ [backDiv] ++ cardsDiv
  where
    (z, zpp) = (0, z + 1)
    bgStyle =
      zpltwh z Relative 0 0 lobbiesPixelWidth lobbiesPixelHeight
        <> "background-image" =: assetsUrl bg
    cards = groupCards deck & Map.toList & sort
    -- Terminal case
    cardsDiver x y _ | x == 4 && y == 3 = return []
    -- Go to new line
    cardsDiver x y cards | x == 4 = cardsDiver 0 (y + 1) cards
    -- No more cards, draw slot; then iterate
    cardsDiver x y [] = do
      let hd = slotDiver x y
      rest <- cardsDiver (x + 1) y []
      return $ hd : rest
    -- No card associated to ID, should not happen; but needed for completness
    cardsDiver x y ((_, []) : rest) = cardsDiver x y rest
    -- Regular cards to draw case; then iterate
    cardsDiver x y ((_, card : _) : rest) = do
      start <- cardDiver x y card
      end <- cardsDiver (x + 1) y rest
      return $ start : end
    xoffset x = 3 + (x * (cardCellWidth + 1))
    yoffset y = 3 + (y * (cardCellHeight + 1))
    -- Create div of a single card
    cardDiver x y card = do
      card <- cardView DeckLoc z shared team card mempty
      return $ div_ [style_ $ cardPositionStyle (xoffset x) (yoffset y)] [card]
    -- Create background of slot
    slotDiver x y =
      div_
        [style_ $ cardPositionStyle (xoffset x) (yoffset y)]
        [imgCell slotPath]
    slotPath = case player of
      PlayerTop -> assetSlotRed
      PlayerBot -> assetSlotBlue
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
