{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- |
-- Module to display an instance of the view in-between games,
-- when augmenting the deck with new cards. Can depend on 'DeckView'
-- and 'GameView'.
-- |
module BuildView (view) where

import Card
import Constants
import Data.Function ((&))
import qualified DeckView as Deck
import qualified GameView
import Miso hiding (view)
import Model (BuildModel (..))
import qualified SharedModel
import Update (Action (..))
import ViewInternal

view :: BuildModel -> Styled (View Action)
view b = do
  boardDiv <- Deck.viewGeneric $ toGenericModel b
  handDiv <- handDivM
  return $ div_ [] [boardDiv, handDiv]
  where
    (z, zpp) = (0, z + 1)
    handDivM = do
      cells <- GameView.boardToInHandCells zpp $ toHandDrawingInput b
      return $ div_ [style_ handStyle] cells
    handStyle =
      zpltwh z Relative 0 0 handPixelWidth buildPixelHeight
        <> "background-image" =: assetsUrl "build-hand.png"

-- | Used to draw the upper part (relies on 'DeckView')
toGenericModel :: BuildModel -> Deck.GenericModel
toGenericModel BuildModel {..} =
  Deck.GenericModel {..}
  where
    gBackground = "build.png"
    gDeck = map (Card.unlift . SharedModel.unsafeIdentToCard shared) buildDeck
    (gPlayer, gTeam) = buildPlayer
    gShared = shared

-- | Used to draw the bottom part (relies on 'GameView')
toHandDrawingInput :: BuildModel -> GameView.HandDrawingInput
toHandDrawingInput BuildModel {..} =
  GameView.HandDrawingInput {..}
  where
    itemCards =
      SharedModel.getCards shared
        & filter (\case ItemCard {} -> True; _ -> False)
    hdiHand = zip (map Card.unlift itemCards) $ repeat False
    hdiInteraction = Nothing
    hdiMana = maxBound -- Simulate gigantic amount of mana, so that
    -- all cards are playable (no grey overlay)
    hdiOffseter (x, y) = (x + boardPixelWidth `div` 2, y - cps)
    (hdiPlayingPlayer, hdiTeam) = buildPlayer
    hdiShared = shared
