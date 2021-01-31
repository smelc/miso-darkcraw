{-# LANGUAGE DataKinds #-}
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
import Miso.String (MisoString)
import Model (BuildModel (..))
import qualified SharedModel
import Update (Action (..))
import ViewInternal

view :: BuildModel -> Styled (View Action)
view b@BuildModel {buildDeck, hand} = do
  boardDiv <- Deck.viewGeneric $ toGenericModel b
  handDiv <- handDivM
  return $ div_ [] [boardDiv, handDiv]
  where
    (z, zpp) = (0, z + 1)
    handDivM = do
      cells <- GameView.boardToInHandCells zpp $ toHandDrawingInput b
      return $ div_ [style_ handStyle] cells
    handStyle =
      zpltwh z Relative 0 0 handPixelWidth handPixelHeight
        <> "background-image" =: assetsUrl "build-hand.png"

toCardCore BuildModel {buildShared = shared, ..} =
  map (Card.unliftCard . SharedModel.unsafeIdentToCard shared) buildDeck

-- | Used to draw the upper part (relies on 'DeckView')
toGenericModel :: BuildModel -> Deck.GenericModel
toGenericModel b@BuildModel {buildShared = shared, ..} =
  Deck.GenericModel {..}
  where
    gBackground = "build.png"
    gDeck = toCardCore b
    (gPlayer, gTeam) = buildPlayer
    gShared = shared

-- | Used to draw the bottom part (relies on 'GameView')
toHandDrawingInput :: BuildModel -> GameView.HandDrawingInput
toHandDrawingInput b@BuildModel {buildShared = shared, ..} =
  GameView.HandDrawingInput {..}
  where
    hdiHand = zip (toCardCore b) $ repeat False
    hdiInteraction = Nothing
    hdiOffseter = id -- TODO @smelc change me
    (hdiPlayingPlayer, hdiTeam) = buildPlayer
    hdiShared = shared
