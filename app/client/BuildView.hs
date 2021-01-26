{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- |
-- Module to display an instance of the view in-between games,
-- when augmenting the deck with new cards. Can depend on 'DeckView'
-- and 'GameView'.
-- |
module BuildView where

import Card
import Constants
import Data.Function ((&))
import qualified DeckView as Deck
import Miso
import Miso.String (MisoString)
import Model (BuildModel (..))
import qualified SharedModel
import Update (Action (..))
import ViewInternal

viewBuildModel :: BuildModel -> Styled (View Action)
viewBuildModel b@BuildModel {buildDeck, hand} = do
  boardDiv <- Deck.viewGeneric $ toGenericModel b
  handDiv <- handDivM
  return $ div_ [] [boardDiv, handDiv]
  where
    handDivM = do
      cells <- undefined -- boardToInHandCells zpp model
      return $ div_ [style_ handStyle] cells
    handStyle =
      zpltwh 0 Relative 0 0 handPixelWidth handPixelHeight
        <> "background-image" =: assetsUrl "build-hand.png"

toGenericModel :: BuildModel -> Deck.GenericModel
toGenericModel BuildModel {buildShared = shared, ..} =
  Deck.GenericModel {..}
  where
    gBack = undefined
    gBackground = "build.png"
    gDeck = map (Card.unliftCard . SharedModel.unsafeIdentToCard shared) buildDeck
    gPlayer = undefined
    gShared = shared
    gTeam = buildTeam
