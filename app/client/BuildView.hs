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
import Data.Function ((&))
import qualified DeckView as Deck
import Miso
import Miso.String (MisoString)
import Model (BuildModel (..))
import qualified SharedModel
import Update (Action (..))
import ViewInternal

viewBuildModel :: BuildModel -> Styled (View Action)
viewBuildModel b@BuildModel {buildDeck, hand} =
  Deck.viewGeneric $ toGenericModel b

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
