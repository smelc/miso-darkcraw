-- |
-- The module dispatching the most general view to the different
-- specializations.
-- |
module View (view) where

import DeckView (viewDeck)
import GameView (viewGameModel)
import qualified LootView
import Miso hiding (view)
import Model (Model (..))
import Update (Action)
import ViewInternal (flexColumnStyle, renderStyledView)
import WorldView (viewWorldModel)

view :: Model -> View Action
view m = center $ go m
  where
    go (Model.Deck' model) = renderStyledView $ viewDeck model
    go (Model.Game' model) = renderStyledView $ viewGameModel model
    go (Model.Loot' model) = renderStyledView $ LootView.view model
    go (Model.World' model) = renderStyledView $ viewWorldModel model

center :: View a -> View a
center v = div_ [style_ flexColumnStyle] [v]
