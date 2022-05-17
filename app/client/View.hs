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
import MultiPlayerLobbyView (viewMultiPlayerLobbyModel)
import SinglePlayerLobbyView (viewSinglePlayerLobbyModel)
import Update (Action)
import ViewInternal (flexColumnStyle, renderStyledView)
import WelcomeView (viewWelcomeModel)
import WorldView (viewWorldModel)

view :: Model -> View Action
view m = center $ go m
  where
    go (Model.Deck' model) = renderStyledView $ viewDeck model
    go (Model.Game' model) = renderStyledView $ viewGameModel model
    go (MultiPlayerLobbyModel' model) = viewMultiPlayerLobbyModel model
    go (SinglePlayerLobbyModel' model) = renderStyledView $ viewSinglePlayerLobbyModel model
    go (Model.Loot' model) = renderStyledView $ LootView.view model
    go (Model.Welcome' model) = renderStyledView $ viewWelcomeModel model
    go (Model.World' model) = renderStyledView $ viewWorldModel model

center :: View a -> View a
center v = div_ [style_ flexColumnStyle] [v]
