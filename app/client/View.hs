-- |
-- The module dispatching the most general view to the different
-- specializations.
-- |
module View where

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

viewModel :: Model -> View Action
viewModel m = center $ go m
  where
    go (DeckModel' model) = renderStyledView $ viewDeck model
    go (GameModel' model) = renderStyledView $ viewGameModel model
    go (MultiPlayerLobbyModel' model) = viewMultiPlayerLobbyModel model
    go (SinglePlayerLobbyModel' model) = renderStyledView $ viewSinglePlayerLobbyModel model
    go (LootModel' model) = renderStyledView $ LootView.view model
    go (WelcomeModel' model) = renderStyledView $ viewWelcomeModel model
    go (World' model) = renderStyledView $ viewWorldModel model

center :: View a -> View a
center v = div_ [style_ flexColumnStyle] [v]
