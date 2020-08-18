-- |
-- The module dispatching the most general view to the different
-- specializations.
-- |
module View where

import GameView (viewGameModel)
import Miso
import Model (Model (..))
import MultiPlayerLobbyView (viewMultiPlayerLobbyModel)
import Update (Action)
import ViewInternal (renderStyledView)
import WelcomeView (viewWelcomeModel)

viewModel :: Model -> View Action
viewModel (GameModel' model) = renderStyledView $ viewGameModel model
viewModel (WelcomeModel' model) = renderStyledView $ viewWelcomeModel model
viewModel (MultiPlayerLobbyModel' model) = viewMultiPlayerLobbyModel model
