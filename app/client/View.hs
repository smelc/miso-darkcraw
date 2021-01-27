-- |
-- The module dispatching the most general view to the different
-- specializations.
-- |
module View where

import qualified BuildView as Build
import DeckView (viewDeck)
import GameView (viewGameModel)
import Miso hiding (view)
import Model (Model (..))
import MultiPlayerLobbyView (viewMultiPlayerLobbyModel)
import SinglePlayerLobbyView (viewSinglePlayerLobbyModel)
import Update (Action)
import ViewInternal (renderStyledView)
import WelcomeView (viewWelcomeModel)

viewModel :: Model -> View Action
viewModel (BuildModel' model) = renderStyledView $ Build.view model
viewModel (DeckModel' model) = renderStyledView $ viewDeck model
viewModel (GameModel' model) = renderStyledView $ viewGameModel model
viewModel (WelcomeModel' model) = renderStyledView $ viewWelcomeModel model
viewModel (MultiPlayerLobbyModel' model) = viewMultiPlayerLobbyModel model
viewModel (SinglePlayerLobbyModel' model) = renderStyledView $ viewSinglePlayerLobbyModel model
