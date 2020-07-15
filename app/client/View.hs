-- |
-- The module dispatching the most general view to the different
-- specializations.
-- |
module View where

import GameView (viewGameModel)
import Miso
import Model (Model (..))
import Update (Action)
import WelcomeView (viewWelcomeModel)

viewModel :: Model -> View Action
viewModel (GameModel' model) = viewGameModel model
viewModel (WelcomeModel' model) = viewWelcomeModel model
