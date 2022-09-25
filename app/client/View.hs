{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- The module dispatching the most general view to the different
-- specializations.
-- |
module View (view) where

import qualified Configuration
import qualified Constants
import DeckView (viewDeck)
import GameView (viewGameModel)
import qualified LootView
import Miso hiding (view)
import Model (End (..), Model (..))
import Update (Action)
import qualified ViewBlocks as VB
import ViewInternal
import WorldView (viewWorldModel)

view :: Model -> View Action
view m = center $ go m
  where
    go =
      \case
        Model.Deck' model -> renderStyledView $ viewDeck model
        Model.Game' model -> renderStyledView $ viewGameModel model
        Model.Loot' model -> renderStyledView $ LootView.view model
        Model.World' model -> renderStyledView $ viewWorldModel model
        Model.End' model -> renderStyledView $ viewEndModel model

center :: View a -> View a
center v = div_ [style_ flexColumnStyle] [v]

viewEndModel :: Model.End -> Styled (View Update.Action)
viewEndModel Model.End {win} = do
  button <-
    VB.textButton
      VB.gui
      z
      VB.Enabled
      [style_ $ textStyle <> ("font-size" =: px 16), buttonStyle]
      "Next →"
  let builder attrs =
        div_
          attrs
          [ div_
              ([style_ bgStyle])
              [ div_
                  [youWinStyle]
                  [div_ [youWinTextStyle] [Miso.text youWinText]]
              ],
            button
          ]
  ViewInternal.fade builder Nothing 2 fadeSty
  where
    z = 0
    background = (if win then "win" else "lose") <> ".png"
    bgStyle =
      ViewInternal.zpltwh z Relative 0 0 Constants.lobbiesPixelWidth Constants.lobbiesPixelHeight
        <> "background-image" =: Constants.assetsUrl background
    fadeSty =
      case Configuration.isDev of
        True -> Constants.DontFade
        False -> Constants.FadeIn
    youWinStyle =
      style_ $
        zpltwh
          z
          Absolute
          0 -- left
          (4 * Constants.cps) -- top
          Constants.lobbiesPixelWidth -- width
          Constants.cps -- height
    youWinText = if win then "You win! 🎉 👑" else "You lose! 😞 💀"
    youWinTextStyle =
      style_ $
        textStyle
          <> flexLineStyle -- Horizontal layouting
          <> "width" =: px Constants.lobbiesPixelWidth -- Needed to center horizontally
          <> "justify-content" =: "center" -- Center horizontally
          <> "font-size" =: px 24
    buttonStyle =
      style_ $
        zpltwh
          z
          Absolute
          500 -- left
          ((9 * Constants.cps) + Constants.cps `div` 2) -- top
          (4 * Constants.cps) -- width
          (2 * Constants.cps) -- height
