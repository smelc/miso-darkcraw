{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module to display the initial view of the game, i.e. the one
-- that shows up when the game starts.
-- |
module WelcomeView (viewWelcomeModel) where

import Card (Team (..), allTeams, ppTeam)
import Constants
import Control.Lens
import Configuration
import Data.Generics.Labels
import qualified Data.Map.Strict as Map
import Data.Maybe (isJust)
import Miso
import Miso.String hiding (concat, length, map)
import Miso.Util ((=:))
import Model (PlayingMode (..), WelcomeModel (..))
import Update
import ViewBlocks (ButtonState (..), anyButton, gui, textButton)
import ViewInternal

-- | Constructs a virtual DOM from a welcome model
viewWelcomeModel :: WelcomeModel -> Styled (View Action)
viewWelcomeModel _ = do
  multiPlayerDiv <-
    createButtonDivM multiPlayerMargin MultiPlayerDestination "Multiplayer"
  singlePlayerDiv <-
    createButtonDivM singlePlayerMargin SinglePlayerDestination "Single Player"
  return $
    div_
      [style_ bgStyle]
      [ torchesDiv zpp, -- Absolute placement
        versionDiv zpp configuration, -- Absolute placement
        div_
          [style_ flexColumnStyle]
          -- top level flex, layout things in a column
          [titleDiv, singlePlayerDiv, multiPlayerDiv]
      ]
  where
    (z, zpp) = (0, z + 1)
    bgStyle =
      zpltwh z Relative 0 0 lobbiesPixelWidth lobbiesPixelHeight
        <> "background-image" =: assetsUrl "welcome.png"
    textStyle = Map.fromList textRawStyle
    titleDiv = div_ [style_ titleStyle] [text gameTitle]
    titleStyle =
      textStyle
        <> "font-size" =: px titleFontSize
        <> "z-index" =: ms zpp
        <> "margin-top" =: px (cps * 2)
    singlePlayerMargin = "margin-top" =: px (cps * 4)
    multiPlayerMargin = "margin-top" =: px (cps * 11)
    buttonTextStyle =
      textStyle
        <> "font-size" =: px subtitleFontSize
        <> "z-index" =: ms zpp
    multiplayerEnabled =
      case configuration of
        Configuration _ Itch -> False
        _ -> True
    -- A flex right below the top level, layout things in a line
    -- It has two cells: ["single player"; "choose your team -> start"]
    createButtonDivM customStyle dest text =
      textButton
        gui
        zpp
        (if enabled then Enabled else Disabled)
        ( [style_ buttonTextStyle, style_ customStyle]
            ++ [onClick $ WelcomeGo dest | enabled]
        )
        text
      where
        enabled =
          case dest of
            SinglePlayerDestination -> True
            MultiPlayerDestination -> multiplayerEnabled

torchesDiv :: Int -> View Action
torchesDiv z =
  div_
    []
    -- TODO lift this style to the top with the Writer
    [ nodeHtml
        "style"
        []
        ["@keyframes torch { 100% { background-position: -48px; } }"],
      torch True,
      torch False
    ]
  where
    torch x = div_ [style_ $ style x] []
    style x =
      tilezprb z Absolute 0 (14 + (if x then 1 else -1))
        <> "background-image" =: assetsUrl "torchs.png"
        <> "animation" =: ("torch " <> duration x <> " steps(2) infinite")
    duration x = if x then "2s" else "2.5s"

versionDiv :: Int -> Configuration -> View Action
versionDiv z (Configuration edition location) =
  div_
    [style_ $ style <> textStyle, style_ flexLineStyle]
    $ [text $ ms txt]
      ++ [img_ [src_ (assetsPath assetFilenameCrown)] | legendary]
  where
    txt = "Version: " ++ showLocation location ++ ", " ++ show edition
    textStyle = Map.fromList textRawStyle
    showLocation Dev = "dev"
    showLocation Itch = "Itch"
    legendary =
      case edition of
        Legendary -> True
        Vanilla -> False
    x = cps
    y = lobbiesPixelHeight - cps
    style = zplt z Absolute x y
