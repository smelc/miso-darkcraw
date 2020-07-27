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
viewWelcomeModel :: WelcomeModel -> View Action
viewWelcomeModel m =
  div_
    [style_ style]
    [ torchesDiv zpp,
      div_
        [style_ flexColumnStyle]
        [titleDiv, buttonsDiv]
    ]
  where
    (z, zpp) = (0, z + 1)
    style =
      zpltwh z Relative 0 0 welcomePixelWidth welcomePixelHeight
        <> "background-image" =: assetsUrl "welcome.png"
    -- The top level flex, layout things in a column
    textStyle = Map.fromList textRawStyle
    titleDiv = div_ [style_ titleStyle] [text gameTitle]
    titleFontSize = cellPixelSize + (cellPixelSize `div` 2)
    titleStyle =
      textStyle
        <> "font-size" =: px titleFontSize
        <> "z-index" =: ms zpp
        <> "margin-top" =: px cellPixelSize
    -- A flex right below the top level, layout things in a line
    -- It has two cells: ["single player"; "start"]
    buttonsDiv =
      div_
        [ style_ $
            flexLineStyle
              <> "justify-content" =: "center"
              <> "width" =: px welcomePixelWidth
              <> "margin-top" =: px cps
        ]
        [singlePlayerTextDiv, selectTeamDiv zpp singlePlayerTeam]
      where
        singlePlayerTeam = m ^? #playingMode . #_SinglePlayer
    singlePlayerFontSize = (cps + titleFontSize) `div` 3
    singlePlayerTextDiv =
      div_
        [ style_ $
            textStyle
              <> marginhv cps 0
              <> "font-size" =: px singlePlayerFontSize
        ]
        [text "Single Player"]

selectTeamDiv :: Int -> Maybe Team -> View Action
selectTeamDiv z maybeTeam =
  div_
    [style_ flexLineStyle]
    $ [ div_
          [style_ flexColumnStyle]
          ( stytextztrbl z "Choose your team" 0 0 (cps `div` 2) 0
              : [teamButton z maybeTeam t | t <- allTeams]
          ),
        startButtonDiv
      ]
  where
    teamSelected = isJust maybeTeam
    startButtonDiv =
      textButton
        gui
        z
        (if teamSelected then Enabled else Disabled)
        ( style_ (marginhv cps 0)
            : [onClick $ WelcomeAction' WelcomeStart | teamSelected]
        )
        "Start"

teamButton ::
  -- The z index
  Int ->
  -- The team actually selected
  Maybe Team ->
  -- The team for which to build the button
  Team ->
  View Action
teamButton z selected team =
  marginifyhv 2 4 $ anyButton gui z bState builder
  where
    bState =
      case selected of
        Nothing -> Enabled
        Just t | t == team -> Selected
        _ -> Disabled
    tile Human = "24x24_3_0.png"
    tile Undead = "24x24_1_1.png"
    textAndTile =
      [ div_ [noDrag] [stytextz z (ms $ ppTeam team)], -- text
        img_' $ tile team -- tile
      ]
    onClickAction = WelcomeAction' $ WelcomeSelectSinglePlayer team
    builder x =
      div_
        ([style_ flexLineStyle, onClick onClickAction] ++ x)
        $ map (marginifyhv 2 2) textAndTile

torchesDiv :: Int -> View Action
torchesDiv z =
  div_
    []
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
