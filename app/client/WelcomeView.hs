{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module to display the initial view of the game, i.e. the one
-- that shows up when the game starts.
-- |
module WelcomeView (viewWelcomeModel) where

import Card (Team (..), allTeams, ppTeam)
import Constants
import qualified Data.Map.Strict as Map
import Miso
import Miso.String hiding (length, map)
import Miso.Util ((=:))
import Model (WelcomeModel (..))
import Update
import ViewBlocks (gui, textButton)
import ViewInternal

-- | Constructs a virtual DOM from a welcome model
viewWelcomeModel :: WelcomeModel -> View Action
viewWelcomeModel _ =
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
      Map.union (zpltwh z Relative 0 0 welcomePixelWidth welcomePixelHeight) $
        Map.fromList
          [("background-image", assetsUrl "welcome.png")]
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
        [singlePlayerTextDiv, selectTeamDiv zpp]
    singlePlayerFontSize = (cps + titleFontSize) `div` 3
    singlePlayerTextDiv =
      div_
        [ style_ textStyle,
          style_ $ marginhv cps 0,
          style_ $ "font-size" =: px singlePlayerFontSize
        ]
        [text "Single Player"]

selectTeamDiv :: Int -> View Action
selectTeamDiv z =
  div_
    [style_ flexLineStyle]
    $ [ div_
          [style_ flexColumnStyle]
          $ stytextztrbl z "Choose your team" 0 0 (cps `div` 2) 0
            : [teamButton z t | t <- allTeams]
      ]
      ++ startButtonDiv
  where
    startButtonDiv :: [View Action] =
      textButton
        gui
        z
        True
        [ onClick $ WelcomeAction' WelcomeStart,
          style_ $ marginhv cps 0
        ]
        "Start"

teamButton :: Int -> Team -> View Action
teamButton z team =
  builder []
  where
    tile Human = "24x24_3_0.png"
    tile Undead = "24x24_1_1.png"
    (hmargin, vmargin) = (0, cellPixelSize `div` 4)
    textAndTile =
      [ stytextzhv z (ms $ ppTeam team) 0 0,
        marginifyhv 4 0 $ img_' $ tile team
      ]
    builder x =
      div_ (style_ flexLineStyle : x) $ map (marginifyhv hmargin vmargin) textAndTile

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
      Map.union
        (tilezprb z Absolute 0 (14 + (if x then 1 else -1)))
        ("background-image" =: assetsUrl "torchs.png")
        <> ("animation" =: ("torch " <> duration x <> " steps(2) infinite"))
    duration x = if x then "2s" else "2.5s"
