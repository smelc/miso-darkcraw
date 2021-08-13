{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module to display the initial view of the game, i.e. the one
-- that shows up when the game starts.
-- |
module WelcomeView (viewWelcomeModel) where

import Cinema (TimedFrame (..))
import Configuration (Configuration (..), Edition (..))
import qualified Configuration as Config
import Constants
import Data.Function ((&))
import qualified Data.Map.Strict as Map
import qualified Data.Vector as V
import Miso
import Miso.String (fromMisoString, ms)
import Miso.Util ((=:))
import Model (SceneModel (..), TimedFrames, WelcomeModel (..))
import PCWViewInternal (DisplayMode (..), viewFrame)
import SharedModel (SharedModel)
import qualified SharedModel
import qualified Tile
import Update
import ViewBlocks (ButtonState (..), gui, textButton)
import ViewInternal

-- | Constructs a virtual DOM from a welcome model
viewWelcomeModel :: WelcomeModel -> Styled (View Action)
viewWelcomeModel WelcomeModel {welcomeShared = shared, ..} = do
  multiPlayerDiv <-
    createButtonDivM multiPlayerMargin MultiPlayerDestination "Multiplayer"
  singlePlayerDiv <-
    createButtonDivM singlePlayerMargin SinglePlayerDestination "Single Player"
  let main =
        div_
          [style_ bgStyle]
          $ [ torchesDiv zpp, -- Absolute placement
              versionDiv shared zpp Config.get, -- Absolute placement
              div_
                [style_ flexColumnStyle]
                -- top level flex, layout things in a column
                [titleDiv, singlePlayerDiv, multiPlayerDiv]
            ]
            ++ viewWelcomeScene welcomeSceneModel
            ++ debugSlider
  let support =
        case Config.get of
          Itch {} -> []
          Dev -> [supportDiv]
          Schplaf {} -> [supportDiv]
  return $ div_ [] $ main : support
  where
    (z, zpp, zpppp) = (0, z + 1, zpp + 1)
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
    multiplayerEnabled = Config.isDev
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
    debugSlider :: [View Action]
    debugSlider
      | (ScenePausedForDebugging frames i) <- welcomeSceneModel =
        [ input_
            [ type_ "range",
              min_ "0",
              max_ (ms $ length frames - 1),
              step_ "1'",
              value_ (ms i),
              onInput (SceneAction' . JumpToFrameForDebugging . read . fromMisoString),
              autofocus_ True,
              style_ ("position" =: "absolute" <> "top" =: "650px" <> "width" =: "500px")
            ]
        ]
      | otherwise = []
    viewWelcomeScene :: SceneModel -> [View Action]
    viewWelcomeScene (ScenePlaying frames i) = viewFrameAt NormalMode frames i
    viewWelcomeScene (ScenePausedForDebugging frames i) = viewFrameAt DebugMode frames i
    viewWelcomeScene _ = []
    viewFrameAt :: DisplayMode -> TimedFrames -> Int -> [View Action]
    viewFrameAt mode frames i =
      let TimedFrame {frame} = frames V.! i
       in [viewFrame mode zpppp shared frame]

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

versionDiv :: SharedModel -> Int -> Configuration -> View Action
versionDiv shared z config =
  div_
    [style_ $ style <> textStyle, style_ flexLineStyle]
    $ [text $ ms txt]
      ++ [img_ [src_ $ tile16 (if legendary then Tile.Crown else Tile.Sword3)]]
  where
    txt = "Version: " ++ location ++ hashString' ++ ", " ++ (if legendary then "Legendary, " else "")
    textStyle = Map.fromList textRawStyle
    (location, legendary, hashString) =
      case config of
        Dev -> ("dev", True, Nothing)
        Itch Vanilla h -> ("Itch", False, Just h)
        Itch Legendary h -> ("Itch", True, Just h)
        Schplaf Vanilla h -> ("schplaf", False, Just h)
        Schplaf Legendary h -> ("schplaf", True, Just h)
    hashString' =
      case hashString of
        Nothing -> ""
        Just h -> ", " ++ h
    x = cps
    y = lobbiesPixelHeight - cps
    style = zplt z Absolute x y
    tile16 tile =
      SharedModel.tileToFilepath shared tile Tile.Sixteen
        & Tile.filepathToString
        & ms
        & Constants.assetsPath

supportDiv :: View a
supportDiv =
  div_
    [ style_ $
        "width" =: px lobbiesPixelWidth
          <> "outline" =: "1px solid red"
          <> "margin-top" =: px 1
    ]
    [ div_
        [ style_ flexColumnStyle,
          style_ $
            "width" =: px (lobbiesPixelWidth - cps * 2)
              <> "margin-left" =: px cps
        ]
        [ div_
            []
            [ text "This is a development version whose ",
              a_ [href_ "https://github.com/smelc/miso-darkcraw"] [text "code is on GitHub"],
              text ". To show your support, star it ⭐, or leave a comment on the released version:"
            ],
          a_
            [ href_ itchUrl,
              style_ $ "margin-top" =: px 12 <> "margin-bottom" =: px 12
            ]
            [text itchUrl]
        ]
    ]
  where
    itchUrl = "https://hgames.itch.io/pixel-card-wars"
