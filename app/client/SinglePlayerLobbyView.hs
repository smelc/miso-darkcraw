{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module to display the single player page, where the player gets
-- to choose its team; or go back to the welcome page.
-- |
module SinglePlayerLobbyView (viewSinglePlayerLobbyModel) where

import Card
  ( CreatureID (..),
    CreatureKind (..),
    Team (..),
    allTeams,
    creatureToFilepath,
    filepathToString,
    ppTeam,
  )
import Constants
import Control.Lens
import Data.Generics.Labels
import qualified Data.Map.Strict as Map
import Data.Maybe (isJust)
import Miso
import Miso.String hiding (concat, length, map)
import Miso.Util ((=:))
import Model (Model (..), SinglePlayerLobbyModel (..))
import SharedModel (SharedModel (..), identToCard, identToCreature)
import Update
import ViewBlocks (ButtonState (..), anyButton, gui, textButton)
import ViewInternal

-- | Constructs a virtual DOM from a 'SinglePlayerLobbyModel'
viewSinglePlayerLobbyModel :: SinglePlayerLobbyModel -> Styled (View Action)
viewSinglePlayerLobbyModel m@SinglePlayerLobbyModel {..} = do
  chooseTeamDiv <- chooseTeamDivM
  startDiv <- startDivM
  backDiv <- backDivM
  return $
    div_
      [style_ style]
      [ div_
          [style_ flexColumnStyle]
          [titleDiv, chooseTeamDiv, startDiv, backDiv]
      ]
  where
    (z, zpp) = (0, z + 1)
    style =
      zpltwh z Relative 0 0 lobbiesPixelWidth lobbiesPixelHeight
        <> "background-image" =: assetsUrl "singleplayer.png"
    -- The top level flex, layout things in a column
    titleDiv = div_ [style_ titleStyle] [text "Single Player"]
    titleStyle =
      textStyle
        <> "font-size" =: px titleFontSize
        <> "z-index" =: ms zpp
        <> "margin-top" =: px (cps * 2)
    subtitleStyle = textStyle <> "font-size" =: px subtitleFontSize
    pushRight = "margin-right" =: px 64
    pushLeft = "margin-left" =: px 96
    maybeTeam = singlePlayerLobbyTeam
    chooseTeamDivM = do
      selectTeam <- selectTeamDiv singlePlayerLobbyShared zpp maybeTeam
      let chooseYourTeam =
            div_
              [style_ $ subtitleStyle <> pushRight]
              [text "Choose your team"]
      return $
        div_
          [ style_ $
              flexLineStyle
                <> "justify-content" =: "center"
                <> "width" =: px lobbiesPixelWidth
                <> "margin-top" =: px (cps * 2)
                <> textStyle
          ]
          [chooseYourTeam, selectTeam]
    startDivM = do
      button <-
        textButton
          gui
          zpp
          (if isJust maybeTeam then Enabled else Disabled)
          ( [style_ $ subtitleStyle <> pushLeft]
              ++ [onClick SinglePlayerGo | isJust maybeTeam]
          )
          "Start"
      return $ div_ [style_ $ "margin-top" =: px (cps * 2)] [button]
    backDivM = do
      let voffset = (length allTeams + 8) * cps & px
      button <-
        textButton
          gui
          zpp
          Enabled
          [style_ $ subtitleStyle <> pushLeft, onClick SinglePlayerBack]
          "Back"
      return $ div_ [style_ $ "margin-top" =: voffset] [button]

selectTeamDiv :: SharedModel -> Int -> Maybe Team -> Styled (View Action)
selectTeamDiv smodel z chosen = do
  -- startButtonDiv <- startButtonDivM
  teamButtons <- sequence [teamButton smodel z chosen t | t <- allTeams]
  return $
    div_
      [style_ flexLineStyle]
      [ div_
          [style_ flexColumnStyle]
          teamButtons
      ]

teamButton ::
  SharedModel ->
  -- The z index
  Int ->
  -- | The selected team, if any
  Maybe Team ->
  -- The team for which to build the button
  Team ->
  Styled (View Action)
teamButton smodel z chosen team = do
  button <- anyButton gui z bState builder
  return $ marginifyhv 2 4 button
  where
    (bState, action) =
      case chosen of
        Nothing -> (Enabled, LobbySelectTeam $ Just team)
        Just t | t == team -> (Selected, LobbySelectTeam Nothing) -- toggle
        Just _ -> (Disabled, LobbySelectTeam $ Just team)
    creature kind team = identToCreature smodel $ CreatureID kind team
    path team kind =
      creature kind team & creatureToFilepath & filepathToString
    tile team =
      ( case team of
          Human -> General
          Undead -> Vampire
      )
        & path team
    textAndTile =
      [ div_ [noDrag] [stytextz z (ms $ ppTeam team)], -- text
        img_' $ ms $ tile team
      ]
    onClickAction = SinglePlayerLobbyAction' action
    builder x =
      div_
        ([style_ flexLineStyle, onClick onClickAction] ++ x)
        $ map (marginifyhv 2 2) textAndTile
