{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Module containing instances of 'ToExpr', to reduce the size
-- of 'Update'
module Exprs where

import AI
import Board
import qualified Campaign
import Card
import qualified Cinema
import Data.TreeDiff
import qualified Game
import Model
import SharedModel
import Skill (Skill)
import qualified Skill
import qualified Spots
import System.Random (StdGen)
import qualified Tile
import Turn (Turn)

instance ToExpr CreatureKind

instance ToExpr Team

instance ToExpr CreatureID

instance ToExpr Skill

instance ToExpr Skill.State

instance ToExpr Neutral

instance ToExpr Item

instance ToExpr (ItemObject 'Core)

instance ToExpr (ItemObject 'UI)

instance ToExpr Tile.Filepath

instance ToExpr (Creature 'Core)

instance ToExpr (Creature 'UI)

instance ToExpr (NeutralObject 'Core)

instance ToExpr (NeutralObject 'UI)

instance ToExpr (CardCommon 'Core)

instance ToExpr (CardCommon 'UI)

instance ToExpr (Card 'Core)

instance ToExpr (Card 'UI)

instance ToExpr PlayerSpot

instance ToExpr DeathCause

instance ToExpr InPlaceEffect

instance ToExpr InPlaceEffects

instance ToExpr Card.ID

instance ToExpr (PlayerPart 'Core)

instance ToExpr (PlayerPart 'UI)

instance ToExpr Spots.CardSpot

instance ToExpr (Board 'Core)

instance ToExpr (Board 'UI)

instance ToExpr HandIndex

instance ToExpr HandFiddle

instance ToExpr Game.Target

instance ToExpr (Dragging Game.Target)

instance ToExpr Hovering

instance ToExpr (Interaction Game.Target)

instance ToExpr Turn

instance ToExpr StdGen where toExpr = defaultExprViaShow

instance ToExpr Tile.TileUI

instance ToExpr Skill.Pack

instance ToExpr SharedModel

instance ToExpr Difficulty

instance ToExpr Campaign.Level

instance ToExpr Game.MessageText

instance ToExpr Game.Animation

instance ToExpr GameModel

instance ToExpr PlayingMode

instance ToExpr SinglePlayerLobbyModel

instance ToExpr Cinema.Direction

instance ToExpr Cinema.ActorState

instance ToExpr Tile.Tile

instance ToExpr Cinema.Element

instance ToExpr Cinema.TimedFrame

instance ToExpr Cinema.Actor

instance ToExpr Cinema.Frame

instance ToExpr SceneModel

instance ToExpr WelcomeModel

instance ToExpr MultiPlayerLobbyModel

instance ToExpr MultiPlayerLobbyError

instance ToExpr InvitationActorState

instance ToExpr InvitedActorState

instance ToExpr DeckModel

instance ToExpr Model.Picked

instance ToExpr LootModel

instance ToExpr Model
