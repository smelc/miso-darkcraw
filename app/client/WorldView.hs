{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Module displaying the world map, which is the first view
-- in legendary edition as well as the view between levels.
module WorldView where

import Card (Team (..))
import qualified Color
import qualified Configuration
import qualified Constants
import qualified Data.Bifunctor as Bifunctor
import Data.Function ((&))
import Data.List.Extra
import qualified Data.Map.Strict as Map
import qualified Direction
import Miso
import qualified Miso.String as MisoString
import qualified Model
import Nat
import qualified Network
import qualified Roads
import qualified Shared
import qualified Tile
import qualified Update
import ViewInternal (Position (..), Styled, px)
import qualified ViewInternal

viewWorldModel :: Model.World -> Styled (View Update.Action)
viewWorldModel world@Model.World {encounters, position, shared, team} = do
  let man = tileView zpp manX manY manTile
      builder attrs =
        div_
          []
          [ div_ (attrs ++ [style_ bgStyle]) ([man] ++ events ++ chooseTeamHint zpp world),
            legendDiv
          ]
  ViewInternal.fade builder Nothing 2 fade
  where
    (z, zpp) = (0, z + 1)
    fade = if Configuration.isDev then Constants.DontFade else Constants.FadeIn
    bgStyle =
      ViewInternal.zpltwh z Relative 0 0 pxWidth pxHeight
        <> "background-image" =: Constants.assetsUrl "world.png"
        <> "background-position-x" =: px (-(Constants.cps * 13))
        <> "background-position-y" =: px pxHeight
    (pxHeight, pxWidth) = (cellsHeight * Constants.cps, cellsWidth * Constants.cps)
    tileView z x y tile =
      div_
        [style_ $ ViewInternal.zpltwh z ViewInternal.Absolute x y Constants.cps Constants.cps]
        [ViewInternal.imgCellwh (filepath tile Tile.TwentyFour) Constants.cps Constants.cps Nothing]
    (manX, manY) = absCoordToPx world position
    manTile =
      case team of
        Nothing -> Tile.Man
        Just t -> encounterToTile (Model.Select t)
    filepath tile size =
      Shared.tileToFilepath shared tile size
        & Tile.filepathToString
        & MisoString.ms
    encounterFilter =
      case team of
        Nothing ->
          -- Show selection spots if team not chosen yet
          const True
        Just _ ->
          -- Don't show selection spots if team chosen already
          \case Model.Select _ -> False; _ -> True
    events =
      encounters
        & Map.filter encounterFilter
        & Map.toList
        & map (Bifunctor.first (absCoordToPx world))
        & map (\((x, y), encounter) -> tileView zpp x y (encounterToTile encounter))

-- | The div showing the legend for the character
legendDiv :: View a
legendDiv =
  div_
    [ style_ $
        "width" =: px Constants.lobbiesPixelWidth
          <> "height" =: px (Constants.cps * 2)
          <> "outline" =: "1px solid red"
          <> "margin-top" =: px 1
          <> "border-radius" =: px borderRadius
    ]
    [ div_
        [ style_ ViewInternal.flexColumnStyle,
          style_ $
            "width" =: px (Constants.lobbiesPixelWidth - Constants.cps * 2)
              <> "margin-left" =: px Constants.cps
        ]
        [ div_ [] [text "The character above with a green cape, that's you!"],
          div_ [style_ $ "margin-top" =: px 4] [text "Move using the pad or the arrow keys."]
        ]
    ]

-- | Converts an absolute coordinate like @(25,43)@ to relative pixels
absCoordToPx :: Model.World -> Direction.Coord -> (Int, Int)
absCoordToPx Model.World {topLeft} c = c Direction.- topLeft & relCoordToPx

-- | Converts a relative coordinate like @(5,4)@ to relative pixels
relCoordToPx :: Direction.Coord -> (Int, Int)
relCoordToPx c = c & Direction.unCoord & both (\n -> Nat.natToInt n * Constants.cps)

-- | The height of the view, in number of cells
cellsHeight :: Int
cellsHeight = Constants.lobbiesPixelHeight `div` Constants.cps

-- | The width of the view, in number of cells
cellsWidth :: Int
cellsWidth = Constants.lobbiesCellWidth

mkModel :: Shared.Model -> Model.World
mkModel shared =
  Model.World {..}
  where
    encounters = mkEncounters (team == Nothing) topLeft (Direction.Coord size)
    moved = False
    position = Direction.Coord (24, 47) -- Initial position of character
    team :: Maybe Team = Nothing
    topLeft = Direction.Coord (13, 22)
    topology = Network.mkTopology $ concat Roads.points
    size = both Nat.intToNat (cellsWidth, cellsHeight)

both :: Bifunctor.Bifunctor p => (a -> d) -> p a a -> p d d
both f = Bifunctor.bimap f f

-- | Shifts the world's position by the given direction, if possible
shift :: Direction.T -> Model.World -> Model.World
shift dir m@Model.World {position, size = (width, height), topLeft = Direction.Coord (tlx, tly)} =
  case Direction.move dir position of
    Nothing -> m
    Just pos'@(Direction.Coord (x, y))
      | x < tlx -> m -- Too much to the left
      | x >= tlx + width -> m -- Too much to the right
      | y < tly -> m -- Too up
      | y >= tly + height -> m -- Too low
      | otherwise -> m {Model.position = pos'}

-- | The position of fights, hardcoded yes
fightSpots :: Map.Map Team [Direction.Coord]
fightSpots =
  Map.map (map Direction.Coord) $
    Map.fromList $
      [ (Undead, [(24, 35)]),
        (Sylvan, [(30, 38)])
      ]

-- | The position where to choose the team
chooseTeamSpots :: Map.Map Team Direction.Coord
chooseTeamSpots =
  [ (Human, (22, 43)),
    (Sylvan, (24, 43)),
    (Evil, (26, 43)),
    (Undead, (28, 43))
  ]
    & map (Bifunctor.second Direction.Coord)
    & Map.fromList

borderRadius :: Int
borderRadius = 6

chooseTeamHint :: Int -> Model.World -> [View a]
chooseTeamHint z Model.World {team, topLeft, size = (width, _)} =
  case team of
    Nothing ->
      pure $
        div_
          [ style_ $
              "z-index" =: MisoString.ms z
                <> "position" =: "absolute"
                <> "top" =: px top
                <> "right" =: px right
                <> "outline" =: "1px solid white"
                <> "border-radius" =: px borderRadius
          ]
          [ div_
              [style_ $ "color" =: Color.html Color.white]
              [ div_
                  [style_ ViewInternal.flexLineStyle]
                  [ div_ [] [text "Choose your team by going to one of these"],
                    div_ [] [text "→"]
                  ]
              ]
          ]
      where
        (right, top) =
          teamSpot Direction.- topLeft
            & (\(Direction.Coord (x, y)) -> Direction.Coord ((width - x), y))
            & relCoordToPx
            & Bifunctor.first (+ Constants.cps `div` 4)
        teamSpot =
          chooseTeamSpots
            & Map.elems
            & sortOn (fst . Direction.unCoord)
            & head
    Just _ ->
      -- Team has been chosen already, do not show hint
      []

mkEncounters :: Bool -> Direction.Coord -> Direction.Coord -> Map.Map Direction.Coord Model.Encounter
mkEncounters includeChoices topLeft size =
  Map.fromList [(c, Model.Fight t) | (t, cs) <- pairs, c <- cs]
    <> (if includeChoices then Map.map Model.Select (Map.fromList choices) else mempty)
  where
    visible c = c >= topLeft && c < topLeft Direction.+ size
    pairs :: [(Team, [Direction.Coord])] =
      fightSpots & Map.map (filter visible) & Map.filter notNull & Map.toList
    choices = [(c, t) | (t, c) <- Map.toList chooseTeamSpots]

encounterToTile :: Model.Encounter -> Tile.Tile
encounterToTile =
  \case
    Model.Fight Beastmen -> Tile.BeastmenDefender
    Model.Fight Evil -> Tile.Beholder
    Model.Fight Human -> Tile.HumanGeneral
    Model.Fight Sylvan -> Tile.SylvanArcher
    Model.Fight Undead -> Tile.UndeadWarrior
    Model.Fight ZKnights -> Tile.ZKnight
    Model.Pickup _item -> Tile.Chest
    Model.Select t -> encounterToTile (Model.Fight t)
