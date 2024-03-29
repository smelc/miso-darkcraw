{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Module displaying the world map, which is the first view
-- in legendary edition as well as the view between levels.
module WorldView where

import Card (Team (..), ppTeam)
import qualified Color
import qualified Configuration
import qualified Constants
import qualified Contains
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
import qualified Shared
import qualified Spots
import qualified Theme
import qualified Tile
import qualified Update
import ViewInternal (Position (..), Styled, px)
import qualified ViewInternal

viewWorldModel :: Model.World -> Styled (View Update.Action)
viewWorldModel world@Model.World {encounters, fade, shared, player, topLeft} = do
  let man = tileView shared zpp manX manY manTile
      builder attrs =
        div_
          attrs
          [ div_ ([style_ bgStyle]) ([man] ++ events ++ chooseTeamHint zpp world),
            legendDiv world
          ]
  ViewInternal.fade builder Nothing 2 fadeSty
  where
    (z, zpp) = (0, z + 1)
    fadeSty =
      case (fade, Configuration.isDev) of
        (Constants.DontFade, True) -> Constants.DontFade
        (Constants.DontFade, False) -> Constants.FadeIn
        (fromModel, _) -> fromModel
    bgStyle =
      ViewInternal.zpltwh z Relative 0 0 pxWidth pxHeight
        <> "background-image" =: Constants.assetsUrl "world.png"
        <> "background-position-x" =: px (-(Nat.natToInt topLeftX * Constants.cps))
        <> "background-position-y" =: px (Nat.natToInt (backgroundPositionY topLeftY))
    (topLeftX, topLeftY) = Direction.unCoord topLeft
    (pxHeight, pxWidth) = (cellsHeight * Constants.cps, cellsWidth * Constants.cps)
    (manX, manY) = absCoordToPx world position
    Model.Player {position, pTeam} = player
    manTile =
      case pTeam of
        Nothing -> Tile.Man
        Just t -> encounterToTile (Network.Select t)
    encounterFilter =
      case pTeam of
        Nothing ->
          -- Show selection spots if team not chosen yet
          const True
        Just _ ->
          -- Don't show selection spots if team chosen already
          \case Network.Select _ -> False; _ -> True
    events =
      encounters
        & Map.filter encounterFilter
        & Map.toList
        & map (Bifunctor.first (absCoordToPx world))
        & map (\((x, y), encounter) -> encounterView shared zpp x y encounter)
        & concat

-- | Given the y (in number of cells) of the top left cell, the corresponding
-- background-position-y css attribute for displaying the world map appropriately
backgroundPositionY :: Nat -> Nat
backgroundPositionY =
  \case
    22 -> 624 -- Found experimentally
    0 -> (624 + (22 * (Nat.intToNat Constants.cps))) -- Deduced from first case
    i -> backgroundPositionY 0 - (i * (Nat.intToNat Constants.cps)) -- Most general case

-- | @encounterView shared z x y e@ returns the views for the encounter 'e'.
-- 'x' and 'y' are relative (to the enclosing container) pixel values. They
-- are not cell coordinates.
encounterView :: Shared.Model -> Int -> Int -> Int -> Network.Encounter -> [View action]
encounterView shared z x y e =
  case e of
    Network.Fight _opponent _theme -> default_
    Network.Reward nb ->
      zip3 [x, 4 ..] [y, 4 ..] [z + Nat.natToInt nb, -1 .. z]
        & map (\(x, y, z) -> tileView shared z x y tile)
    Network.Select _ -> default_
  where
    tile = encounterToTile e
    default_ = [tileView shared z x y tile]

filepath :: Shared.Model -> Tile.Tile -> Tile.Size -> MisoString.MisoString
filepath shared tile size =
  Shared.tileToFilepath shared tile size
    & Tile.filepathToString
    & MisoString.ms

tileView :: Shared.Model -> Int -> Int -> Int -> Tile.Tile -> View action
tileView shared z x y tile =
  nodeHtmlKeyed
    "div"
    (Key $ "worldview-" <> MisoString.ms x <> "-" <> MisoString.ms y)
    [style_ $ ViewInternal.zpltwh z ViewInternal.Absolute x y Constants.cps Constants.cps]
    [ ViewInternal.imgCellwh
        (filepath shared tile Tile.TwentyFour)
        Constants.cps
        Constants.cps
        Nothing
    ]

-- | The div showing the legend for the character
legendDiv :: Model.World -> View a
legendDiv Model.World {player} =
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
        [ ( case Model.pTeam player of
              Nothing -> div_ [] [text "The character above with a green cape, that's you!"]
              Just team ->
                div_
                  []
                  [text "You've chosen the ", i_ [] [text $ MisoString.ms $ Card.ppTeam team], text " team 🎉"]
          ),
          div_ [style_ $ "margin-top" =: px 4] [text "Move using the arrow keys or WASD."]
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

-- | The initial model, for when the game starts
mkInitialModel :: Shared.Model -> Model.World
mkInitialModel shared =
  Model.mkWorld shared encounters moved player size
  where
    encounters = mkEncounters (pTeam == Nothing) topLeft
    moved = False
    past = mempty
    pDeck = [] -- Placeholder
    pTeam :: Maybe Team = Nothing
    player = Model.Player {pDeck, pSpot = Spots.startingPlayerSpot, ..}
    position = Direction.Coord (24, 47) -- Initial position of character
    size = modelSize
    topLeft = Model.mkTopLeft position

modelSize :: (Nat, Nat)
modelSize = both Nat.intToNat (cellsWidth, cellsHeight)

both :: Bifunctor.Bifunctor p => (a -> d) -> p a a -> p d d
both f = Bifunctor.bimap f f

-- | Shifts the world's position by the given direction, if possible
shift :: Direction.T -> Model.World -> Model.World
shift dir m@Model.World {player, size = (width, height), topLeft = Direction.Coord (tlx, tly)} =
  case Direction.move dir (Model.position player) of
    Nothing -> m
    Just pos'@(Direction.Coord (x, y))
      | x < tlx -> m -- Too much to the left
      | x >= tlx + width -> m -- Too much to the right
      | y < tly -> m -- Too up
      | y >= tly + height -> m -- Too low
      | otherwise -> m `Contains.with` pos'

borderRadius :: Int
borderRadius = 6

chooseTeamHint :: Int -> Model.World -> [View a]
chooseTeamHint z Model.World {player, topLeft, size = (width, _)} =
  case Model.pTeam player of
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
          Network.chooseTeamSpots
            & Map.elems
            & sortOn (fst . Direction.unCoord)
            & head
    Just _ ->
      -- Team has been chosen already, do not show hint
      []

mkEncounters :: Bool -> Direction.Coord -> Map.Map Direction.Coord Network.Encounter
mkEncounters includeChoices topLeft =
  Map.fromList [(c, Network.Fight t th) | (t, cs) <- fights, (c, th) <- cs]
    <> Map.fromList [(c, Network.Reward n) | (c, n) <- Map.toList rewards]
    <> (if includeChoices then Map.map Network.Select (Map.fromList choices) else mempty)
  where
    visible c = c >= topLeft && c < topLeft Direction.+ (Direction.Coord modelSize)
    fights :: [(Team, [(Direction.Coord, Theme.Kind)])] =
      Network.fightSpots & Map.map (filter (\(c, _) -> visible c)) & Map.filter notNull & Map.toList
    rewards :: Map.Map Direction.Coord Nat =
      Network.lootSpots & Map.filterWithKey (\k _ -> visible k)
    choices = [(c, t) | (t, c) <- Map.toList Network.chooseTeamSpots]

encounterToTile :: Network.Encounter -> Tile.Tile
encounterToTile =
  \case
    Network.Fight t _ -> teamToTile t
    Network.Reward {} -> Tile.Chest
    Network.Select t -> teamToTile t
  where
    teamToTile =
      \case
        Beastmen -> Tile.BeastmenDefender
        Evil -> Tile.Beholder
        Human -> Tile.HumanGeneral
        Sylvan -> Tile.SylvanArcher
        Undead -> Tile.UndeadWarrior
        ZKnights -> Tile.ZKnight
