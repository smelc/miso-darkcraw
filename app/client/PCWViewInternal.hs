{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

-- |
-- This module contains generic things to be used in *View.hs
-- and *ViewInternal.hs files. Contrary to 'ViewInternal', it contains
-- things that are specific to the game; not solely pure HTML/CSS stuff.
-- |
module PCWViewInternal
  ( cardBoxShadowStyle,
    cardView,
    cardPositionStyle,
    cardPositionStyle',
    viewFrame,
    CardDrawStyle (..),
    DisplayMode (..),
  )
where

import Card
import Cinema (Actor (..), ActorKind (..), ActorState (..), Direction, Element (..), Frame (..), defaultDirection, spriteToKind)
import Constants
import Data.Function ((&))
import qualified Data.Map.Strict as Map
import Data.Maybe
import Miso
import Miso.String (MisoString, ms)
import Miso.Util ((=:))
import SharedModel (SharedModel, liftSkill, tileToFilepath)
import qualified SharedModel
import Tile
import Update (Action)
import ViewInternal

data DisplayMode = NormalMode | DebugMode

data CardDrawStyle = CardDrawStyle
  { -- | Whether the card should fade in
    fadeIn :: Bool,
    -- | Whether the card is being hovered
    hover :: Bool
  }

instance Semigroup CardDrawStyle where
  CardDrawStyle fadeIn1 hover1 <> CardDrawStyle fadeIn2 hover2 =
    CardDrawStyle (fadeIn1 || fadeIn2) (hover1 || hover2)

instance Monoid CardDrawStyle where
  mempty = CardDrawStyle False False

cardBoxShadowStyle ::
  -- | The (r, g, b) of the border
  (Int, Int, Int) ->
  -- | The width of the border
  Int ->
  -- | The timing-function of the transition
  MisoString ->
  Attribute a
cardBoxShadowStyle (r, g, b) width timingFunction =
  style_ $
    Map.fromList
      [ ("box-shadow", "0 0 0 " <> ms width <> "px " <> rgba r g b),
        ("transition", "box-shadow"),
        ("transition-duration", "0.15s"),
        ("transition-timing-function", timingFunction)
      ]

-- | Div displaying a card
cardView ::
  -- | The z index
  Int ->
  SharedModel ->
  Card Core ->
  CardDrawStyle ->
  Styled (View Action)
cardView z shared card cdsty@CardDrawStyle {fadeIn} =
  if fadeIn
    then
      keyframed
        builder
        (keyframes (animDataName animData) "opacity: 0;" [] "opacity: 1;")
        animData
    else pure $ builder []
  where
    topMargin = cps `div` 4
    picSize =
      case card of
        CreatureCard _ -> cps
        NeutralCard _ -> 16
        _ -> error $ "Unhandled card: " ++ show card
    pictureStyle =
      zplt (z + 1) Absolute ((cardPixelWidth - picSize) `div` 2) topMargin
    filepath =
      SharedModel.unsafeLiftCard shared card
        & SharedModel.cardToFilepath shared
        & filepathToString
        & ms
    pictureCell = imgCell filepath
    builder attrs =
      div_ attrs $
        [div_ [style_ pictureStyle] [pictureCell]]
          ++ cardView' z shared card
          ++ [cardBackground z cdsty]
    animData =
      (animationData "handCardFadein" "1s" "ease")
        { animDataFillMode = Just "forwards"
        }

cardView' :: Int -> SharedModel -> Card Core -> [View Action]
cardView' z shared card =
  -- Note that we don't have this function to take a Card UI, despite
  -- translating 'card' to 'ui' here. The translation is solely for UI
  -- only fields; we don't want to force callers to do it. That was the point
  -- of having SharedModel around.
  case ui of
    -- drawing of Creature cards
    (CreatureCard Creature {attack, hp, skills}) ->
      [div_ [style_ statsStyle] [statsCell]]
        ++ [div_ [style_ skillsStyle] skillsDivs]
      where
        inStatsStyle = flexLineStyle <> fontStyle
        statsCell =
          div_
            [style_ inStatsStyle]
            [ text $ ms hp,
              imgCell assetFilenameHeart,
              text $ ms attack,
              imgCell assetFilenameSword
            ]
        skillsTopMargin = statsTopMargin + fontSize + (fontSize `div` 2)
        skillsHeight = cps * length skills
        skillsStyle =
          "display" =: "flex"
            <> "flex-direction" =: "column"
            <> "align-items" =: "flex-start" -- left
            <> zpltwh (z + 1) Absolute leftMargin skillsTopMargin width skillsHeight
            <> fontStyle
        skillDiv skill = div_ [] [liftSkill shared skill & skillTitle & ms & text]
        skillsDivs = map skillDiv skills
    -- drawing of Neutral cards
    (NeutralCard NeutralObject {ntitle, ntext}) ->
      [ div_
          [ style_ $ zpwh (z + 1) Absolute cardPixelWidth cardPixelHeight,
            style_ fontStyle
          ]
          [ div_
              [ style_ $
                  "align" =: "center"
                    <> "position" =: "absolute"
                    <> "margin-top" =: "24px"
                    <> "transform" =: "translate(-50%,0%)"
                    <> "margin-left" =: "50%"
                    <> "margin-right" =: "50%"
              ]
              [text $ ms ntitle],
            p_
              [ style_ $
                  "position" =: "absolute"
                    <> "margin-top" =: "40px"
                    <> "margin-left" =: px leftMargin
              ]
              [text $ ms ntext]
          ]
      ]
    _ -> error "Unhandled card"
  where
    ui = SharedModel.unsafeLiftCard shared card
    (topMargin, leftMargin) = (cps `div` 4, topMargin)
    statsTopMargin = topMargin * 2 + cps
    statsStyle = zpltwh (z + 1) Absolute leftMargin statsTopMargin width cps
    fontSize = cps `div` 2
    fontStyle =
      "font-size" =: px fontSize
        <> "font-family" =: "serif"
    width = cardPixelWidth - (topMargin * 2)

cardBackground ::
  -- | The z index
  Int ->
  CardDrawStyle ->
  View Action
cardBackground z cdsty =
  div_
    [style_ $ sty1 <> sty2]
    [imgCellwh assetFilenameBeigeBG cardPixelWidth cardPixelHeight]
  where
    sty1 = zpwh z Absolute cardPixelWidth cardPixelHeight
    sty2 =
      if hover cdsty
        then "outline" =: (ms borderSize <> "px solid red")
        else mempty

cardPositionStyle ::
  -- | The horizontal offset from the enclosing container, in number of cells
  Int ->
  -- | The vertical offset from the enclosing container, in number of cells
  Int ->
  Map.Map MisoString MisoString
cardPositionStyle xCellsOffset yCellsOffset =
  cardPositionStyle' (xCellsOffset * cps) (yCellsOffset * cps)

cardPositionStyle' ::
  -- | The horizontal offset from the enclosing container, in pixels
  Int ->
  -- | The vertical offset from the enclosing container, in pixels
  Int ->
  Map.Map MisoString MisoString
cardPositionStyle' xPixelsOffset yPixelsOffset =
  pltwh Absolute xPixelsOffset yPixelsOffset cardPixelWidth cardPixelHeight

-- Now come functions that are about building the view of a Scene
data Context = Context
  { z :: Int,
    paths :: Map.Map CreatureID (Direction -> MisoString),
    shared :: SharedModel
  }

createContext :: Int -> SharedModel -> Context
createContext z shared =
  Context {..}
  where
    paths = map f (SharedModel.getCards shared) & catMaybes & Map.fromList
    f card@(CreatureCard Creature {creatureId}) =
      Just (creatureId, dirToFilename (SharedModel.cardToFilepath shared card))
    f _ = Nothing
    -- Leave 24x24_3_0.png untouched if direction is ToLeft
    dirToFilename filepath dir
      | dir == defaultDirection =
        ms $ filepathToString filepath
    -- Return 24x24_3_1.png untouched if direction is ToRight. This is
    -- where we rely on the right version of a sprite to be one line below
    -- its left version
    dirToFilename f@Filepath {..} _ =
      dirToFilename f {fpY = fpY + 1} defaultDirection

viewFrame :: DisplayMode -> Int -> SharedModel -> Frame -> View a
viewFrame mode z smodel (Frame mapping) =
  div_
    []
    $ mapping
      & Map.toList
      & map (uncurry (viewEntry mode context))
      & concat
  where
    context = createContext z smodel

stateToAttribute :: Int -> ActorState -> Attribute a
stateToAttribute z ActorState {x, y} =
  style_ $
    pltwh Absolute (x * cps) (y * cps) cps cps
      <> "z-index" =: ms z

viewEntry :: DisplayMode -> Context -> Element -> Actor -> [View a]
viewEntry _ _ _ (Actor _ ActorState {sprite = Nothing}) = []
viewEntry mode Context {..} element (Actor mname state@ActorState {direction, telling, sprite = Just sprite}) =
  [ nodeHtmlKeyed
      "div"
      (Key (ms (show element)))
      ([stateToAttribute (zFor sprite) state] ++ nameTooltip)
      [imgCell path]
  ]
    ++ [ div_ [bubbleStyle state] [text $ ms $ fromJust telling]
         | isJust telling
       ]
  where
    (zpp, zpppp, zppPP) = (z + 1, zpp + 1, zpppp + 1)
    path = case sprite of
      Left cid ->
        case paths Map.!? cid of
          Nothing -> error $ "CreatureID has no corresponding filename: " ++ show cid
          Just dirToPath -> dirToPath direction
      Right tile -> tileToFilepath shared tile & filepathToString & ms
    bubbleStyle ActorState {x, y} =
      style_ $
        "position" =: "absolute"
          <> "left" =: px ((x * cps) + cps `div` 2)
          <> "top" =: px ((y - 1) * cps)
          <> "transform" =: "translate(-50%, -50%)" -- Center element
          <> "background-color" =: beigeHTML
          <> "border-radius" =: px 2 -- rounded corners
          <> "z-index" =: ms zppPP -- on top of everything
          <> "width" =: "fit-content" -- make box exactly the size of the text
    zFor sprite =
      case spriteToKind sprite of
        CreatureKind -> zpppp
        TileKind -> zpp
    nameTooltip
      | Just name <- mname, DebugMode <- mode = [title_ (ms name)]
      | otherwise = []
