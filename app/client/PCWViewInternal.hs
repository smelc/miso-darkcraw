{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# OPTIONS_GHC -Wno-orphans #-}

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
    DisplayLocation (..),
    DisplayMode (..),
  )
where

import Card
import Cinema (Actor (..), ActorKind (..), ActorState (..), Direction, Element (), Frame (..), defaultDirection, spriteToKind)
import Constants
import Data.Char (toLower)
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.List.Extra
import qualified Data.Map.Strict as Map
import Data.Maybe
import Miso
import Miso.String (MisoString, ToMisoString (..), ms)
import Miso.Util ((=:))
import Nat
import SharedModel (SharedModel, tileToFilepath)
import qualified SharedModel
import Tile
import qualified Total
import Update (Action)
import ViewInternal

data DisplayMode = NormalMode | DebugMode

-- | Where a card is being drawn
data DisplayLocation = GameInPlaceLoc | GameHandLoc | GameDragLoc | DeckLoc

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

instance Miso.String.ToMisoString Nat where
  toMisoString = toMisoString . show

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

-- | Div displaying a card. Draws the picture and the background
-- and delegate most stuff to 'cardView''
cardView ::
  DisplayLocation ->
  -- | The z index
  Int ->
  SharedModel ->
  -- | The team of the card, for selecting the background
  Team ->
  Card 'Core ->
  CardDrawStyle ->
  Styled (View Action)
cardView loc z shared team card cdsty@CardDrawStyle {fadeIn} =
  if fadeIn
    then
      keyframed
        builder
        (keyframes (animDataName animData) "opacity: 0;" [] "opacity: 1;")
        animData
    else pure $ builder []
  where
    avatarPicStyle =
      zplt (z + 1) Absolute ((cardPixelWidth - picSize id) `div` 2) picTopMargin
    id = Card.cardToIdentifier card
    manaTextStyle =
      zplt (z + 1) Absolute (cps `div` 4) (cps `div` 6)
        <> mkFontStyle skillFontSize
    manaColorStyle = "color" =: "#2087BA"
    mana = uiCard <&> Card.toCommon <&> Card.mana <&> ms & fromMaybe (ms ("?" :: String))
    drawMana = case loc of GameHandLoc -> True; DeckLoc -> True; _ -> False
    uiCard = SharedModel.mlift shared card
    filepath =
      uiCard
        <&> SharedModel.cardToFilepath shared
        & fromMaybe default24Filepath
    avatarPicCell = imgCell $ ms $ filepathToString filepath
    manaDiv =
      case drawMana of
        False -> []
        True ->
          pure $
            div_
              [style_ manaTextStyle, style_ manaColorStyle]
              [Miso.text mana]
    builder attrs =
      div_ (attrs ++ extraAttrs) $
        [div_ [style_ avatarPicStyle] [avatarPicCell]]
          ++ manaDiv
          ++ cardView' z shared card
          ++ [PCWViewInternal.cardBackground z team cdsty]
    animData =
      (animationData "handCardFadein" "1s" "ease")
        { animDataFillMode = Just "forwards"
        }
    extraAttrs =
      case card of
        CreatureCard {} ->
          [ style_ $
              "overflow-y" =: "auto"
                <> scrollbarStyle
                <> "height" =: px (cardPixelHeight - picTopMargin - picSize id)
          ]
        (NeutralCard {}) -> [] -- Done in itemNeutralView
        (ItemCard {}) -> [] -- Done in itemNeutralView

picTopMargin :: Int
picTopMargin = cps `div` 4

scrollbarStyle :: Map.Map MisoString MisoString
scrollbarStyle =
  "scrollbar-width" =: "thin"
    <> "scrollbar-color" =: "#9d9fa0 #333333"

cardView' :: Int -> SharedModel -> Card 'Core -> [View Action]
cardView' z shared card =
  -- Note that we don't have this function to take a Card UI, despite
  -- translating 'card' to 'ui' here. The translation is solely for UI
  -- only fields; we don't want to force callers to do it. That was the point
  case (card, ui) of
    -- drawing of Creature cards
    (CreatureCard _ core@Creature {skills}, CreatureCard _ Creature {hp, items}) ->
      [div_ [style_ statsStyle] [statsCell]]
        ++ [div_ [style_ skillsStyle] skillsDivs]
        ++ itemDivs
      where
        inStatsStyle = flexLineStyle <> mkFontStyle skillFontSize
        statsCell =
          div_
            [style_ inStatsStyle]
            [ Miso.text $ ms hp,
              imgCell assetFilenameHeart,
              Miso.text $ ms $ Total.attack core,
              imgCell assetFilenameSword
            ]
        skillsTopMargin = statsTopMargin + skillFontSize + (skillFontSize `div` 2)
        skillsHeight = cps * length skills
        skillsStyle =
          "display" =: "flex"
            <> "flex-direction" =: "column"
            <> "align-items" =: "flex-start" -- left
            <> zpltwh (z + 1) Absolute leftMargin skillsTopMargin width skillsHeight
            <> mkFontStyle skillFontSize
        skillsDivs = map (skillDiv shared) skills
        itemDivs =
          map (\(i, item) -> itemDiv zpp shared i item) $ zip [0 ..] items
    -- drawing of Item cards
    (_, ItemCard _ ItemObject {ititle = title, ititleSzOffset = titleSzOffset, itext = text, itextSzOffset = textSzOffset}) ->
      [itemNeutralView zpp card INViewInput {fontStyle = mkFontStyle offFontSize, ..}]
      where
        -- We don't distinguish textSzOffset and titleSzOffset, we just
        -- take the largest offset.
        offFontSize = skillFontSize + min textSzOffset titleSzOffset
    -- drawing of Neutral cards
    (_, NeutralCard _ NeutralObject {ntitle = title, ntext = text}) ->
      [itemNeutralView zpp card INViewInput {fontStyle = mkFontStyle skillFontSize, ..}]
    (core, ui) ->
      error $ "Wrong core/ui combination: " ++ show core ++ "/" ++ show ui
  where
    ui :: Card 'UI =
      case SharedModel.mlift shared card of
        Nothing -> error $ "Cannot lift card: " ++ show card
        Just c -> c
    (topMargin, leftMargin) = (cps `div` 4, topMargin)
    statsTopMargin = topMargin * 2 + cps
    statsStyle = zpltwh (z + 1) Absolute leftMargin statsTopMargin width cps
    width = cardPixelWidth - (topMargin * 2)
    zpp = z + 1

-- The size of the text in the skill stats, and in other places too
skillFontSize :: Int
skillFontSize = cps `div` 2

mkFontStyle :: Int -> Map.Map MisoString MisoString
mkFontStyle fontSize = "font-size" =: px fontSize <> "font-family" =: "serif"

data INViewInput = INViewInput
  { fontStyle :: Map.Map MisoString MisoString,
    leftMargin :: Int,
    text :: String,
    title :: String
  }

-- | Draws an item or neutral card
itemNeutralView ::
  Int -> Card 'Core -> INViewInput -> View a
-- itemNeutralView z card fontStyle leftMargin title txt =
itemNeutralView z card INViewInput {fontStyle, text, title} =
  div_
    [ style_ $ zpwh (z + 1) Absolute cardPixelWidth cardPixelHeight,
      style_ $ fontStyle <> flexColumnStyle,
      style_ $
        "top" =: px cps
          <> "left" =: px 4 -- So that text doesn't overlap card border
          <> "width" =: px (cardPixelWidth - (4 * 2))
          <> "height" =: px (cardPixelHeight - picTopMargin - (picSize $ Card.cardToIdentifier card))
          <> "overflow-y" =: "auto"
          <> scrollbarStyle
    ]
    [ Miso.text $ ms title,
      div_
        [style_ $ "margin-top" =: px (cps `div` 4)]
        [Miso.text $ ms $ typeset text]
    ]

skillDiv :: SharedModel -> SkillCore -> View a
skillDiv shared skill =
  div_ [style_ color, hover] [label & ms & Miso.text]
  where
    ui = Card.liftSkill skill & SharedModel.liftSkill shared
    color =
      case skill of
        DrawCard' False -> "color" =: grey
        Blow' False -> "color" =: grey
        Blow' True -> "color" =: greenHTML
        Fear' False -> "color" =: grey
        Terror' False -> "color" =: grey
        _ | isStupid skill -> "color" =: redHTML
        _ -> mempty
      where
        grey = "#555555"
    label =
      case skill of
        Stupid4' i -> skillTitle ui ++ " " ++ show (i + 1) ++ "/4"
        _ -> skillTitle ui
    hover = title_ $ ms (skillText ui & typeset)

-- | Div to show an item on a creature in place
itemDiv :: Int -> SharedModel -> Int -> ItemObject 'UI -> View a
itemDiv z shared i iobj@ItemObject {item} =
  div_ [style_ itemStyle] [pictureCell]
  where
    itemStyle =
      "z-index" =: ms z
        <> "position" =: "absolute"
        <> "left" =: px (cardPixelWidth - (imgSize `div` 2))
        <> "top" =: px (i * (imgSize + (imgSize `div` 4)))
    id = IDI item
    imgSize = picSize id
    card = ItemCard (SharedModel.unsafeToCardCommon shared id) iobj
    filepath =
      SharedModel.cardToFilepath shared card
        & filepathToString
        & ms
    pictureCell = imgCellwh filepath imgSize imgSize

picSize :: Card.ID -> Int
picSize id = case id of IDC {} -> cps; IDN {} -> 16; IDI {} -> 16

typeset :: String -> String
typeset x = go x replacements
  where
    go x [] = x
    go x ((src, img) : rest) = go (replace src img x) rest
    replacements = [(":heart:", "❤️"), (":crossed_swords:", "⚔️")]

cardBackground ::
  -- | The z index
  Int ->
  Team ->
  CardDrawStyle ->
  View Action
cardBackground z team cdsty =
  div_
    [style_ $ sty1 <> sty2]
    [ imgCellwh
        (Constants.cardBackground (ms . stringToLower $ show team))
        cardPixelWidth
        cardPixelHeight
    ]
  where
    sty1 = zpwh z Absolute cardPixelWidth cardPixelHeight
    sty2 =
      if hover cdsty
        then "outline" =: (ms borderSize <> "px solid red")
        else mempty
    stringToLower str = [toLower c | c <- str]

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
    f card@(CreatureCard _ Creature {creatureId}) =
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
    ++ [ div_ [bubbleStyle state] [Miso.text $ ms $ fromJust telling]
         | isJust telling
       ]
  where
    (zpp, zpppp, zppPP) = (z + 1, zpp + 1, zpppp + 1)
    path = case sprite of
      Left cid ->
        case paths Map.!? cid of
          Nothing -> error $ "CreatureID has no corresponding filename: " ++ show cid
          Just dirToPath -> dirToPath direction
      Right tile -> tileToFilepath shared tile Tile.TwentyFour & filepathToString & ms
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
