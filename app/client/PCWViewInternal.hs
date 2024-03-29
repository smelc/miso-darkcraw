{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
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
    noCardView,
    scrollbarStyle,
    tileCell,
    viewFrame,
    BorderOverlay (..),
    CardDrawStyle (..),
    DisplayLocation (..),
    DisplayMode (..),
  )
where

import Card
import Cinema (Actor (..), ActorKind (..), ActorState (..), Element (), Frame (..), defaultDirection, spriteToKind)
import qualified Cinema
import qualified Color
import Constants
import Damage (Damage)
import Data.Char (toLower)
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.List.Extra
import qualified Data.Map.Strict as Map
import Data.Maybe
import qualified Effect
import qualified Mana
import Miso
import Miso.String (MisoString, ToMisoString (..), ms)
import Nat
import qualified Shared
import qualified Skill
import Tile (Tile)
import qualified Tile
import qualified Total
import Update (Action)
import ViewInternal

data DisplayMode = NormalMode | DebugMode

type ManaShower = Mana.Mana -> String

-- | Where a card is being drawn
data DisplayLocation
  = GameApplicationLoc ManaShower
  | GameInPlaceLoc ManaShower Total.Place
  | GameHandLoc ManaShower
  | -- | TODO @smelc remove?
    GameDragLoc ManaShower
  | DeckLoc ManaShower
  | LootLoc ManaShower

toManaShower :: DisplayLocation -> ManaShower
toManaShower = \case
  GameApplicationLoc f -> f
  GameInPlaceLoc f _ -> f
  GameHandLoc f -> f
  GameDragLoc f -> f
  DeckLoc f -> f
  LootLoc f -> f

toPlace :: DisplayLocation -> Maybe Total.Place
toPlace =
  \case
    GameApplicationLoc {} -> Nothing
    GameInPlaceLoc _ place -> Just place
    GameHandLoc {} -> Nothing
    GameDragLoc {} -> Nothing
    DeckLoc {} -> Nothing
    LootLoc {} -> Nothing

-- | The overlay to show the selected card in the 'LootView'
data BorderOverlay
  = -- | Blue overlay
    Blue
  | -- | Green overlay
    Green
  | -- | No border. Used to avoid having to wrap values in 'Maybe'
    None
  | -- | Yellow overlay
    Yellow

instance Semigroup BorderOverlay where
  None <> _ = None
  _ <> None = None
  left <> _ = left

instance Monoid BorderOverlay where
  mempty = None

data CardDrawStyle = CardDrawStyle
  { -- | Whether the card should fade in
    fade :: Constants.Fade,
    -- | Whether the card is being hovered
    hover :: Bool,
    -- | Whether to show a green overlay border. Used for cards picked
    -- from the rewards in 'LootView'.
    overlay :: BorderOverlay,
    -- | Whether the card is selected
    selected :: Bool
  }

instance Semigroup CardDrawStyle where
  CardDrawStyle fadeIn1 hover1 o1 s1 <> CardDrawStyle fadeIn2 hover2 o2 s2 =
    CardDrawStyle (fadeIn1 <> fadeIn2) (hover1 || hover2) (o1 <> o2) (s1 || s2)

instance Monoid CardDrawStyle where
  mempty =
    let (fade :: Constants.Fade, hover, overlay, selected) = (mempty, False, None, False)
     in CardDrawStyle {..}

instance Miso.String.ToMisoString Nat where
  toMisoString = toMisoString . show

instance Miso.String.ToMisoString Damage where
  toMisoString = toMisoString . show

-- TODO @smelc return style_ [] if @width == 0@?
cardBoxShadowStyle ::
  -- | The (r, g, b) of the border. FIXME @smelc change to Nat, Nat, Nat
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
  Shared.Model ->
  -- | The team of the card, for selecting the background.
  Team ->
  Card 'Core ->
  CardDrawStyle ->
  Styled (View Action)
cardView loc z shared team card cdsty@CardDrawStyle {fade} =
  ViewInternal.fade builder Nothing 1 fade
  where
    avatarPicStyle =
      zplt (z + 1) Absolute ((cardPixelWidth - picSize id) `div` 2) picTopMargin
    id = Card.toIdentifier card
    manaTextStyle =
      zplt (z + 1) Absolute (cps `div` 4) (cps `div` 6)
        <> mkFontStyle skillFontSize
    manaColorStyle = "color" =: "#2087BA"
    mana = uiCard <&> Card.toCommon <&> Card.mana <&> manaShower <&> ms & fromMaybe (ms ("?" :: String))
    drawMana = case loc of GameHandLoc {} -> True; DeckLoc {} -> True; _ -> False
    manaShower = toManaShower loc
    uiCard = Shared.mlift shared card
    filepath =
      uiCard
        <&> Shared.cardToFilepath shared
        & fromMaybe Tile.default24Filepath
    avatarPicCell = imgCell $ ms $ Tile.filepathToString filepath
    manaDiv =
      case drawMana of
        False -> []
        True -> [div_ [style_ manaTextStyle, style_ manaColorStyle] [Miso.text mana]]
    builder attrs =
      div_ (attrs ++ extraAttrs) $
        [div_ [style_ avatarPicStyle] [avatarPicCell]]
          ++ manaDiv
          ++ cardView' z shared (toPlace loc) card
          ++ [PCWViewInternal.cardBackground z team cdsty]
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

-- | The view to display on a spot without a creature
noCardView ::
  -- | The z index
  Int ->
  Shared.Model ->
  Effect.T ->
  Styled (View Action)
noCardView z shared Effect.T {fadeOut} = do
  views <- traverse (\tile -> ViewInternal.fade (builder tile) Nothing 1 FadeOut) fadeOut
  return $ div_ [] views
  where
    builder tile attrs =
      div_
        ( [ style_ $ flexColumnStyle,
            style_ $ zpwh z Absolute cardPixelWidth cardPixelHeight
          ]
            ++ attrs
        )
        [tileCell shared Tile.TwentyFour tile]

picTopMargin :: Int
picTopMargin = cps `div` 4

scrollbarStyle :: Map.Map MisoString MisoString
scrollbarStyle =
  "scrollbar-width" =: "thin"
    <> "scrollbar-color" =: "#9d9fa0 #333333"
    <> "overflow-x" =: "hidden" -- No horizontal scrollbar

-- | The 'Place' argument is where the @Card 'Core@ is, if any.
cardView' :: Int -> Shared.Model -> Maybe Total.Place -> Card 'Core -> [View Action]
cardView' z shared part card =
  -- Note that we don't want this function to take a Card UI value, despite
  -- translating 'card' to 'ui' here. The translation is solely for UI
  -- only fields; we don't want to force callers to do it.
  case (card, ui) of
    -- drawing of Creature cards
    (CreatureCard _ core@Creature {skills}, CreatureCard CardCommon {text, textSzOffset} Creature {hp, items}) ->
      [div_ [style_ statsStyle] [statsCell]]
        ++ (maybeToList $ textDiv <$> text) -- Exclusive with the next one (tested)
        ++ [div_ [style_ skillsStyle] skillsDivs] -- Exclusive with the previous one (tested)
        ++ itemDivs
      where
        inStatsStyle = flexLineStyle <> mkFontStyle skillFontSize
        statsCell =
          div_
            [style_ inStatsStyle]
            [ Miso.text $ ms hp,
              imgCell assetFilenameHeart,
              div_ [style_ attackStyle] [Miso.text $ ms $ actualAttack],
              imgCell assetFilenameSword
            ]
        (actualAttack, formalAttack) = (Total.attack part core, Card.attack core)
        attackStyle =
          ( case compare actualAttack formalAttack of
              EQ -> Nothing
              LT -> Just $ Color.html Color.red
              GT -> Just $ Color.html Color.green
          )
            & ( \case
                  Nothing -> mempty
                  Just color -> "color" =: color <> "display" =: "inline-block"
              )
        textDiv text =
          div_
            [ style_ $
                zpltwh
                  (z + 1)
                  Absolute
                  4 -- left
                  skillsTopMargin -- top, works because text and skills are exclusive
                  (cardPixelWidth - (4 * 2)) -- width
                  (cardPixelHeight - skillsTopMargin - 2) -- height, last 2 is margin from bottom
                  <> "overflow-y" =: "auto"
                  <> scrollbarStyle
                  <> mkFontStyle (skillFontSize + textSzOffset)
            ]
            [Miso.text $ ms $ typeset text]
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
    (_, ItemCard (cc@CardCommon {text}) ItemObject {title, titleSzOffset}) ->
      itemNeutralGo text title $ mkFontStyle $ mkFontSize cc titleSzOffset
    -- drawing of Neutral cards
    (_, NeutralCard (cc@CardCommon {text}) NeutralObject {title, titleSzOffset}) ->
      itemNeutralGo text title $ mkFontStyle $ mkFontSize cc titleSzOffset
    (core, ui) ->
      error $ "Wrong core/ui combination: " ++ show core ++ "/" ++ show ui
  where
    ui :: Card 'UI =
      case Shared.mlift shared card of
        Nothing -> error $ "Cannot lift card: " ++ show card
        Just c -> c
    (topMargin, leftMargin) = (cps `div` 4, topMargin)
    statsTopMargin = topMargin * 2 + cps
    statsStyle = zpltwh (z + 1) Absolute leftMargin statsTopMargin width cps
    width = cardPixelWidth - (topMargin * 2)
    zpp = z + 1
    itemNeutralGo text title fontStyle =
      maybeToList $ case text of
        Nothing -> Nothing
        Just text -> Just $ itemNeutralView zpp card INViewInput {..}
    mkFontSize CardCommon {textSzOffset} titleSzOffset =
      -- We don't distinguish textSzOffset and titleSzOffset, we just
      -- take the largest offset.
      skillFontSize + (min textSzOffset titleSzOffset)

-- The size of the text in the skill stats, and in other places too
skillFontSize :: Int
skillFontSize = cps `div` 2

mkFontStyle :: Int -> Map.Map MisoString MisoString
mkFontStyle fontSize = "font-size" =: px fontSize <> "font-family" =: "serif"

-- | Data to typeset item and neutral cards
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
          <> "height" =: px (cardPixelHeight - picTopMargin - (picSize $ Card.toIdentifier card))
          <> "overflow-y" =: "auto"
          <> scrollbarStyle
    ]
    [ Miso.text $ ms title,
      div_
        [style_ $ "margin-top" =: px (cps `div` 4)]
        [Miso.text $ ms $ typeset text]
    ]

skillDiv :: Shared.Model -> Skill.State -> View a
skillDiv shared skill =
  div_ [style_ color, hover] [label & ms & Miso.text]
  where
    Skill.Pack {text, title} = Skill.lift skill & Shared.liftSkill shared
    color =
      ( case skill of
          Skill.DrawCard False -> Just Color.grey
          Skill.Block False -> Just Color.grey
          Skill.Blow False -> Just Color.grey
          Skill.Blow True -> Just Color.green
          Skill.Fear False -> Just Color.grey
          Skill.Terror False -> Just Color.grey
          Skill.GreenAffinity False -> Just Color.grey
          Skill.Growth False -> Just Color.grey
          Skill.Slow False -> Just Color.grey
          _ | Skill.isStupid skill -> Just Color.red
          _ -> Nothing
      )
        <&> Color.html
        & (\case Nothing -> mempty; Just color -> "color" =: color)
    label =
      case skill of
        Skill.Bleed i -> title ++ " " ++ show i
        Skill.Stupid4 i -> title ++ " " ++ show (i + 1) ++ "/4"
        _ -> title
    hover = title_ $ ms $ typeset text

-- | Div to show an item on a creature in place
itemDiv :: Int -> Shared.Model -> Int -> ItemObject 'UI -> View a
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
    card = ItemCard (Shared.unsafeToCardCommon shared id) iobj
    filepath =
      Shared.cardToFilepath shared card
        & Tile.filepathToString
        & ms
    pictureCell = imgCellwh filepath imgSize imgSize Nothing

picSize :: Card.ID -> Int
picSize id = case id of IDC {} -> cps; IDN {} -> 16; IDI {} -> 16

typeset :: String -> String
typeset x = go x replacements
  where
    go x [] = x
    go x ((src, img) : rest) = go (replace src img x) rest
    replacements = [(":heart:", "❤️"), (":crossed_swords:", "⚔️"), (":droplet:", "💧")]

cardBackground ::
  -- | The z index
  Int ->
  Team ->
  CardDrawStyle ->
  View Action
cardBackground z team CardDrawStyle {overlay, hover, selected} =
  div_
    [style_ $ posSty <> borderSty]
    $ [mkImgCellwh (Constants.cardBackground (ms . stringToLower $ show team))]
      ++ (maybeToList $ mkImgCellwh <$> overlayToFilename overlay)
  where
    mkImgCellwh filename = imgCellwh filename cardPixelWidth cardPixelHeight (Just Absolute)
    posSty = zpwh z Absolute cardPixelWidth cardPixelHeight
    borderSty =
      case color of Nothing -> mempty; Just c -> "outline" =: (ms borderSize <> "px solid " <> c)
      where
        color =
          case (hover, selected) of
            -- If both hovered and selected, selection has precedence
            (_, True) -> Just $ Color.html Color.selection
            (True, _) -> Just $ Color.html Color.hover
            _ -> Nothing
    stringToLower str = [toLower c | c <- str]
    overlayToFilename :: BorderOverlay -> Maybe MisoString
    overlayToFilename = \case
      None -> Nothing
      Green -> go "green"
      Blue -> go "blue"
      Yellow -> go "yellow"
      where
        go middle = Just ("card-" <> middle <> "-fat-border.png")

-- | The position of a card, from its enclosing container. Sizes are in
-- number of cells.
cardPositionStyle ::
  -- | The horizontal offset from the enclosing container, in number of cells
  Int ->
  -- | The vertical offset from the enclosing container, in number of cells
  Int ->
  Map.Map MisoString MisoString
cardPositionStyle xCellsOffset yCellsOffset =
  cardPositionStyle' (xCellsOffset * cps) (yCellsOffset * cps)

-- | The position of a card, from its enclosing container. Sizes are in pixels.
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
    paths :: Map.Map CreatureID (Cinema.Direction -> MisoString),
    shared :: Shared.Model
  }

createContext :: Int -> Shared.Model -> Context
createContext z shared =
  Context {..}
  where
    paths = map f (Shared.getCards shared) & catMaybes & Map.fromList
    f card@(CreatureCard _ Creature {creatureId}) =
      Just (creatureId, dirToFilename (Shared.cardToFilepath shared card))
    f _ = Nothing
    -- Leave 24x24_3_0.png untouched if direction is ToLeft
    dirToFilename filepath dir
      | dir == defaultDirection =
          ms $ Tile.filepathToString filepath
    -- Return 24x24_3_1.png untouched if direction is ToRight. This is
    -- where we rely on the right version of a sprite to be one line below
    -- its left version
    dirToFilename f@Tile.Filepath {..} _ =
      dirToFilename f {Tile.fpY = fpY + 1} defaultDirection

viewFrame :: DisplayMode -> Int -> Shared.Model -> Frame -> View a
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

tileCell :: Shared.Model -> Tile.Size -> Tile -> View a
tileCell shared sz tile = imgCell path
  where
    path =
      Shared.tileToFilepath shared tile sz
        & Tile.filepathToString
        & ms

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
      Right tile -> Shared.tileToFilepath shared tile Tile.TwentyFour & Tile.filepathToString & ms
    bubbleStyle ActorState {x, y} =
      style_ $
        "position" =: "absolute"
          <> "left" =: px ((x * cps) + cps `div` 2)
          <> "top" =: px ((y - 1) * cps)
          <> "transform" =: "translate(-50%, -50%)" -- Center element
          <> "background-color" =: Color.html Color.beige
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
