{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

-- |
-- Module to display an instance of the main view, i.e.
-- the battle between two players. Only this module (and modules that
-- depend on it) can depend on 'GameViewInternal'.
-- |
module GameView where

import Board
import Card
import qualified Configuration
import Constants
import Data.Function ((&))
import Data.Functor
import Data.List
import qualified Data.Map.Strict as Map
import Data.Maybe
import qualified Data.Text as Text
import Debug.Trace (traceShow)
import Event
import qualified Game
import GameViewInternal
import Miso hiding (at)
import Miso.String hiding (concat, intersperse, last, length, map, null, zip)
import Model
import qualified Move
import Nat
import PCWViewInternal
import SharedModel
import Spots hiding (Card)
import qualified Spots
import qualified Total
import Update
import ViewBlocks (dummyOn)
import ViewInternal

-- | Constructs a virtual DOM from a game model
viewGameModel :: GameModel -> Styled (View Action)
viewGameModel model@GameModel {anim, board, shared, interaction, playingPlayer} = do
  boardDiv <- boardDivM
  handDiv <- handDivM
  let divs = [boardDiv, handDiv] ++ if Configuration.isDev then cmdDiv shared else []
  let builder attrs = div_ attrs divs
  ViewInternal.fade builder Nothing 2 $ animToFade anim
  where
    (z, zpp) = (0, z + 1)
    enemySpot = otherPlayerSpot playingPlayer
    application :: Styled (Maybe (View Action)) =
      case anim of
        Game.Application pSpot target card -> do
          c :: View Action <- cardView GameApplicationLoc (zpp + 4) shared team card cdsty
          return $ pure $ div_ [style_ $ mkTargetOffset target] [c]
          where
            cdsty :: CardDrawStyle = mempty {PCWViewInternal.fadeIn = True} -- FIXME @smelc it's fadeOut we want
            team = Board.toPart board pSpot & Board.team
        Game.NoAnimation -> pure Nothing
        Game.Fadeout -> pure Nothing
        Game.Message {} -> pure Nothing
    boardCardsM = boardToInPlaceCells (InPlaceCellContext {z = zpp, mkOffset}) model dragTargetType
    mkOffset pSpot cSpot = cardCellsBoardOffset pSpot cSpot & uncurry cardPositionStyle
    boardDivM = do
      let errs = errView model zpp & maybeToList
      stacks <-
        if Configuration.isDev
          then traverse (stackView model z enemySpot GameViewInternal.Board) [minBound ..]
          else return []
      msg <- messageView anim & maybeToList & sequence
      turn <- turnView model zpp
      scores <- scoreViews model zpp & sequence
      boardCards <- boardCardsM
      apps <- application <&> maybeToList
      let manaView_ = manaView model zpp
      return $
        div_ [style_ boardStyle] $
          concat stacks ++ msg ++ [turn] ++ errs ++ scores ++ boardCards ++ apps ++ [manaView_]
    boardStyle =
      zpltwh z Relative 0 0 boardPixelWidth boardPixelHeight
        <> "background-image" =: assetsUrl "forest.png"
    handDivM = do
      cells <- boardToInHandCells zpp $ toHandDrawingInput model
      stacks <- traverse (stackView model z playingPlayer Hand) [Discarded, Stacked]
      return $ div_ [style_ handStyle] $ cells ++ concat stacks
    handStyle =
      zpltwh z Relative 0 0 handPixelWidth handPixelHeight
        <> "background-image" =: assetsUrl "forest-hand.png"
    dragTargetType =
      case interaction of
        DragInteraction (Dragging (HandIndex i) _) ->
          case Board.toHand board playingPlayer & flip Board.lookupHand i of
            Left err -> traceShow (Text.unpack err) Nothing
            Right id -> Just $ targetType id
        _ -> Nothing

-- | mkTargetOffset returns the offset to display the application of
-- a Neutral card to a creature in place.
mkTargetOffset :: Game.Target -> Map.Map MisoString MisoString
mkTargetOffset target =
  uncurry cardPositionStyle' $ case target of
    Game.PlayerTarget pSpot ->
      (xCells * cps, (yCells * cps) + down)
      where
        -- The card spot from which we go down
        cSpot :: Spots.Card =
          Spots.Top & (case pSpot of Spots.PlayerTop -> id; Spots.PlayerBot -> bottomSpotOfTopVisual)
        (xCells, yCells) :: (Int, Int) = cardCellsBoardOffset pSpot cSpot
        down = (cardCellHeight * cps) `div` 2
    Game.CardTarget pSpot cSpot ->
      ( xPixels + cardPixelWidth `div` 2, -- Shift to the right
        yPixels - cardPixelHeight `div` 2 -- Shift toward the top
      )
      where
        (xCells, yCells) = cardCellsBoardOffset pSpot cSpot
        (xPixels, yPixels) = (xCells * cps, yCells * cps)

cmdDiv :: SharedModel -> [View Action]
cmdDiv shared =
  buttons ++ [doc]
  where
    lift = GameAction'
    buttons =
      [ div_
          [style_ flexLineStyle]
          [ input_
              [ style_ $ "width" =: px (boardPixelWidth - 128),
                type_ "text",
                onInput (lift . Move.UpdateCmd)
              ],
            button_
              [ style_ $ "width" =: px 120,
                onClick $ lift Move.ExecuteCmd
              ]
              [Miso.text "Execute"]
          ]
      ]
    doc =
      div_
        []
        [ div_ [style_ $ "margin-top" =: px 8] [Miso.text "Available commands:"],
          ul_
            []
            [li_ [] [Miso.text $ show cmd & ms] | cmd <- SharedModel.allCommands shared]
        ]

-- | Dumb container to reduce the number of arguments to some functions of
-- this file.
data InPlaceCellContext = InPlaceCellContext
  { -- | The z-index
    z :: Int,
    -- | How to build the style for offsetting a card from the board's origin
    mkOffset :: Spots.Player -> Spots.Card -> Map.Map MisoString MisoString
  }

boardToInPlaceCells ::
  InPlaceCellContext ->
  GameModel ->
  -- | The target to which the card being dragged applies (if any)
  Maybe TargetType ->
  Styled [View Action]
boardToInPlaceCells ctxt@InPlaceCellContext {z} m@GameModel {board} dragTargetType = do
  emitTopLevelStyle $ bumpKeyFrames True
  emitTopLevelStyle $ bumpKeyFrames False
  main <- mainM
  let playerTargets = [boardToPlayerTarget (z + 1) m dragTargetType pSpot | pSpot <- Spots.allPlayers]
  return [div_ [] $ main ++ catMaybes playerTargets]
  where
    mainM =
      sequence
        [ boardToInPlaceCell ctxt m dragTargetType pSpot cSpot
          | (pSpot, cSpot, _) <- Board.toHoleyInPlace board
        ]
    bumpAnim upOrDown = ms $ "bump" ++ (if upOrDown then "Up" else "Down")
    bumpInit = "transform: translateY(0px);"
    sign upOrDown = if upOrDown then "-" else ""
    bump50 upOrDown = "transform: translateY(" ++ sign upOrDown ++ show cellPixelSize ++ "px);"
    bumpKeyFrames upOrDown =
      keyframes (bumpAnim upOrDown) bumpInit [(50, bump50 upOrDown)] bumpInit

boardToInPlaceCell ::
  InPlaceCellContext ->
  GameModel ->
  -- | The target to which the card being dragged applies (if any)
  Maybe TargetType ->
  -- | The part considered
  Spots.Player ->
  -- | The spot of the card to show
  Spots.Card ->
  Styled (View Action)
boardToInPlaceCell InPlaceCellContext {z, mkOffset} m@GameModel {anims, board, interaction, shared} dragTargetType pSpot cSpot =
  nodeHtmlKeyed
    "div"
    (Key $ ms key)
    ( [ style_ $
          Map.fromList bounceStyle
            <> mkOffset pSpot cSpot -- Absolute positioning
            <> "z-index" =: ms z,
        class_ "card",
        cardBoxShadowStyle rgb (borderWidth m target) "ease-in-out"
      ]
        ++ inPlaceEnterLeaveAttrs GameAction'
        ++ (dragDropEvents <$> dragTarget & concat)
    )
    <$> do
      fades <- fadeouts shared (z + 1) attackEffect
      heartw <- heartWobble (z + 1) attackEffect
      heartg <- statChange (z + 1) HitPoint attackEffect
      attackg <- statChange (z + 1) Attack attackEffect
      cards <-
        case maybeCreature of
          Nothing -> pure []
          Just creature -> do
            let cdsty :: CardDrawStyle =
                  mempty
                    { hover = beingHovered,
                      PCWViewInternal.fadeIn = Board.fadeIn attackEffect
                    }
            v <- cardView loc z shared t (CreatureCard mkCoreCardCommon creature) cdsty
            -- "pointer-events: none" turns off handling of drag/drog
            -- events. Without that, on full-fledged cards, children
            -- would trigger GameDragLeave/GameDragEnter events (because the
            -- parent 'v' declares them) which disturbs dropping
            -- of neutral cards.
            let attr =
                  case dragTargetType of
                    -- Not dragging, don't disable nested events: we want hover
                    -- to be handled. Also, for consistency with hand, do
                    -- not make text selectable
                    Nothing -> noDrag
                    -- Dragging: disable nested events (see above)
                    Just _ -> style_ $ "pointer-events" =: "none"
            return $ [div_ [attr] [v]]
      return $ cards ++ [fades] ++ heartw ++ heartg ++ attackg
  where
    part = Board.toPart board pSpot
    t = Board.team part
    loc = GameInPlaceLoc $ Total.Place {place = Board.inPlace part, cardSpot = cSpot}
    key = intersperse "_" ["inPlace", show pSpot, show cSpot] & concat
    maybeCreature = Board.toInPlaceCreature board pSpot cSpot
    inPlaceEnterLeaveAttrs lift =
      -- If dragging we don't need to handle in place hovering
      case (maybeCreature, dragTarget) of
        (Just _, Nothing) ->
          [ onMouseEnter' "card" $ lift $ Move.InPlaceMouseEnter target,
            onMouseLeave' "card" $ lift $ Move.InPlaceMouseLeave target
          ]
        _ -> []
    target = Game.CardTarget pSpot cSpot
    bumpAnim upOrDown = ms $ "bump" ++ (if upOrDown then "Up" else "Down")
    upOrDown = case pSpot of PlayerTop -> False; PlayerBot -> True
    beingHovered = interaction == HoverInPlaceInteraction target
    attackEffect =
      (Board.toInPlace anims pSpot) Map.!? cSpot
        & fromMaybe mempty
    bounceStyle =
      [ ("animation", bumpAnim upOrDown <> " 0.5s ease-in-out")
        | attackBump attackEffect
      ]
    rgb = borderRGB interaction target
    dragTarget =
      case (dragTargetType, maybeCreature) of
        (Just (CardTargetType Hole), Nothing) -> Just target
        (Just (CardTargetType Occupied), Just _) -> Just target
        _ -> Nothing

-- | Whether to show the player target at @pSpot@
boardToPlayerTarget ::
  Int ->
  GameModel ->
  Maybe TargetType ->
  Spots.Player ->
  Maybe (View Action)
boardToPlayerTarget z m@GameModel {interaction} dragTargetType pSpot =
  if bwidth <= 0
    then Nothing
    else
      Just $
        nodeHtmlKeyed
          "div"
          (Key $ ms key)
          ( [ style_ $ posStyle x y <> "z-index" =: ms z, -- Absolute positioning
              cardBoxShadowStyle rgb bwidth "ease-in-out"
            ]
              ++ (dragDropEvents <$> dragTarget & concat)
          )
          []
  where
    key = show pSpot ++ "-target"
    (x, y) = cardCellsBoardOffset pSpot cSpot
    (w, h) = (cardCellWidth * 3 + cardHCellGap * 2, cardCellHeight * 2 + cardVCellGap)
    posStyle x y = pltwh Absolute (x * cps) (y * cps) (w * cps) (h * cps)
    cSpot = case pSpot of PlayerTop -> TopLeft; PlayerBot -> BottomRight
    target = Game.PlayerTarget pSpot
    (rgb, bwidth) = (borderRGB interaction target, borderWidth m target)
    dragTarget =
      case dragTargetType of
        Just PlayerTargetType -> Just target
        _ -> Nothing

-- | The events for placeholders showing drag targets
dragDropEvents :: Game.Target -> [Attribute Action]
dragDropEvents target =
  [ onDragEnter $ lift $ Move.DragEnter target,
    onDragLeave $ lift $ Move.DragLeave target,
    onDrop (AllowDrop True) $ lift Move.Drop,
    dummyOn "dragover"
  ]
  where
    lift = GameAction' . Move.DnD

borderRGB :: Eq a => Interaction a -> a -> (Int, Int, Int)
borderRGB interaction target =
  case interaction of
    DragInteraction Dragging {dragTarget} | dragTarget == Just target -> yellowRGB
    _ -> greenRGB

boardToInHandCells ::
  -- | The z index
  Int ->
  HandDrawingInput ->
  Styled [View Action]
boardToInHandCells z hdi@HandDrawingInput {hand} =
  traverse (boardToInHandCell hdi actionizer bigZ) zicreatures'
  where
    cards = map fst hand
    icreatures = zip cards [HandIndex 0 ..]
    zicreatures = zip [z, z + 2 ..] icreatures
    bigZ = case safeLast zicreatures of Nothing -> z; Just (z', _) -> z' + 2
    safeLast l = if null l then Nothing else Just $ last l
    zicreatures' = map (\(a, (b, c)) -> (a, b, c)) zicreatures
    actionizer =
      HandActionizer
        { onDragStart = Move.DnD . Move.DragStart,
          onDragEnd = Move.DnD Move.DragEnd,
          onMouseEnter = Move.InHandMouseEnter,
          onMouseLeave = Move.InHandMouseLeave
        }
        <&> GameAction'

toHandDrawingInput :: GameModel -> HandDrawingInput
toHandDrawingInput GameModel {interaction = gInteraction, ..} =
  HandDrawingInput {..}
  where
    hand =
      [ (card, fadeIn)
        | (i, card) <-
            zip
              [0 ..]
              ( Board.toHand board playingPlayer
                  & map (Card.unlift . SharedModel.unsafeIdentToCard shared)
              ),
          let fadeIn = i `elem` Board.toHand anims playingPlayer
      ]
    interaction = Just gInteraction
    offseter = id
    part = Board.toPart board playingPlayer
    (mana, team) = (Board.mana part, Board.team part)

data HandActionizer a = HandActionizer
  { -- The event to raise when starting to drag a card
    onDragStart :: HandIndex -> a,
    -- The event to raise when stopping hovering a card
    onDragEnd :: a,
    -- The event to raise when hovering a card
    onMouseEnter :: HandIndex -> a,
    -- The event to raise when stopping hovering a card
    onMouseLeave :: HandIndex -> a
  }
  deriving (Functor)

type HandOffseter = (Int, Int) -> (Int, Int)

data HandDrawingInput = HandDrawingInput
  { -- | The hand, and whether the corresponding card is being faded in
    hand :: [(Card 'Core, Bool)],
    -- | The current interaction, if any. FIXME @smelc do not hardcode Game.Target
    interaction :: Maybe (Interaction Game.Target),
    -- | The mana of the team being drawn
    mana :: Nat,
    -- | How to offset the default position of the hand's cards
    offseter :: HandOffseter,
    -- | The team of the hand being drawn
    team :: Team,
    -- | The player of the hand being drawn
    playingPlayer :: Spots.Player,
    shared :: SharedModel
  }

boardToInHandCell ::
  HandDrawingInput ->
  HandActionizer Action ->
  -- The z-index when the card is on top of others
  Int ->
  -- The z-index, the card, the index in the hand
  (Int, Card 'Core, HandIndex) ->
  Styled (View Action)
boardToInHandCell
  HandDrawingInput
    { mana = availMana,
      ..
    }
  HandActionizer {..}
  bigZ
  (z, card, i) = do
    card <- cardView loc (if beingHovered || beingDragged then bigZ else z) shared team card cdsty
    return $ div_ attrs [card | not beingDragged]
    where
      (beingHovered, beingDragged) =
        case interaction of
          Just (HoverInteraction Hovering {hoveredCard}) ->
            (hoveredCard == i, False)
          Just (DragInteraction Dragging {draggedCard}) ->
            (False, draggedCard == i)
          Just (HoverInPlaceInteraction _) -> (False, False)
          Just NoInteraction -> (False, False)
          Just (ShowErrorInteraction _) -> (False, False)
          Nothing -> (False, False)
      loc = if beingDragged then GameDragLoc else GameHandLoc
      rightmargin = cps * 2
      hgap = (cardHCellGap * cps) `div` 2 -- The horizontal space between two cards
      i' = unHandIndex i
      y = 2 * cps
      x =
        rightmargin
          + if handSize <= 5
            then i' * (cardPixelWidth + hgap) -- No overlapping
            else case i' of -- Overlapping
              0 -> 0
              _ -> i' * cpw'
      (x', y') = offseter (x, y)
      -- The visible width of a card when there's overlapping.
      -- Draw a picture to understand from where this formula comes from.
      cpw' = (maxHSpace - cardPixelWidth) `div` (handSize - 1)
      handSize = length hand
      maxHSpace = (5 * cardPixelWidth) + 4 * hgap -- cards + gaps
      fadeIn = map snd hand !! unHandIndex i
      attrs =
        [ style_ $ cardPositionStyle' x' y',
          prop "draggable" True,
          Miso.onDragStart $ onDragStart i,
          Miso.onDragEnd onDragEnd,
          class_ "card",
          onMouseEnter' "card" $ onMouseEnter i,
          onMouseLeave' "card" $ onMouseLeave i
        ]
          ++ [style_ $ "filter" =: "brightness(50%)" | not playable]
      cdsty = mempty {hover = beingHovered, PCWViewInternal.fadeIn = fadeIn}
      playable =
        case mlift shared card <&> Card.toCommon of
          Nothing -> traceShow ("[ERR] Common not found for card: " ++ show card) True
          Just (CardCommon {mana = requiredMana}) -> availMana >= requiredMana

-- | Offsets, in number of cells, from the board's origin
cardCellsBoardOffset :: Spots.Player -> Spots.Card -> (Int, Int)
cardCellsBoardOffset PlayerTop cardSpot =
  (offsetx + x, offsety + y)
  where
    (offsetx, offsety) =
      (boardToLeftCardCellsOffset, 3 :: Int) -- offset from background corner
    botyShift = cardCellHeight + cardVCellGap -- offset from top to bottom line
    xtop = cardCellWidth + cardHCellGap -- offset from left to middle
    xtopright = xtop * 2 -- offset from left to right
    (x, y) = case cardSpot of
      TopLeft -> (0, 0)
      Top -> (xtop, 0)
      TopRight -> (xtopright, 0)
      BottomLeft -> (0, botyShift)
      Bottom -> (xtop, botyShift)
      BottomRight -> (xtopright, botyShift)
cardCellsBoardOffset PlayerBot cardSpot =
  (offsetx + x, offsety + y)
  where
    (offsetx, offsety) =
      ( boardToLeftCardCellsOffset,
        3 + (2 * cardCellHeight) + cardVCellGap + teamsVCellGap -- offset from background corner
      )
    topyShift = cardCellHeight + cardVCellGap
    xtop = cardCellWidth + cardHCellGap -- offset from left to middle
    xtopright = xtop * 2 -- offset from left to right
    (x, y) = case cardSpot of
      TopLeft -> (xtopright, topyShift) -- TopLeft is bottom right in bottom part
      Top -> (xtop, topyShift) -- Top is Bottom in bottom part
      TopRight -> (0, topyShift) -- TopRght is bottom left in bottom part
      BottomLeft -> (xtopright, 0) -- BottomLeft is top right in bottom part
      Bottom -> (xtop, 0) -- Bottom is Top in bottom part
      BottomRight -> (0, 0) -- BottomRight is Top Left in bottom part

handCell :: View Action
handCell =
  img_
    [ width_ $ ms handPixelWidth,
      src_ $ assetsPath "forest-hand.png",
      noDrag
    ]
