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
module GameView where

import qualified Board
import Card
import qualified Configuration
import Constants
import Control.Monad (join)
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
import qualified Mana
import Miso hiding (at)
import Miso.String hiding (concat, intersperse, last, length, map, null, zip)
import Model
import qualified Move
import Nat
import PCWViewInternal
import qualified Shared
import Spots hiding (Card)
import qualified Spots
import Theme (Theme)
import qualified Theme
import qualified Total
import qualified Turn
import Update
import ViewInternal

-- | Constructs a virtual DOM from a game model
viewGameModel :: Model.Game -> Styled (View Action)
viewGameModel model@Model.Game {anim, board, shared, interaction, playingPlayer} = do
  boardDiv <- boardDivM
  handDiv <- handDivM
  let divs = [boardDiv, handDiv] ++ if Configuration.isDev then cmdDiv shared else []
  let builder attrs = div_ attrs divs
  ViewInternal.fade builder Nothing 2 $ animToFade anim
  where
    (z, zpp, zpppp) = (0, z + 1, zpp + 1)
    enemySpot = Spots.other playingPlayer
    application :: Styled (Maybe (View Action)) =
      case anim of
        Game.Application pSpot target card -> do
          c :: View Action <- cardView loc (zpppp + 4) shared team card cdsty
          return $ pure $ div_ [style_ $ mkTargetOffset target] [c]
          where
            cdsty :: CardDrawStyle = mempty {PCWViewInternal.fade = Constants.FadeOut}
            team = Board.toPart board pSpot & Board.team
        Game.NoAnimation -> pure Nothing
        Game.Fadeout -> pure Nothing
        Game.Message {} -> pure Nothing
    boardCardsM = boardToInPlaceCells (InPlaceCellContext {z = zpp, mkOffset}) model selectedTargetType
    mkOffset pSpot cSpot = cardCellsBoardOffset pSpot cSpot & uncurry cardPositionStyle
    boardDivM = do
      let errs = errView model zpppp & maybeToList
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
          decos = concat [decoViews zpppp pSpot board | pSpot <- Spots.allPlayers]
      return $
        div_ [style_ boardStyle] $
          concat stacks ++ msg ++ [turn] ++ errs ++ scores ++ boardCards ++ decos ++ apps ++ [manaView_]
    loc = GameApplicationLoc (Mana.labeler model)
    theme :: Theme = Theme.kindToTheme Theme.DarkForest
    boardStyle =
      zpltwh z Relative 0 0 boardPixelWidth boardPixelHeight
        <> "background-image" =: assetsUrl (Theme.board theme)
    handDivM = do
      cells <- boardToInHandCells zpp $ toHandDrawingInput model
      stacks <- traverse (stackView model z playingPlayer Hand) [Board.Discarded', Board.Stacked]
      return $ div_ [style_ handStyle] $ cells ++ concat stacks
    handStyle =
      zpltwh z Relative 0 0 handPixelWidth handPixelHeight
        <> "background-image" =: assetsUrl (Theme.hand theme)
    selectedTargetType :: Maybe Card.TargetType =
      case Model.toSelection interaction of
        Just (Model.InHand (Board.HandIndex idx)) ->
          case Board.getpk @'Board.Hand playingPlayer board & flip Board.lookupHand idx of
            Left err -> traceShow ("viewGameModel: " ++ Text.unpack err) Nothing
            Right id -> Just $ targetType id
        Just (Model.InPlace {}) -> Nothing
        Nothing -> Nothing

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

cmdDiv :: Shared.Model -> [View Action]
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
            [li_ [] [Miso.text $ show cmd & ms] | cmd <- Shared.allCommands shared]
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
  Model.Game ->
  -- | The target to which the selected card (if any) applies (if any)
  Maybe TargetType ->
  Styled [View Action]
boardToInPlaceCells ctxt@InPlaceCellContext {z} m@Model.Game {board} selectedTargetType = do
  emitTopLevelStyle $ bumpKeyFrames True
  emitTopLevelStyle $ bumpKeyFrames False
  main <- mainM
  let playerTargets = [boardToPlayerTarget (z + 1) m selectedTargetType pSpot | pSpot <- Spots.allPlayers]
  return [div_ [] $ main ++ catMaybes playerTargets]
  where
    mainM =
      sequence
        [ boardToInPlaceCell ctxt m actionizer selectedTargetType pSpot cSpot
          | (pSpot, cSpot, _) <- Board.toHoleyInPlace board
        ]
    bumpAnim upOrDown = ms $ "bump" ++ (if upOrDown then "Up" else "Down")
    bumpInit = "transform: translateY(0px);"
    sign upOrDown = if upOrDown then "-" else ""
    bump50 upOrDown = "transform: translateY(" ++ sign upOrDown ++ show cellPixelSize ++ "px);"
    bumpKeyFrames upOrDown =
      keyframes (bumpAnim upOrDown) bumpInit [(50, bump50 upOrDown)] bumpInit
    actionizer =
      Actionizer
        { onMouseEnter = Move.InPlaceMouseEnter,
          onMouseLeave = Move.InPlaceMouseLeave,
          onSelection = \(pSpot, cSpot) ->
            Move.Selection $ Model.InPlace $ Game.CardTarget pSpot cSpot
        }
        <&> GameAction'

boardToInPlaceCell ::
  InPlaceCellContext ->
  Model.Game ->
  Actionizer (Spots.Player, Spots.Card) Action ->
  -- | The target to which the card being selected applies (if any)
  Maybe TargetType ->
  -- | The part considered
  Spots.Player ->
  -- | The spot of the card to show
  Spots.Card ->
  Styled (View Action)
boardToInPlaceCell InPlaceCellContext {z, mkOffset} m@Model.Game {anims, board, interaction, shared} Actionizer {..} selectedTargetType pSpot cSpot =
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
        ++ inPlaceEnterLeaveAttrs
        ++ (selectionTarget <&> dropEvents & concat)
    )
    <$> do
      fades <- fadeouts shared (z + 1) attackEffect
      heartw <- heartWobble (z + 1) attackEffect
      heartg <- statChange (z + 1) HitPoint attackEffect
      attackg <- statChange (z + 1) Attack attackEffect
      cards <-
        case maybeCreature of
          Nothing -> pure Nothing
          Just creature -> do
            let cdsty :: CardDrawStyle =
                  mempty
                    { hover = beingHovered,
                      PCWViewInternal.fade = Board.fade attackEffect
                    }
            Just <$> cardView loc z shared t (CreatureCard mkCoreCardCommon creature) cdsty
      return $ maybeToList cards ++ maybeToList fades ++ heartw ++ heartg ++ attackg
  where
    part = Board.toPart board pSpot
    t = Board.team part
    loc = GameInPlaceLoc (Mana.labeler m) (Total.mkPlace board pSpot cSpot)
    key = intersperse "_" ["inPlace", show pSpot, show cSpot] & concat
    maybeCreature = Board.toInPlaceCreature board pSpot cSpot
    inPlaceEnterLeaveAttrs =
      -- If there's a selection we don't need to handle in place hovering
      case (maybeCreature, selectionTarget) of
        (Just _, Nothing) ->
          [ onMouseEnter' "card" $ onMouseEnter (pSpot, cSpot),
            onMouseLeave' "card" $ onMouseLeave (pSpot, cSpot),
            onClick $ onSelection (pSpot, cSpot)
          ]
        _ -> []
    target = Game.CardTarget pSpot cSpot
    bumpAnim upOrDown = ms $ "bump" ++ (if upOrDown then "Up" else "Down")
    upOrDown = case pSpot of PlayerTop -> False; PlayerBot -> True
    beingHovered =
      Model.toHover interaction == Just (Model.InPlace (Game.CardTarget pSpot cSpot))
    attackEffect =
      (Board.toInPlace anims pSpot) Map.!? cSpot
        & fromMaybe mempty
    bounceStyle =
      [ ("animation", bumpAnim upOrDown <> " 0.5s ease-in-out")
        | Board.attackBump attackEffect
      ]
    rgb = targetBorderRGB interaction target
    selectionTarget =
      case (selectedTargetType, maybeCreature) of
        (Just (CardTargetType Hole), Nothing) -> Just target
        (Just (CardTargetType Occupied), Just _) -> Just target
        _ -> Nothing

-- TODO @smelc Honor the fadeout from @Board.T 'UI@
decoViews :: Int -> Spots.Player -> Board.T 'Core -> [View Action]
decoViews z pSpot board =
  [ img_
      [ src_ $ assetsPath assetRoundTreeForestSpell,
        noDrag,
        -- minus one, because the asset is larger than a card and must
        -- be offseted accordingly.
        style_ $ zplt z Absolute ((x - 1) * cps) ((y - 1) * cps),
        -- Avoid disturbing drag/drop events:
        style_ $ "pointer-events" =: "none"
      ]
    | (cSpot, _deco) <- Board.toPart board pSpot & Board.deco & Map.toList,
      let (x, y) = cardCellsBoardOffset pSpot cSpot
  ]

-- | Whether to show the player target at @pSpot@
boardToPlayerTarget ::
  Int ->
  Model.Game ->
  Maybe TargetType ->
  Spots.Player ->
  Maybe (View Action)
boardToPlayerTarget z m@Model.Game {interaction} dragTargetType pSpot =
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
              ++ (dragTarget <&> dropEvents & concat)
          )
          []
  where
    key = show pSpot ++ "-target"
    (x, y) = cardCellsBoardOffset pSpot cSpot
    (w, h) = (cardCellWidth * 3 + cardHCellGap * 2, cardCellHeight * 2 + cardVCellGap)
    posStyle x y = pltwh Absolute (x * cps) (y * cps) (w * cps) (h * cps)
    cSpot = case pSpot of PlayerTop -> TopLeft; PlayerBot -> BottomRight
    target = Game.PlayerTarget pSpot
    (rgb, bwidth) = (targetBorderRGB interaction target, borderWidth m target)
    dragTarget =
      case dragTargetType of
        Just PlayerTargetType -> Just target
        _ -> Nothing

-- | The events for placeholders showing drop targets
dropEvents :: Game.Target -> [Attribute Action]
dropEvents target =
  [ onClick $ GameAction' $ (Move.Selection (Model.InPlace target))
  -- TODO @smelc Replug once InHand/InPlace is merged in Move
  -- Miso.onMouseEnter $ GameAction' $ (Move.InPlaceMouseEnter target)
  ]

-- | The border color of a target
targetBorderRGB :: Eq a => Interaction a -> Game.Target -> (Int, Int, Int)
targetBorderRGB interaction target =
  case interaction of
    Model.HoverInteraction (Model.InPlace t) | t == target -> yellowRGB
    Model.HoverSelectionInteraction (Model.InPlace t) _ | t == target -> yellowRGB
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
    icreatures = zip cards [Board.HandIndex 0 ..]
    zicreatures = zip [z, z + 2 ..] icreatures
    bigZ = case safeLast zicreatures of Nothing -> z; Just (z', _) -> z' + 2
    safeLast l = if null l then Nothing else Just $ last l
    zicreatures' = map (\(a, (b, c)) -> (a, b, c)) zicreatures
    actionizer =
      Actionizer
        { onMouseEnter = Move.InHandMouseEnter,
          onMouseLeave = Move.InHandMouseLeave,
          onSelection = Move.Selection . Model.InHand
        }
        <&> GameAction'

toHandDrawingInput :: Model.Game -> HandDrawingInput
toHandDrawingInput m@Model.Game {interaction = gInteraction, ..} =
  HandDrawingInput {..}
  where
    hand =
      [ (card, fadeIn)
        | (i, card) <-
            zip
              [0 ..]
              ( Board.getpk @'Board.Hand playingPlayer board
                  & map (Card.unlift . Shared.unsafeIdentToCard shared)
              ),
          let fadeIn = i `elem` Board.getpk @'Board.Hand playingPlayer anims
      ]
    interaction = Just gInteraction
    labeler = Mana.labeler m
    offseter = id
    part = Board.toPart board playingPlayer
    (mana, team) = (Board.mana part, Board.team part)

-- | Events on cards
data Actionizer a b = Actionizer
  { -- The event to raise when hovering a card
    onMouseEnter :: a -> b,
    -- The event to raise when stopping hovering a card
    onMouseLeave :: a -> b,
    -- The event to raise when clicking on a card
    onSelection :: a -> b
  }
  deriving (Functor)

type HandOffseter = (Int, Int) -> (Int, Int)

data HandDrawingInput = HandDrawingInput
  { -- | The hand, and whether the corresponding card is being faded in
    hand :: [(Card 'Core, Bool)],
    -- | The current interaction, if any. FIXME @smelc do not hardcode Game.Target
    interaction :: Maybe (Interaction Game.Target),
    -- | How to typeset mana
    labeler :: Mana.Mana -> String,
    -- | The mana of the team being drawn
    mana :: Nat,
    -- | How to offset the default position of the hand's cards
    offseter :: HandOffseter,
    -- | The team of the hand being drawn
    team :: Team,
    -- | The current turn
    turn :: Turn.T,
    -- | The player of the hand being drawn
    playingPlayer :: Spots.Player,
    shared :: Shared.Model
  }

boardToInHandCell ::
  HandDrawingInput ->
  Actionizer Board.HandIndex Action ->
  -- The z-index when the card is on top of others
  Int ->
  -- The z-index, the card, the index in the hand
  (Int, Card 'Core, Board.HandIndex) ->
  Styled (View Action)
boardToInHandCell
  HandDrawingInput {mana = availMana, ..}
  Actionizer {..}
  bigZ
  (z, card, i) = do
    card <- cardView loc (if beingHovered || beingSelected then bigZ else z) shared team card cdsty
    return $ div_ attrs [card]
    where
      beingHovered =
        case interaction <&> Model.toHover & join of
          Just (Model.InHand hoveredCard) -> hoveredCard == i
          Just (Model.InPlace _) -> False
          Nothing -> False
      beingSelected =
        case interaction <&> Model.toSelection & join of
          Just (Model.InHand j) -> i == j
          Just (Model.InPlace {}) -> False
          Nothing -> False
      loc = GameHandLoc labeler
      rightmargin = cps * 2
      hgap = (cardHCellGap * cps) `div` 2 -- The horizontal space between two cards
      i' = Board.unHandIndex i
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
      fade = if (map snd hand !! Board.unHandIndex i) then Constants.FadeIn else Constants.DontFade
      attrs =
        [ style_ $ cardPositionStyle' x' y',
          class_ "card",
          onMouseEnter' "card" $ onMouseEnter i,
          onMouseLeave' "card" $ onMouseLeave i,
          onClick $ onSelection i
        ]
          ++ [style_ $ "filter" =: "brightness(50%)" | not playable]
      cdsty = mempty {hover = beingHovered, PCWViewInternal.fade = fade, selected = beingSelected}
      playable =
        case Shared.mlift shared card <&> Card.toCommon of
          Nothing -> traceShow ("[ERR] Common not found for card: " ++ show card) True
          Just (CardCommon {mana = requiredMana}) -> (Mana.<=) turn requiredMana availMana

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
