{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- This module contains things used in GameView.hs
-- but which are likely rarely modified, to avoid
-- GameView.hs growing too much. Other modules besides
-- 'GameView' should nod depend on this module.
-- |
module GameViewInternal
  ( borderWidth,
    deathFadeout,
    errView,
    heartWobble,
    keyframes,
    manaView,
    messageView,
    noDrag,
    scoreViews,
    StackPosition (..),
    stackView,
    turnView,
    statChange,
    StatChangeKind (..),
  )
where

import Board hiding (StackType)
import Card
import Constants
import Control.Lens
import Control.Monad.Except
import qualified Damage
import qualified Data.Map.Strict as Map
import Data.Maybe (fromJust)
import qualified Data.Text as Text
import Debug.Trace (trace)
import qualified Game (Animation (..), MessageText (..), Target (..), appliesTo, enemySpots)
import Miso hiding (at)
import Miso.String hiding (length, map)
import Model
import Nat
import PCWViewInternal ()
import SharedModel (unsafeIdentToCard)
import qualified Skill
import Spots hiding (Card)
import qualified Tile
import qualified Turn
import Update
import ViewBlocks (ButtonState (..), gui, textButton)
import ViewInternal

errView :: GameModel -> Int -> Maybe (View Action)
errView GameModel {interaction} z =
  case interaction of
    ShowErrorInteraction msg ->
      Just $
        trace (Text.unpack msg) $ -- Log it to console
          div_ [style_ errViewStyle] $ [textView msg] ++ feedbackViews
    _ -> Nothing
  where
    (width, height) = (504, 168)
    left = (boardPixelWidth - width) `div` 2
    top = boardPixelHeight `div` 2
    errViewStyle =
      zpltwh z Relative left top width height
        <> flexColumnStyle
        <> "justify-content" =: "center"
        <> "background-image" =: assetsUrl "errbox.png"
    textView msg = div_ [style_ textStylePairs] [Miso.text $ ms msg]
    textWidth = width - (cellPixelSize * 2)
    textWidthAttr = "width" =: px textWidth
    -- XXX It'd be better to center the error message (like the feedback text).
    -- This is the case if the width is NOT specified.
    textStylePairs = "color" =: "#FF0000" <> textWidthAttr
    feedbackViews :: [View Action] =
      [ br_ [],
        Miso.text "Please copy/paste this error in a comment of ",
        a_ [href_ itchURL] [Miso.text itchURL]
      ]

messageView :: Game.Animation -> Maybe (Styled (View a))
messageView =
  \case
    Game.NoAnimation -> Nothing
    Game.Fadeout -> Nothing
    Game.Message txt duration ->
      -- Message shown during 'duration' seconds and fades out during 1 second
      Just $ ViewInternal.fade builder (Just duration) 1 ViewInternal.FadeOut
      where
        builder attrs = div_ (pos : attrs) [div_ [nested] $ map viewMessageText txt]
        -- TODO @smelc share code with LootView
        pos =
          style_ $
            zpltwh
              1 -- z
              Absolute
              0 -- left
              top
              Constants.boardPixelWidth -- width
              Constants.cps -- height
        nested =
          style_ $
            textStyle
              <> flexLineStyle -- Horizontal layouting
              <> "width" =: (px Constants.boardPixelWidth) -- Required for horizontal centering
              <> "justify-content" =: "center" -- Center horizontally
              <> "font-size" =: "20px"
        top = Constants.cps * 12 + (Constants.cps `div` 2)
  where
    viewMessageText :: Game.MessageText -> View a
    viewMessageText =
      \case
        (Game.Text txt) ->
          Miso.text $ ms txt
        (Game.Image filepath) ->
          img_ [src_ $ assetsPath $ ms $ Tile.filepathToString filepath]

scoreViews :: GameModel -> Int -> [View Action]
scoreViews m@GameModel {board} z =
  [ scoreView m z PlayerTop,
    scoreView m z PlayerBot
  ]
    ++ [ scoreLeaderView
           (if topScore > botScore then PlayerTop else PlayerBot)
         | topScore /= botScore
       ]
  where
    topScore = board ^. #playerTop . #score
    botScore = board ^. #playerBottom . #score

scoreMarginTop :: Spots.Player -> Int
scoreMarginTop PlayerTop = cps
scoreMarginTop PlayerBot = cps * 24

scoreView :: GameModel -> Int -> Spots.Player -> View Action
scoreView GameModel {board} z pSpot =
  div_
    [ style_ $
        Map.fromList textRawStyle
          <> flexColumnStyle
          <> "z-index" =: ms z
          <> "position" =: "absolute"
          -- Center horizontally
          <> "margin-left" =: "50%"
          <> "margin-right" =: "50%"
          -- And tell the element to center horizontally, not to its left
          <> "transform" =: "translate(-50%, 0%)"
          -- Finally shift element down
          <> "margin-top" =: px (scoreMarginTop pSpot)
    ]
    [ div_ [] [Miso.text "Score"],
      div_ [] [Miso.text $ ms $ show score]
    ]
  where
    score = board ^. (spotToLens pSpot . #score)

manaView :: GameModel -> Int -> View a
manaView GameModel {board, playingPlayer} z =
  div_ [style_ flexColumnStyle, style_ positioning] (manaText : imgs)
  where
    manaText = div_ [style_ textStyle] [Miso.text "Mana"]
    positioning =
      "z-index" =: ms z
        <> "position" =: ms (show Absolute)
        <> "left" =: px (2 * cps)
        <> "bottom" =: px (3 * cps)
        <> "width" =: px cps
    imgPosition i =
      "z-index" =: ms z
        <> "position" =: ms (show Absolute)
        <> "bottom" =: px ((i * seize) + (seize `div` 2))
        <> "width" =: px seize
    nbMana = Board.toPart board playingPlayer & Board.mana & natToInt
    imgs = if nbMana == 0 then [] else map mkImage [1 .. nbMana]
    mkImage i =
      div_
        [style_ $ imgPosition i]
        $ [img_ [src_ (assetsPath $ assetFilenameMana)]]

-- Not in scoreView itself, otherwise it mixes up the central alignment
-- (that uses scoreView's div own width (50%))
scoreLeaderView :: Spots.Player -> View Action
scoreLeaderView pSpot =
  img_
    [ src_ (assetsPath assetFilenameCrown),
      style_ $
        "margin-top" =: px (scoreMarginTop pSpot + cps `div` 2)
          <> "margin-left" =: px ((cps * 11) + cps `div` 2)
    ]

turnView :: GameModel -> Int -> Styled (View Action)
turnView model@GameModel {turn} z = do
  line3 <- line3M
  return $ div_ [style_ $ turnViewStyle <> textStyle] [line1, line2, line3]
  where
    turnViewStyle =
      zprbwh z Absolute 0 0 turnPixelWidth turnPixelHeight
        <> flexColumnStyle
        <> "justify-content" =: "center"
        <> "background-image" =: assetsUrl "turn.png"
    line1 :: View Action = Miso.text $ "Turn " <> ms (Turn.toNat turn)
    playerImgY =
      case Turn.toPlayerSpot turn of
        PlayerTop -> "1"
        PlayerBot -> "2"
    topMargin = cellPixelSize `div` 2
    topMarginAttr = style_ $ "margin-top" =: px topMargin
    line2 :: View Action =
      div_
        [topMarginAttr]
        [img_ [src_ (assetsPath $ "24x24_" <> playerImgY <> "_2.png")]]
    line3M :: Styled (View Action) =
      textButton
        gui
        z
        (if isPlayerTurn model then Enabled else Disabled)
        [ topMarginAttr,
          onClick $ GameAction' GameEndTurnPressed
        ]
        "End Turn"

data StackPosition
  = -- | Stack/discarded widget in board (enemy), only in hashless Configuration
    Board
  | -- | Stack/discarded widget in hand (playing player)
    -- | Always visible
    Hand

data StackWidgetType
  = Button
  | Plus

-- | The widget showing the number of cards in the stack/discarded stack
stackView :: GameModel -> Int -> Spots.Player -> StackPosition -> StackKind -> Styled [View Action]
stackView GameModel {anims, board, shared, uiAvail} z pSpot stackPos stackType = do
  button <- textButton gui z enabledness [] $ ms (label ++ ": " ++ show atColonSize)
  plus <- keyframed plusBuilder plusFrames animData
  return $
    [div_ (buttonStyle : [onClick $ DeckGo deck | enabled]) [button]]
      ++ [div_ [plusStyle] [plus] | plusValue > 0]
  where
    commonStyle = "z-index" =: ms z <> "position" =: "absolute"
    enabledness = if uiAvail then Enabled else Disabled
    enabled = case enabledness of
      Disabled -> False
      Enabled -> True
      Selected -> False
    verticalMargin GameViewInternal.Board _ = "top" =: px (scoreMarginTop PlayerTop)
    verticalMargin Hand Button = "bottom" =: px (cps `div` 2)
    verticalMargin Hand Plus = "bottom" =: px (- (cps `div` 2))
    horizontalMargin GameViewInternal.Board _ Button = px (cps * 4)
    horizontalMargin GameViewInternal.Board Discarded Plus = px (cps * 8)
    horizontalMargin GameViewInternal.Board Stacked Plus = px (cps * 2)
    horizontalMargin Hand _ Button = px (cps * 4)
    horizontalMargin Hand Discarded Plus = px (cps * 8)
    horizontalMargin Hand Stacked Plus = px (cps * 2)
    buttonStyle =
      style_ $
        commonStyle
          <> marginSide =: horizontalMargin stackPos stackType Button
          <> verticalMargin stackPos Button
    plusStyle =
      style_ $
        commonStyle
          <> marginSide =: horizontalMargin stackPos stackType Plus
          <> verticalMargin stackPos Button
          -- Specify size, for element to stay in place
          <> "width" =: px plusFontSize
          <> "height" =: px plusFontSize
          -- Tell the element to stay in place
          <> "transform" =: "translate(-50%, -50%)"
    (getter, label, marginSide) = case stackType of
      Stacked -> (#stack, "Stack", "right")
      Discarded -> (#discarded, "Discarded", "left")
    plusValue = case stackType of
      Stacked -> Board.toStack anims pSpot
      Discarded -> Board.toDiscarded anims pSpot + nbDeaths
    deck :: [Card 'Core] = board ^. spotToLens pSpot . getter & map (unsafeIdentToCard shared) & map Card.unlift
    atColonSize = length deck
    attackEffects = anims ^. spotToLens pSpot . #inPlace & unInPlaceEffects
    nbDeaths = Map.foldr (\ae i -> i + (if (isDead . death) ae then 1 else 0)) 0 attackEffects
    animName = "stackPlus"
    animData =
      (animationData animName "1s" "linear")
        { animDataFillMode = Just "forwards"
        }
    (plusFontSize, plusFontSize') =
      (defaultFontSize * 2, defaultFontSize) -- stard, end
    plusFrames =
      textFrames
        animName
        (plusFontSize, "#FF0000", False)
        (plusFontSize', "#FFFFFF", True)
    plusBuilder x = div_ x [Miso.text $ ms ("+" :: MisoString) <> ms (show plusValue)]

-- | Draw border around target if:
-- 1/ card in hand is being hovered or dragged -> draw borders around
--    valid drag targets
-- or 2/ card in place is being hovered -> draw borders around cards
--       be attacked from this card== playingPlayerSpot,
borderWidth :: GameModel -> Game.Target -> Int
borderWidth GameModel {board, interaction, playingPlayer} pTarget =
  case (interaction, pTarget) of
    (DragInteraction Dragging {draggedCard}, _) | cond draggedCard -> 3
    (HoverInteraction Hovering {hoveredCard}, _) | cond hoveredCard -> 3
    (HoverInPlaceInteraction (Game.CardTarget pSpotHov cSpotHov), Game.CardTarget pSpot cSpot) ->
      if pSpot /= pSpotHov && cSpot `elem` Game.enemySpots canAttack skills' cSpotHov
        then borderSize
        else 0
      where
        attacker = Board.toInPlaceCreature board pSpotHov cSpotHov
        canAttack = Damage.dealer attacker
        skills' = maybe [] skills attacker & map Skill.lift
    _ -> 0
  where
    cond hi =
      case handCard $ unHandIndex hi of
        Left errMsg -> trace (Text.unpack errMsg) False
        Right id -> Game.appliesTo board id playingPlayer pTarget
    handCard i = lookupHand (Board.toHand board playingPlayer) i & runExcept

deathFadeout :: InPlaceEffect -> Int -> Int -> Styled [View Action]
deathFadeout ae _ _ =
  sequence
    [ keyframed
        builder
        (keyframes (animDataName animData) "opacity: 1;" [] "opacity: 0;")
        animData
      | isDead
    ]
  where
    (isDead, asset) =
      case death ae of
        DeathByBreathIce -> (True, Just assetFilenameSnowflake)
        DeathByTerror -> (True, Just assetFilenameShade)
        DeathByFear -> (True, Just assetFilenameGhost)
        NoDeath -> (False, Nothing)
        UsualDeath -> (True, Just assetFilenameSkull)
    sty = pltwh Absolute left top imgw imgh
    (imgw, imgh) :: (Int, Int) = (cellPixelSize, imgw)
    left = (cardPixelWidth - imgw) `div` 2
    top = (cardPixelHeight - imgh) `div` 2
    builder x =
      img_ $ [src_ (assetsPath $ fromJust asset), style_ sty] ++ x
    animData =
      (animationData "deathFadeout" "1s" "ease")
        { animDataFillMode = Just "forwards"
        }

heartWobble :: Int -> InPlaceEffect -> Styled [View Action]
heartWobble z ae =
  sequence
    [ keyframed
        builder
        (wobblev (animDataName animData) True imgw (imgh * 3))
        animData
      | delay <- delays,
        let animData = createAnimData delay
    ]
  where
    hpc = hitPointsChange ae
    hpLoss = hpc < 0
    delay = 250 -- The delay between each wobbling heart, milliseconds
    delays =
      [delay * (i - 1) | i <- if hpLoss then [1 .. (- hpc)] else []]
    sty = pltwh Absolute left top imgw imgh <> "z-index" =: ms z
    (imgw, imgh) :: (Int, Int) = (seize, imgw)
    (left, top) = ((cardPixelWidth - imgw) `div` 2, 0)
    builder x =
      img_ $ [src_ (assetsPath assetFilenameHeart), style_ sty] ++ x
    createAnimData delay =
      (animationData "heartWobble" "1s" "linear")
        { animDataFillMode = Just "forwards",
          animDataDelay = Just $ ms delay <> "ms"
        }

data StatChangeKind = HitPoint | Attack

statChange :: Int -> StatChangeKind -> InPlaceEffect -> Styled [View Action]
statChange z sck ae =
  sequence
    [ keyframed
        (builder x)
        (grow (animDataName animData) (imgw, imgh) (imgw * 2, imgh * 2))
        animData
      | x <- [0 .. change - 1],
        change > 0
    ]
  where
    (change, animName, assetName) =
      case sck of
        Attack -> (attackChange ae, "swordGain", assetFilenameSword)
        HitPoint -> (hitPointsChange ae, "heartGain", assetFilenameHeart)
    sty xshift = pltwh Absolute (left + xshift) top imgw imgh <> "z-index" =: ms z
    (imgw, imgh) :: (Int, Int) = (seize, imgw)
    (left, top) = (case sck of HitPoint -> cps `div` 3; Attack -> 2 * cps, 0)
    builder x rest =
      img_ $ [src_ $ assetsPath assetName, style_ $ sty (xshiftf x change)] ++ rest
    xshiftf 0 1 = 0 -- if there's a single heart, center it
    xshiftf x 2 = (if odd x then -1 else 1) * cps -- two hearts
    xshiftf 0 3 = 0 -- three hearts, center
    xshiftf x 3 | odd x = xshiftf 43 2 -- three hearts, 43 is odd: to the left
    xshiftf _ 3 = xshiftf 42 2 -- three hearts, 42 is even: to the right
    xshiftf x total = error $ "xshiftf " ++ show x ++ " " ++ show total ++ " is unsupported"
    animData =
      (animationData animName "1s" "linear") {animDataFillMode = Just "forwards"}
