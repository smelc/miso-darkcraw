{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- This module contains things used in GameView.hs
-- but which are likely rarely modified, to avoid
-- GameView.hs growing too much. Other modules besides
-- 'GameView' should nod depend on this module.
-- |
module GameViewInternal
  ( animToFade,
    borderWidth,
    errView,
    fadeouts,
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
import qualified Damage ()
import Data.Function ((&))
import Data.Functor ((<&>))
import qualified Data.Map.Strict as Map
import Data.Maybe (maybeToList)
import qualified Data.Text as Text
import Debug.Trace (trace)
import qualified Game
import Miso hiding (at)
import Miso.String hiding (length, map, null)
import Model
import qualified Move
import Nat
import PCWViewInternal (tileCell)
import qualified Shared
import qualified Skill
import Spots (Player (..))
import qualified Spots
import Theme (Theme)
import qualified Theme
import qualified Tile
import qualified Turn
import Update
import ViewBlocks (ButtonState (..), gui, textButton)
import ViewInternal

errView :: Model.Game -> Int -> Maybe (View Action)
errView Model.Game {interaction} z =
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
    Game.Application {} -> Nothing
    Game.NoAnimation -> Nothing
    Game.Fadeout -> Nothing
    Game.Message txt duration ->
      -- Message shown during 'duration' seconds and fades out during 1 second
      Just $ ViewInternal.fade builder (Just duration) 1 Constants.FadeOut
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

-- | What fading a 'Game.Animation' triggers
animToFade :: Game.Animation -> Constants.Fade
animToFade = \case
  Game.Application {} -> Constants.DontFade
  Game.NoAnimation -> Constants.DontFade
  Game.Fadeout -> Constants.FadeOut
  Game.Message {} -> Constants.DontFade

scoreViews :: Model.Game -> Int -> [Styled (View Action)]
scoreViews m@Model.Game {anims, board} z =
  both PlayerTop
    ++ both PlayerBot
    ++ [ pure $
           scoreLeaderView
             (if topScore > botScore then PlayerTop else PlayerBot)
         | topScore /= botScore
       ]
  where
    topScore = Board.getpk @'Board.Score PlayerTop board
    botScore = Board.getpk @'Board.Score PlayerBot board
    both pSpot = [pure $ scoreView m z pSpot, scorePluses anims z pSpot]

scoreMarginTop :: Spots.Player -> Int
scoreMarginTop PlayerTop = cps
scoreMarginTop PlayerBot = cps * 24

scoreView :: Model.Game -> Int -> Spots.Player -> View Action
scoreView Model.Game {board} z pSpot =
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
      div_ [] [Miso.text $ ms $ show $ Board.getpk @'Board.Score pSpot board]
    ]

scorePluses :: Board.T 'UI -> Int -> Spots.Player -> Styled (View Action)
scorePluses board z pSpot = do
  pluses <- traverse (\_ -> f) [0 .. scoreIncrease - 1]
  return $
    div_
      [ style_ $
          "color" =: greenHTML
            <> "font-weight" =: "bold"
            <> "z-index" =: ms z
            <> "position" =: "absolute"
            <> "margin-left" =: px leftMargin
            <> flexColumnStyle
            -- Finally shift element down
            <> "margin-top" =: px (scoreMarginTop pSpot)
      ]
      pluses
  where
    f = ViewInternal.fade builder Nothing 1 FadeOut
    builder attrs = div_ attrs [Miso.text "+1"]
    scoreIncrease =
      Board.toInPlace board pSpot
        & Map.elems
        & map (\InPlaceEffect {scoreChange} -> scoreChange)
        & sum
    leftMargin = ((Constants.boardToLeftCardCellsOffset + cardCellWidth) * cps) + cps `div` 2

manaView :: Model.Game -> Int -> View a
manaView Model.Game {board, playingPlayer} z =
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

turnView :: Model.Game -> Int -> Styled (View Action)
turnView model@Model.Game {turn} z = do
  line3 <- line3M
  return $ div_ [style_ $ turnViewStyle <> textStyle] [line1, line2, line3]
  where
    theme :: Theme = Theme.kindToTheme Theme.DarkForest
    turnViewStyle =
      zprbwh z Absolute 0 0 turnPixelWidth turnPixelHeight
        <> flexColumnStyle
        <> "justify-content" =: "center"
        <> "background-image" =: assetsUrl (Theme.turn theme)
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
          onClick $ GameAction' $ Move.Sched Move.EndTurnPressed
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
stackView :: Model.Game -> Int -> Spots.Player -> StackPosition -> StackKind -> Styled [View Action]
stackView Model.Game {anims, board, shared, uiAvail} z pSpot stackPos stackType = do
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
    verticalMargin GameViewInternal.Hand Button = "bottom" =: px (cps `div` 2)
    verticalMargin GameViewInternal.Hand Plus = "bottom" =: px (-(cps `div` 2))
    horizontalMargin :: StackPosition -> StackKind -> StackWidgetType -> MisoString
    horizontalMargin GameViewInternal.Board Handed Button = px (cps `div` 2)
    horizontalMargin GameViewInternal.Board _ Button = px (cps * 4)
    horizontalMargin GameViewInternal.Board Discarded' Plus = px (cps * 8)
    horizontalMargin GameViewInternal.Board Stacked Plus = px (cps * 2)
    horizontalMargin GameViewInternal.Board Handed Plus = "0" -- unused value: Hand button never receives +
    horizontalMargin GameViewInternal.Hand _ Button = px (cps * 4) -- unused case for Handed
    horizontalMargin GameViewInternal.Hand Discarded' Plus = px (cps * 8)
    horizontalMargin GameViewInternal.Hand Stacked Plus = px (cps * 2)
    horizontalMargin GameViewInternal.Hand Handed Plus = "0" -- unused value: Hand button never receives +
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
    (getter :: Board.PlayerPart 'Core -> [Card.ID], label, marginSide) = case stackType of
      Handed -> (Board.inHand, "Hand", "right")
      Stacked -> (Board.stack, "Stack", "right")
      Discarded' -> (Board.discarded, "Discarded", "left")
    plusValue = case stackType of
      Handed -> 0
      Stacked -> Board.getpk @'Board.Stack pSpot anims
      Discarded' -> Board.getpk @'Board.Discarded pSpot anims + nbDeaths
    deck :: [Card 'Core] =
      Board.toPart board pSpot & getter & map (Card.unlift . Shared.unsafeIdentToCard shared)
    atColonSize = length deck
    pAnims = Board.toInPlace anims pSpot
    nbDeaths = Map.foldr (\ae i -> i + (if (isDead . death) ae then 1 else 0)) 0 pAnims
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
--       that can be attacked from this card
borderWidth :: Model.Game -> Game.Target -> Int
borderWidth Model.Game {board, interaction, playingPlayer} pTarget =
  case (interaction, pTarget) of
    (DragInteraction Dragging {draggedCard}, _) | cond draggedCard -> 3
    (HoverInteraction Hovering {hoveredCard}, _) | cond hoveredCard -> 3
    (HoverInPlaceInteraction (Game.CardTarget pSpotHov cSpotHov), Game.CardTarget pSpot cSpot)
      | pSpot /= pSpotHov && cSpot `elem` attackedSpots ->
          borderSize
      where
        attacker = Board.toInPlaceCreature board pSpotHov cSpotHov
        attackedSpots :: [Spots.Card] =
          case attacker <&> flip Game.enemySpots cSpotHov of
            Nothing -> []
            Just Game.Ace -> []
            Just Game.Imprecise -> []
            Just (Game.Spots spots) -> spots
    (HoverInPlaceInteraction (Game.CardTarget pSpotHov cSpotHov), Game.PlayerTarget pSpot)
      | pSpot /= pSpotHov && imprecise ->
          borderSize
      where
        attacker :: Maybe (Creature 'Core) = Board.toInPlaceCreature board pSpotHov cSpotHov
        imprecise :: Bool = attacker `has` (Skill.Imprecise :: Skill.State)
    _ -> 0
  where
    cond hi =
      case handCard $ unHandIndex hi of
        Left errMsg -> trace (Text.unpack errMsg) False
        Right id -> Game.appliesTo board id playingPlayer pTarget
    handCard i = Board.lookupHand (Board.getpk @'Board.Hand playingPlayer board) i

fadeouts :: Shared.Model -> Int -> InPlaceEffect -> Styled (Maybe (View Action))
fadeouts shared z Board.InPlaceEffect {death, fadeOut} = do
  death :: Maybe (View Action) <- case deadAsset of
    Nothing -> pure Nothing
    Just asset -> sequence $ Just $ f (builder asset)
  fades <- traverse (\tile -> ViewInternal.fade (tbuilder tile) Nothing 1 FadeOut) fadeOut
  let views = maybeToList death ++ fades
  -- We shouldn't return a useless 'div_', hence:
  pure $ if null views then Nothing else Just $ div_ gloSty views
  where
    f :: ([Attribute a] -> View a) -> Styled (View a)
    f builder =
      keyframed
        builder
        (keyframes (animDataName animData) "opacity: 1;" [] "opacity: 0;")
        animData
    gloSty =
      [ style_ $ flexColumnStyle,
        style_ $ zpwh z Absolute cardPixelWidth cardPixelHeight
      ]
    deadAsset :: Maybe MisoString =
      case death of
        DeathByBreathIce -> Just assetFilenameSnowflake
        DeathByTerror -> Just assetFilenameShade
        DeathByFear -> Just assetFilenameGhost
        NoDeath -> Nothing
        UsualDeath -> Just assetFilenameSkull
    sty = pltwh Absolute left top imgw imgh
    (imgw, imgh) :: (Int, Int) = (cellPixelSize, imgw)
    left = (cardPixelWidth - imgw) `div` 2
    top = (cardPixelHeight - imgh) `div` 2
    builder (asset :: MisoString) attrs =
      img_ $ [src_ (assetsPath asset), style_ sty] ++ attrs
    tbuilder :: Tile.Tile -> [Attribute a] -> View a
    tbuilder tile attrs =
      div_ attrs [PCWViewInternal.tileCell shared Tile.TwentyFour tile]
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
      [delay * (i - 1) | i <- if hpLoss then [1 .. (-hpc)] else []]
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
