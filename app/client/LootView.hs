{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module to display the view reached after a successful end of 'GameView'.
-- This view sits between two games, where the player can pick new
-- cards to augment its deck.
module LootView where

import Card
import qualified Constants
import qualified Data.Bifunctor
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.List.Extra (findIndex)
import Data.List.Index
import qualified Data.Map.Strict as Map
import Data.Maybe
import Debug.Trace (traceShow)
import Miso
import Miso.String hiding (drop, filter, findIndex, length, map, partition, split, take, zip)
import Model (LootModel (..), Picked (..))
import Nat
import PCWViewInternal (DisplayLocation (..), cardPositionStyle')
import qualified PCWViewInternal
import qualified Shared
import Update (Action (LootAction'), LootAction (Pick, Unpick))
import ViewBlocks (ButtonState (..), gui, textButton)
import ViewInternal

-- | Top-level rendering function
view :: LootModel -> Styled (View Action)
view LootModel {..} = do
  next <- nextView zpppp remainingToPick
  rewards <- rewardsView ctxt zpppp rewards
  deck <-
    deckView
      ctxt
      zpppp
      $ (map (\(idx, id) -> DeckCard id (Just idx)) picked)
        ++ (map (flip DeckCard Nothing) deck)
  return $
    div_
      [style_ $ bgStyle z]
      [ rewards,
        rewardsLegend,
        next,
        deck,
        deckLegend,
        div_
          [style_ $ "position" =: "absolute" <> "top" =: "0" <> "right" =: "0" <> "z-index" =: ms zpp]
          [img_ [src_ $ Constants.assetsPath "loot-top-sand.png"]]
      ]
  where
    (z, zpp, zpppp) = (0, z + 1, z + 2)
    bgStyle :: Int -> Map.Map MisoString MisoString
    bgStyle z =
      zpltwh z Relative 0 0 Constants.lobbiesPixelWidth Constants.boardPixelHeight
        <> "background-image" =: Constants.assetsUrl "loot-bot-forest.png"
    ctxt = Context {..}
    rewardsLegend =
      div_
        [ legendStyle
            ((rewardsViewTopMargin + rewardsViewHeight) + (Constants.cps `div` 4)) -- below rewards + margin
        ]
        [div_ [legendTextStyle] [Miso.text rewardsTextLegend]]
    rewardsTextLegend :: MisoString =
      case remainingToPick of
        0 -> "All rewards picked!"
        _ -> ms $ "Rewards: pick " ++ show nbRewards
    picked :: [(Nat, ID)] =
      zip [(0 :: Nat) ..] rewards
        & filter (\(_idx, (_id, picked)) -> case picked of Picked -> True; NotPicked -> False)
        & map (\(idx, (id, _)) -> (idx, id))
    cardinalPicked =
      map snd rewards
        & filter (\case Picked -> True; NotPicked -> False)
        & natLength
    remainingToPick = nbRewards - cardinalPicked
    deckLegend =
      div_
        [legendStyle (deckViewTopMargin + deckViewHeight)] -- below deck + margin
        [div_ [legendTextStyle] [Miso.text "Your deck"]]
    legendStyle top =
      style_ $
        zpltwh
          z
          Absolute
          0 -- left
          top
          Constants.lobbiesPixelWidth -- width
          Constants.cps -- height
    legendTextStyle =
      style_ $
        textStyle
          <> flexLineStyle -- Horizontal layouting
          <> "width" =: px Constants.lobbiesPixelWidth -- Needed to center horizontally
          <> "justify-content" =: "center" -- Center horizontally

-- | Context that is common to all calls to 'deckView'
data Context = Context
  { remainingToPick :: Nat,
    shared :: Shared.Model,
    team :: Team
  }

-- | The margin from the top to the row of the rewards
rewardsViewTopMargin :: Int
rewardsViewTopMargin = Constants.cps * 7

-- | The height of the rewards div: the height of a card plus space
-- for the individual legends atop each card
rewardsViewHeight :: Int
rewardsViewHeight = Constants.cardPixelHeight + (Constants.cps `div` 2)

-- | The div showing the rewards. It is displayed above 'deckView'
rewardsView :: Context -> Int -> [(Card.ID, Model.Picked)] -> Styled (View Action)
rewardsView Context {remainingToPick, shared, LootView.team} z cards = do
  let legends :: [View Action] = map mkLegend $ zip [0 ..] $ map snd cards'
  cardViews :: [View Action] <- traverse divCard $ zip [0 ..] cards'
  return $
    div_
      [ style_ $
          zpltwh
            z
            Absolute
            viewLeft -- left margin
            rewardsViewTopMargin -- top margin
            viewWidth -- width
            rewardsViewHeight -- height
      ]
      $ legends ++ cardViews
  where
    nbCards = length cards
    viewWidth =
      (nbCards * Constants.cardPixelWidth) -- Cards horizontally
        + ((max 0 (nbCards - 1)) * Constants.cps) -- 1 cell gap between cards
    viewLeft =
      Constants.cps
        * ( case nbCards of
              1 -> 9
              2 -> 7
              3 -> 5
              _ -> traceShow ("Unexpected number of rewards cards: " ++ show nbCards) 5
          )
    cards' :: [(Card 'Core, Model.Picked)] =
      map (Data.Bifunctor.first $ Shared.identToCard shared) cards
        & mapMaybe liftNothing
        & map (Data.Bifunctor.first unlift)
      where
        liftNothing = \case
          (Nothing, _) -> Nothing
          (Just x, y) -> Just (x, y)
    mkLegend :: (Nat, Model.Picked) -> (View Action)
    mkLegend (i, picked) =
      div_
        [legendStyle i]
        [div_ [legendTextStyle] [Miso.text $ "Click to " <> verb]]
      where
        verb = case picked of Picked -> "unpick"; NotPicked -> "pick"
    legendStyle i =
      style_ $
        zpltwh
          z
          Absolute
          (natToInt i * (Constants.cardPixelWidth + Constants.cps)) -- left
          0 -- top
          Constants.cardPixelWidth -- width
          legendHeight -- height
    legendHeight = 14
    legendTextStyle =
      style_ $
        textStyle
          <> flexLineStyle -- Horizontal layouting
          <> "width" =: px Constants.cardPixelWidth -- Needed to center horizontally
          <> "justify-content" =: "center" -- Center horizontally
          <> "font-size" =: px 10
    divCard :: (Nat, (Card 'Core, Model.Picked)) -> Styled (View Action)
    divCard (i, (card, picked)) = do
      inner <- PCWViewInternal.cardView LootLoc z shared team card mempty
      return $
        div_
          ( [ style_ $
                cardPositionStyle'
                  (Constants.cps * x) -- left
                  legendHeight -- top: leave space above for individual legend
                  <> "z-index" =: ms z
                  <> ( if picked'
                         then "filter" =: "brightness(50%)"
                         else mempty
                     )
            ]
              ++ [ onClick $
                     LootAction'
                       ( if picked'
                           then Update.Unpick i
                           else Update.Pick i
                       )
                   | remainingToPick > 0 || picked'
                 ]
          )
          [inner]
      where
        picked' = case picked of Model.Picked -> True; Model.NotPicked -> False
        xCellsOffset = Constants.cardCellWidth + 1 -- card width + horizontal margin
        x = (natToInt i `mod` deckCardsPerRow) * xCellsOffset

nextView :: Int -> Nat -> Styled (View action)
nextView z remainingToPick = do
  button <- textButton gui z state [style_ textStyle] "Next →"
  let sty =
        "z-index" =: ms z
          <> "position" =: "absolute"
          <> "margin-left" =: px (Constants.lobbiesPixelWidth - (Constants.cps * 2))
          -- And tell the element to center horizontally, not to its left
          <> "transform" =: "translate(-50%, 0%)"
          -- Finally shift element down
          <> "margin-top" =: px ((rewardsViewTopMargin + rewardsViewHeight) + (Constants.cps `div` 2))
  return $ div_ [style_ sty] [button]
  where
    state = if remainingToPick == 0 then Enabled else Disabled

deckViewTopMargin :: Int
deckViewTopMargin = Constants.cps * 14

deckViewHeight :: Int
deckViewHeight =
  (Constants.cardPixelHeight * 2)
    + Constants.cps -- 2 cards high + vertical gap
    + (Constants.cps `div` 4) -- Give a bit more space, so that we don't have
    -- a scrollbar if there are less than 8 stacks

deckCardsPerRow :: Int
deckCardsPerRow = 4

deckViewWidth :: Int
deckViewWidth =
  (Constants.cardPixelWidth * deckCardsPerRow) -- Cards
    + (max 0 (deckCardsPerRow - 1)) * Constants.cps -- Gaps between cards
    + Constants.cps -- Space for scrollbar

-- | Gather equal items, counting them on the way; and *keep the order*.
-- We could use Data.Map.Ordered, but it doesn't have mapFromListWith so it's not
-- really easier.
counts :: forall a. Eq a => [a] -> [(a, Nat)]
counts l =
  Prelude.reverse $ go l []
  where
    go :: [a] -> [(a, Nat)] -> [(a, Nat)]
    go [] acc = acc
    go (x : xs) acc =
      -- Quadratic, yes.
      case findIndex (\(xAcc, _) -> xAcc == x) acc of
        Nothing -> go xs ((x, 1 :: Nat) : acc)
        Just i -> go xs $ updateAt i (Just <$> Data.Bifunctor.second succ) acc

-- | A dumb container for passing structured data to 'deckView'
data DeckCard a = DeckCard
  { -- | The card
    card :: a,
    -- | If the card comes from the reward, its index in the list of rewards
    -- otherwise Nothing.
    picked :: Maybe Nat
  }
  deriving (Eq, Foldable, Functor, Traversable)

-- | The div showing the deck. The value of type 'Model.Picked' indicates
-- whether the card is a reward that was picked or a card from
-- the deck of the previous game.
deckView :: Context -> Int -> [DeckCard Card.ID] -> Styled (View Action)
deckView Context {shared, LootView.team} _z cards = do
  cardViews :: [View Action] <- traverse divStack (zip [0 ..] $ counts cards')
  return $
    div_
      [ style_ $
          zpltwh
            z
            Absolute
            (Constants.cps * 3)
            deckViewTopMargin
            deckViewWidth
            deckViewHeight
            <> "overflow-y" =: "auto"
            <> PCWViewInternal.scrollbarStyle
      ]
      cardViews
  where
    z = 64
    cards' :: [DeckCard (Card 'Core)] =
      map (fmap $ Shared.identToCard shared) cards
        & mapMaybe sequence -- Pull Maybe out of DeckCard.card
        & map (fmap unlift)
    -- First nat is the index in the whole list of cards. Last nat
    -- indicates the number of such identical cards.
    divStack :: (Nat, (DeckCard (Card 'Core), Nat)) -> Styled (View Action)
    divStack (i, (dc, cardinal)) = do
      cards <-
        traverse
          (\offset -> divCard (i, dc) offset)
          $ [(natToInt cardinal - 1), (natToInt cardinal - 2) .. 0]
      return $ div_ [] cards
    divCard :: (Nat, DeckCard (Card 'Core)) -> Int -> Styled (View Action)
    divCard (i, DeckCard {card, picked}) stackIdx = do
      inner <- PCWViewInternal.cardView LootLoc z shared team card cds
      return $
        div_
          ( [ style_ $
                cardPositionStyle'
                  ((Constants.cps * x) + (stackIdx * 4))
                  ((Constants.cps * y) + (stackIdx * 4))
                  <> "z-index" =: ms (z - (stackIdx * 3))
            ]
              ++ maybeToList clickHandler
          )
          [inner]
      where
        clickHandler = picked <&> (\j -> LootAction' (Update.Unpick j)) <&> onClick
        xCellsOffset = Constants.cardCellWidth + 1 -- card width + horizontal margin
        yCellsOffset = Constants.cardCellHeight + 1 -- card height + vertical margin
        x = (natToInt i `mod` deckCardsPerRow) * xCellsOffset
        y = (natToInt i `div` deckCardsPerRow) * yCellsOffset
        cds =
          mempty
            { PCWViewInternal.overlay = case picked of
                Just _ ->
                  -- If we want to use multiple colors in the future,
                  -- this is the place to change.
                  PCWViewInternal.Green
                Nothing -> PCWViewInternal.None
            }
