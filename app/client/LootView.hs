{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module LootView where

import Card
import qualified Constants
import qualified Data.Bifunctor
import Data.Function ((&))
import Data.List (partition)
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
import SharedModel (SharedModel)
import qualified SharedModel
import Update (Action (LootAction'), LootAction (Pick, Unpick))
import ViewInternal

-- |
-- Module to display the view reached after a successful end of 'GameView'.
-- This view sits between two games, where the player can pick new
-- cards to augment its deck.
-- |

-- | Top-level rendering function
view :: LootModel -> Styled (View Action)
view LootModel {..} = do
  rewards <- rewardsView ctxt zpppp rewards
  deck <- deckView ctxt zpppp (zip picked (repeat Model.Picked) ++ zip deck (repeat Model.NotPicked))
  return $
    div_
      [style_ $ bgStyle z]
      [ rewards,
        rewardsLegend,
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
        [div_ [legendTextStyle] [text rewardsTextLegend]]
    rewardsTextLegend :: MisoString =
      case remainingToPick of
        0 -> "Rewards: you picked them all! Click on a card to unpick it."
        _ ->
          ms $
            "Rewards: pick "
              ++ show nbRewards
              ++ (if nbRewards > 1 then " of them" else "")
              ++ " (click on a card to pick it)"
    (picked :: [Card.ID], _notPicked :: [Card.ID]) =
      partition ((\case Picked -> True; NotPicked -> False) . snd) rewards
        & Data.Bifunctor.bimap (map fst) (map fst)
    cardinalPicked =
      map snd rewards
        & filter (\case Picked -> True; NotPicked -> False)
        & natLength
    remainingToPick = nbRewards - cardinalPicked
    deckLegend =
      div_
        [ legendStyle
            ((deckViewTopMargin + deckViewHeight) + (Constants.cps `div` 4)) -- below deck + margin
        ]
        [div_ [legendTextStyle] [text "Your deck"]]
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
    shared :: SharedModel,
    team :: Team
  }

-- | The margin from the top to the row of the rewards
rewardsViewTopMargin :: Int
rewardsViewTopMargin = Constants.cps * 8

-- | The height of the rewards div
rewardsViewHeight :: Int
rewardsViewHeight = Constants.cardPixelHeight

-- | The div showing the rewards. It's displayed above 'deckView'
rewardsView :: Context -> Int -> [(Card.ID, Model.Picked)] -> Styled (View Action)
rewardsView Context {remainingToPick, shared, LootView.team} z cards = do
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
      cardViews
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
      map (Data.Bifunctor.first $ SharedModel.identToCard shared) cards
        & mapMaybe liftNothing
        & map (Data.Bifunctor.first unlift)
      where
        liftNothing = \case
          (Nothing, _) -> Nothing
          (Just x, y) -> Just (x, y)
    divCard :: (Nat, (Card 'Core, Model.Picked)) -> Styled (View Action)
    divCard (i, (card, picked)) = do
      inner <- PCWViewInternal.cardView LootLoc z shared team card mempty
      return $
        div_
          ( [ style_ $
                cardPositionStyle'
                  (Constants.cps * x)
                  (Constants.cps * y)
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
        yCellsOffset = Constants.cardCellHeight + 1 -- card height + vertical margin
        x = (natToInt i `mod` deckCardsPerRow) * xCellsOffset
        y = (natToInt i `div` deckCardsPerRow) * yCellsOffset

deckViewTopMargin :: Int
deckViewTopMargin = Constants.cps * 14

deckViewHeight :: Int
deckViewHeight =
  (Constants.cardPixelHeight * 2)
    + Constants.cps -- 2 cards high + vertical gap
    + (Constants.cps `div` 2) -- Give a bit more space, so that we don't have
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

-- | The div showing the deck. The value of type 'Model.Picked' indicates
-- whether the card is a reward that was picked or a card from
-- the deck of the previous game.
deckView :: Context -> Int -> [(Card.ID, Model.Picked)] -> Styled (View Action)
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
    cards' :: [(Card 'Core, Picked)] =
      map (Data.Bifunctor.first $ SharedModel.identToCard shared) cards
        & mapMaybe liftNothing
        & map (Data.Bifunctor.first $ unlift)
    liftNothing (Nothing, _) = Nothing
    liftNothing (Just x, y) = Just (x, y)
    -- First nat is the index in the whole list of cards. Value of type
    -- Model.Picked indicates whether the card comes from the rewards or
    -- from the deck of the previous game.
    divStack :: (Nat, ((Card 'Core, Model.Picked), Nat)) -> Styled (View Action)
    divStack (i, ((card, picked), cardinal)) = do
      cards <-
        traverse
          (\offset -> divCard (i, card, picked) offset)
          $ [(natToInt cardinal - 1), (natToInt cardinal - 2) .. 0]
      return $ div_ [] cards
    divCard :: (Nat, Card 'Core, Model.Picked) -> Int -> Styled (View Action)
    divCard (i, card, picked) stackIdx = do
      inner <- PCWViewInternal.cardView LootLoc z shared team card cds
      return $
        div_
          [ style_ $
              cardPositionStyle'
                ((Constants.cps * x) + (stackIdx * 4))
                ((Constants.cps * y) + (stackIdx * 4))
                <> "z-index" =: ms (z - (stackIdx * 3))
          ]
          [inner]
      where
        xCellsOffset = Constants.cardCellWidth + 1 -- card width + horizontal margin
        yCellsOffset = Constants.cardCellHeight + 1 -- card height + vertical margin
        x = (natToInt i `mod` deckCardsPerRow) * xCellsOffset
        y = (natToInt i `div` deckCardsPerRow) * yCellsOffset
        cds =
          mempty
            { PCWViewInternal.overlay = case picked of
                Picked ->
                  -- If we want to use multiple colors in the future,
                  -- this is the place to change.
                  PCWViewInternal.Green
                NotPicked -> PCWViewInternal.None
            }
