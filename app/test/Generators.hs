{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Generators where

import AI (Difficulty)
import Board
import qualified Campaign
import Card
import Cinema
import Damage (Damage (..))
import Data.Function ((&))
import Data.Maybe
import GHC.Generics
import qualified Game
import Generic.Random
import qualified SharedModel
import Skill (Skill)
import qualified Skill
import Spots hiding (Card)
import qualified Spots
import Test.QuickCheck
import Tile (Tile)
import Turn
import Unsafe.Coerce (unsafeCoerce)

newtype SceneAst = SceneAst [SceneAction]
  deriving (Eq, Show, Generic)

-- | Counts the number newActor, during, |||, fork, and += in a SceneAst.
-- | Useful for monitoring the size distribution of the ASTs generated by a
-- | QuickCheck test using 'collect'.
numSceneInstructions :: SceneAst -> Int
numSceneInstructions (SceneAst actions) = sum (map count actions)
  where
    count (NewActor _ _) = 1
    count (scene1 :|||: scene2) = 1 + numSceneInstructions scene1 + numSceneInstructions scene2
    count (Fork scene) = 1 + numSceneInstructions scene
    count (Wait _) = 1
    count (SetActorState _ _) = 1

data SceneAction
  = NewActor (Maybe String) ActorState
  | Wait Int
  | SetActorState Int ActorState
  | SceneAst :|||: SceneAst
  | Fork SceneAst
  deriving (Eq, Show, Generic)

instance Arbitrary CreatureKind where
  arbitrary = genericArbitraryU
  shrink = genericShrink

instance Arbitrary Team where
  arbitrary = genericArbitraryU
  shrink = genericShrink

instance Arbitrary Campaign.Level where
  arbitrary = genericArbitraryU
  shrink = genericShrink

instance Arbitrary Damage where
  arbitrary = Damage <$> arbitrary <*> arbitrary

instance Arbitrary CreatureID where
  -- Only generate CreatureID that are known to SharedModel, because
  -- we map them back a lot.
  arbitrary =
    elements $
      SharedModel.unsafeGet
        & SharedModel.getCardIdentifiers
        & mapMaybe identToId
  shrink = genericShrink

instance Arbitrary Neutral where
  arbitrary = genericArbitraryU
  shrink = genericShrink

instance Arbitrary (NeutralObject 'Core) where
  arbitrary = mkNeutralObject <$> arbitrary
    where
      mkNeutralObject x = NeutralObject x () ()
  shrink = genericShrink

instance Arbitrary Card.ID where
  arbitrary = elements $ SharedModel.getCardIdentifiers SharedModel.unsafeGet
  shrink = genericShrink

instance Arbitrary Spots.Card where
  arbitrary = elements $ Spots.allCardsSpots
  shrink = genericShrink

instance Arbitrary Item where
  arbitrary = elements Card.allItems
  shrink = genericShrink

instance Arbitrary (ItemObject 'Core) where
  arbitrary = mkCoreItemObject <$> arbitrary
  shrink = genericShrink

instance Arbitrary Skill where
  arbitrary = genericArbitraryU
  shrink = genericShrink

instance Arbitrary Skill.State where
  arbitrary = genericArbitraryU
  shrink = genericShrink

instance Arbitrary (Creature 'Core) where
  arbitrary =
    let shared = SharedModel.unsafeGet
     in elements $
          SharedModel.getCards shared
            & mapMaybe (\case CreatureCard _ creature -> Just creature; _ -> Nothing)
            & map Card.unlift
  shrink = genericShrink

instance Arbitrary (CardCommon 'Core) where
  arbitrary = genericArbitraryU
  shrink = genericShrink

instance Arbitrary (Card 'Core) where
  arbitrary =
    let shared = SharedModel.unsafeGet
     in elements $
          SharedModel.getCards shared
            & map Card.unlift
  shrink = genericShrink

instance Arbitrary (PlayerPart 'Core) where
  arbitrary = genericArbitraryU
  shrink = genericShrink

instance Arbitrary (Board 'Core) where
  arbitrary = genericArbitraryU
  shrink = genericShrink

instance Arbitrary Tile where
  arbitrary = genericArbitraryU
  shrink = genericShrink

instance Arbitrary Spots.Player where
  arbitrary = elements $ Spots.allPlayersSpots
  shrink = genericShrink

instance Arbitrary Turn where
  arbitrary = genericArbitraryU
  shrink = genericShrink

instance Arbitrary DeathCause where
  arbitrary = genericArbitraryU
  shrink = genericShrink

instance Arbitrary InPlaceEffect where
  arbitrary = genericArbitraryU
  shrink = genericShrink

instance Arbitrary InPlaceEffects where
  arbitrary = genericArbitraryU
  shrink = genericShrink

instance Arbitrary Game.StatChange where
  arbitrary = genericArbitraryU
  shrink = genericShrink

instance Arbitrary HandIndex where
  arbitrary = genericArbitraryU
  shrink = genericShrink

instance Arbitrary Game.Target where
  arbitrary = genericArbitraryU
  shrink = genericShrink

instance Arbitrary Game.Event where
  -- FIXME @smelc Generate Event that satisfy the Neutral<->Target invariant
  arbitrary = genericArbitraryU
  shrink = genericShrink

instance Arbitrary Neighborhood where
  arbitrary = genericArbitraryU
  shrink = genericShrink

instance Arbitrary (Teams Team) where
  arbitrary = genericArbitraryU
  shrink = genericShrink

instance Arbitrary (Difficulty) where
  arbitrary = genericArbitraryU
  shrink = genericShrink

instance Arbitrary (Campaign.Outcome) where
  arbitrary = genericArbitraryU
  shrink = genericShrink

newtype SmallList a = SmallList a
  deriving (Show)

instance Arbitrary a => Arbitrary (SmallList a) where
  arbitrary = SmallList <$> resize 5 arbitrary

genSceneAst :: Int -> Gen SceneAst
genSceneAst i = do
  n <- getSize
  k <- choose (0, n)
  SceneAst <$> go k i
  where
    go :: Int -> Int -> Gen [SceneAction]
    go 0 _ = return []
    go n i =
      oneof
        [ (:)
            <$> (NewActor <$> arbitrary <*> arbitrary)
            <*> go (n -1) (i + 1),
          (:)
            <$> ((:|||:) <$> subSequence i <*> subSequence i)
            <*> go (n -1) i,
          (:)
            <$> (Fork <$> subSequence i)
            <*> go (n -1) i,
          (:)
            <$> (Wait <$> (getPositive <$> arbitrary))
            <*> go (n -1) i,
          (:)
            <$> (SetActorState <$> choose (0, n -1) <*> arbitrary)
            <*> go (n -1) i
        ]
      where
        subSequence :: Int -> Gen SceneAst
        subSequence i = scale (`div` 2) (genSceneAst i)

instance Arbitrary SceneAction where
  arbitrary = error "Arbitrary instance only exists to provide shrink"
  shrink = genericShrink

deleteOrphanInstructions :: SceneAst -> SceneAst
deleteOrphanInstructions (SceneAst actions) = SceneAst (go 0 actions)
  where
    go i (NewActor n s : k) =
      NewActor n s : go (i + 1) k
    go i (Wait d : k) =
      Wait d : go i k
    go i (SetActorState j s : k)
      | j < i = SetActorState j s : go i k
      | otherwise = go i k
    go i ((SceneAst actions1 :|||: SceneAst actions2) : k) =
      (SceneAst (go i actions1) :|||: SceneAst (go i actions2)) : go i k
    go i (Fork (SceneAst actions1) : k) =
      Fork (SceneAst (go i actions1)) : go i k
    go _ [] = []

instance Arbitrary SceneAst where
  arbitrary = genSceneAst 0

  -- This is a bit hacky: genericShrink will generate instructions of the form
  -- actor += change, where actor has not been declared yet, we simply delete
  -- these instructions. A better solution would be to write a manual shrink
  -- that only produces well-formed scenes.
  shrink = map deleteOrphanInstructions . genericShrink

astToScene :: SceneAst -> Scene ()
astToScene (SceneAst actions) = go [] actions
  where
    go _ [] = return ()
    go actors (NewActor n s : k) = do
      actor <- newActor n s
      go (actor : actors) k
    go actors (Wait d : k) = do
      wait d
      go actors k
    go actors (SetActorState a s : k) = do
      setActorState (unsafeCoerce a) s
      go actors k
    go actors ((SceneAst actions1 :|||: SceneAst actions2) : k) = do
      go actors actions1 ||| go actors actions2
      go actors k
    go actors (Fork (SceneAst actions1) : k) = do
      fork (go actors actions1)
      go actors k

instance Arbitrary Actor where
  arbitrary = genericArbitraryU
  shrink = genericShrink

instance Arbitrary Frame where
  arbitrary = genericArbitraryU
  shrink = genericShrink

instance Arbitrary Element where
  arbitrary = genericArbitraryU
  shrink = genericShrink

instance Arbitrary ActorState where
  arbitrary = genericArbitraryU
  shrink = genericShrink

instance Arbitrary Direction where
  arbitrary = genericArbitraryU
  shrink = genericShrink
