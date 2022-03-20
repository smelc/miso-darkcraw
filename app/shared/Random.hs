{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Library functions about randomness
module Random where

import Control.Monad.Identity (runIdentity)
import Control.Monad.Random (runRandT)
import Control.Monad.State
import Data.Function ((&))
import qualified Data.List.NonEmpty as NE
import GHC.Base (assert)
import System.Random
import qualified System.Random.Shuffle

-- | Pick one element at random from the second argument, using the given random
-- generator. Returns the updated generator and the
-- picked element (being 'Just' if the list non-empty). See 'oneof' for
-- a variant of this function.
pick :: RandomGen r => r -> [a] -> (Maybe a, r)
pick r = \case
  [] -> (Nothing, r)
  l ->
    let (i, r') = randomR (0, length l - 1) r
     in (Just $ l !! i, r')

-- | Returns a random element from the list, if it is non empty. If
-- you a non-empty list at hand, call 'oneof' instead.
pickM ::
  RandomGen r =>
  MonadState r m =>
  -- | The elements from which something must be picked
  [a] ->
  m (Maybe a)
pickM elems =
  case elems of
    [] -> return Nothing
    _ -> do
      r <- get
      let (idx, r') = randomR (0, nbElems - 1) r
      put r'
      return $ Just $ elems !! idx
      where
        nbElems :: Int = length elems

-- | Shuffles the second argument with the random generator.
-- Returns the shuffle and the updated generator.
shuffle :: RandomGen r => r -> [a] -> ([a], r)
shuffle r l =
  System.Random.Shuffle.shuffleM l
    & flip runRandT r
    & runIdentity

-- | Shuffles the given list with a random generator
shuffleM :: RandomGen r => MonadState r m => [a] -> m [a]
shuffleM =
  \case
    [] -> pure []
    l -> do
      shared <- get
      let (l', r') = shuffle shared l
      put r'
      return l'

-- | Returns a random element from the list. If you have a possibly empty list
-- at hand, call 'pickM' instead.
oneof ::
  RandomGen r =>
  MonadState r m =>
  -- | The elements from which something must be picked
  NE.NonEmpty a ->
  m a
oneof elems = do
  r <- get
  let (idx, r') = randomR (0, nbElems - 1) r
  put r'
  return $ elems NE.!! idx
  where
    nbElems :: Int = NE.length elems

-- | 'roll min max' returns a value between 'min' (included) and 'max'
-- (included). 'min <= max' should hold.
roll ::
  RandomGen r =>
  MonadState r m =>
  Random p =>
  Ord p =>
  -- | The minimum element
  p ->
  -- | The maximum element
  p ->
  m p
roll min max = do
  r <- get
  let (res, r') = assert (min <= max) $ randomR (min, max) r
  put r'
  return res
