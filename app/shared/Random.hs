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

-- | Pick one element at random from the second argument, using the random
-- generator of 'SharedModel'. Returns the updated 'SharedModel' and the
-- picked element (being 'Just' if the list non-empty). See 'oneof' for
-- a variant of this function.
--
-- TODO @smelc change order of returned pair, to be consistent with 'RandomGen'
pick :: RandomGen r => r -> [a] -> (r, Maybe a)
pick r = \case
  [] -> (r, Nothing)
  l ->
    let (i, r') = randomR (0, length l - 1) r
     in (r', Just $ l !! i)

-- | Shuffles the second argument with the random generator
-- of 'SharedModel'. Returns the shuffle and the updated 'SharedModel'
--
-- TODO @smelc change order of returned pair, to be consistent with 'RandomGen'
shuffle :: RandomGen r => r -> [a] -> (r, [a])
shuffle r l =
  (r', l')
  where
    (l', r') =
      System.Random.Shuffle.shuffleM l
        & flip runRandT r
        & runIdentity

-- | Shuffles the given list with the random generator of 'SharedModel'
shuffleM :: RandomGen r => MonadState r m => [a] -> m [a]
shuffleM =
  \case
    [] -> pure []
    l -> do
      shared <- get
      let (shared', l') = shuffle shared l
      put shared'
      return l'

-- | Returns a random element from the list, using 'sharedStdGen' as
-- the random generator.
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
