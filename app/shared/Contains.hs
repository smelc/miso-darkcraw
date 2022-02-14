{-# LANGUAGE MultiParamTypeClasses #-}

-- | Module concerned with the 'Contains' class, that deals with types
-- containing another type.
module Contains where

import Control.Monad.Except

-- | Class to extract a piece 'b' from a type 'a', with the ability to
-- set such a piece into an 'a'. Used for calling 'onContained' and also
-- because `with` is convenient.
--
-- Please refrain from writing convenience instances for setting a single
-- field of a record, and also from writing the automatic lifting
-- @Contains a b => Contains a c => Contains (b, c)@. That would yield
-- harder to understand code, for not much value.
class Contains a b where
  to :: a -> b
  with :: a -> b -> a

-- | @onContained f a@ applies 'f' on the subset of 'a' of type 'b' and
-- then returns a variant of 'a' where the subset has been mapped over.
onContained :: Contains a b => (b -> b) -> a -> a
onContained f a = a `with` (f (to a))

-- | @onContainedE f a@ applies 'f' on the subset of 'a' of type 'b' and
-- then returns a variant of 'a' where the subset has been mapped over. Fails
-- if the application on the subset fails.
onContainedE :: Contains a b => MonadError e m => (b -> m b) -> a -> m a
onContainedE f a = do
  b' <- f (to a)
  return $ a `with` b'
