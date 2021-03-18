{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}

-- | This module contains instances of classes regarding types defined
-- in 'Card'.
-- |
module CardInstances where

import Card

-- | Class for things that can be reset at the start of a turn
class Startable a where
  start :: a -> a
  start = id

instance Startable SkillCore where
  start (DrawCard' False) = DrawCard' True -- Renew it
  start (Blow' True) = Blow' False -- Always turn OFF, meaning the only change
  -- to use it is when you initially place the card
  start (Fear' False) = Blow' True -- Renew it
  start s@(Stupid4' _) | isStupid s = Stupid4' 0
  start (Stupid4' i) = Stupid4' $ i + 1
  start a = a

instance Startable (Creature 'Core) where
  start Creature {..} = Creature {skills = map start skills, ..}
