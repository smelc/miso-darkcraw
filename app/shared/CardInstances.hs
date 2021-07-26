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
  -- to use it is when you initially place the card (A)
  start (Fear' False) = Fear' True -- Renew it
  start (Source' n True) = Source' n False -- Always turn OFF, as
  -- it is simultaneously being consumed in the Startable instance of
  -- Board 'Core. This means that the only time this skill appears
  -- not being used is when you place the card. After, it's marked
  -- used at the start of your turn.
  start s@(Stupid4' _) | isStupid s = Stupid4' 0
  start (Stupid4' i) = Stupid4' $ i + 1
  start a = a

instance Startable (Creature 'Core) where
  start Creature {..} = Creature {skills = map start skills, ..}
