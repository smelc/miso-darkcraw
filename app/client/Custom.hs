{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

-- |
-- Module that depends on TH. Put the minimum in there to minimize
-- dependencies to this module.
-- |
module Custom where

import Data.Maybe
import Debug.Trace (trace)
import Language.Haskell.TH.Env

-- | The value of the PCW_LOCATION environment variable if any
pcwLocation :: Maybe String
pcwLocation =
  trace ("PCW_LOCATION is " ++ show r) r
  where
    r :: Maybe String = $$(envQ "PCW_LOCATION")
