{-# LANGUAGE TemplateHaskell #-}

-- |
-- Module that depends on TH. Put the minimum in there to minimize
-- dependencies to this module.
-- |
module Custom where

import Data.Maybe
import Language.Haskell.TH.Env

-- | The value of the PCW_LOCATION environment variable if any
pcwLocation :: Maybe String
pcwLocation = $$(envQ "PCW_LOCATION")