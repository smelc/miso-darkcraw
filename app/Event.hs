{-# LANGUAGE OverloadedStrings #-}

module Event where

import Data.Aeson
import Miso
import Miso.String
import Update

clientXDecoder :: Decoder (Int, Int)
clientXDecoder = mempty `at` withObject "xy" parser
  where
    parser o = (,) <$> o .: "clientX" <*> o .: "clientY"

onEvent ::
  -- | The event on which to apply
  MisoString ->
  -- | How to build the Action
  Attribute Action
onEvent s =
  on s clientXDecoder $ uncurry DragXY
