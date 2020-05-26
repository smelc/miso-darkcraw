{-# LANGUAGE OverloadedStrings #-}

module Event
  ( onEvent,
    onMouseEnter',
    onMouseLeave',
  )
where

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

-- | Extracts the class name of the target from an event.
classNameDecoder :: Decoder Value
classNameDecoder =
  Decoder
    { decodeAt = DecodeTarget ["target"],
      decoder = withObject "target" $ \o -> o .: "className"
    }

-- | Helper function for onMouseEnter' and onMouseLeave'.
onMouseEvent eventName className action =
  on eventName classNameDecoder $ \(String name) ->
    if name == className then action else NoOp

-- | Like onMouseEnter, but restricts event firing to elements with the
-- provided class name.
onMouseEnter' = onMouseEvent "mouseenter"

-- | Like onMouseLeave, but restricts event firing to elements with the
-- provided class name.
onMouseLeave' = onMouseEvent "mouseleave"
