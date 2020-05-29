{-# LANGUAGE OverloadedStrings #-}

module Event
  ( onDragXYEvent,
    onMouseEnter',
    onMouseLeave',
  )
where

import Data.Aeson
import Miso
import Miso.String
import Update

clientXYDecoder :: Decoder (Int, Int)
clientXYDecoder = mempty `at` withObject "xy" parser
  where
    parser o = (,) <$> o .: "clientX" <*> o .: "clientY"

onDragXYEvent ::
  -- | The event on which to apply
  MisoString ->
  -- | How to build the Action
  Attribute Action
onDragXYEvent s =
  on s clientXYDecoder $ uncurry DragXY

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
