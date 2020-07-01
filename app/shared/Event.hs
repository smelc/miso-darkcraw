{-# LANGUAGE OverloadedStrings #-}

module Event
  ( onMouseEnter',
    onMouseLeave',
  )
where

import Data.Aeson
import Miso
import Miso.String
import Update
import Data.Text (Text)

-- | Extracts the class name of the target from an event.
classNameDecoder :: Decoder Value
classNameDecoder =
  Decoder
    { decodeAt = DecodeTarget ["target"],
      decoder = withObject "target" $ \o -> o .: "className"
    }

-- | Helper function for onMouseEnter' and onMouseLeave'.
onMouseEvent :: MisoString -> Text -> Action -> Attribute Action
onMouseEvent eventName className action =
  on eventName classNameDecoder $ \(String name) ->
    if name == className then action else NoOp

-- | Like onMouseEnter, but restricts event firing to elements with the
-- provided class name.
onMouseEnter' :: Text -> Action -> Attribute Action
onMouseEnter' = onMouseEvent "mouseenter"

-- | Like onMouseLeave, but restricts event firing to elements with the
-- provided class name.
onMouseLeave' :: Text -> Action -> Attribute Action
onMouseLeave' = onMouseEvent "mouseleave"
