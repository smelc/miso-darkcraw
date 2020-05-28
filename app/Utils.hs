module Utils where

import Data.Map.Strict as Map
import Miso
import Miso.String

style1_ :: MisoString -> MisoString -> Attribute action
style1_ k v = style_ $ Map.fromList [(k, v)]
