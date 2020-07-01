{-# LANGUAGE TemplateHaskell #-}

module JsonData where

import qualified Data.ByteString
import Data.FileEmbed

jsonData :: Data.ByteString.ByteString
jsonData = $(embedFile "data.json")
