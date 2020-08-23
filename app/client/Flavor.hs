module Flavor where

data Edition
  = -- 2 races
    Vanilla
  | -- 4 races
    Legendary
  deriving (Show)

data Location
  = -- | Dev version: https://www.schplaf.org/hgames/darkcraw/
    -- | This version is pushed regularly by @smelc
    Dev
  | -- | Version released on https://hgames.itch.io/pixel-card-wars
    -- | This version is pushed when relevant by @smelc (where there's a devlog
    -- | with it usually)
    Itch
  deriving (Show)

data Configuration = Configuration Edition Location

-- | Given the value of PCW_LOCATION, the configuration
configuration :: Maybe String -> Configuration
configuration Nothing = Configuration Legendary Dev
configuration (Just "itch") = Configuration Legendary Itch
configuration (Just s) = error $ "Unrecognized PCW_LOCATION: " ++ s
