module Configuration where

data Edition
  = -- 2 races
    Vanilla
  | -- 4 races
    Legendary
  deriving (Show)

data Configuration
  = -- | Dev version: https://www.schplaf.org/hgames/darkcraw/
    -- This version is pushed regularly by @smelc. Second parameter
    -- is the commit hash.
    Dev
  | -- | Version released on https://hgames.itch.io/pixel-card-wars
    -- This version is pushed when relevant by @smelc (where there's a devlog
    -- with it usually). Second parameter is the commit hash.
    Itch Edition String
  | Schplaf Edition String

isDev :: Bool
isDev =
  case get of
    Dev -> True
    _ -> False

-- | The current configuration
get :: Configuration
get = Schplaf Legendary "4c39200"
