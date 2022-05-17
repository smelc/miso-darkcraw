module Configuration where

data Edition
  = -- 2 races
    Vanilla
  | -- 4 races
    Legendary
  deriving (Show)

data Configuration
  = -- | Dev version, only used locally.
    Dev
  | -- | Version released on https://smelc3.itch.io/pixel-card-wars
    -- This version is pushed when relevant by @smelc (where there's a devlog
    -- with it usually). Second parameter is the commit hash.
    Itch Edition String
  | -- | Version released on https://www.schplaf.org/smelc3/darkcraw
    -- This version is usually pushed at every commit by @smelc.
    -- Second parameter is the commit hash.
    Schplaf Edition String

isDev :: Bool
isDev =
  case get of
    Dev -> True
    _ -> False

-- | The current configuration
get :: Configuration
get = Dev
