module Configuration where

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

type Hash = Maybe String

data Configuration = Configuration Edition Location Hash

-- | The current configuration
configuration :: Configuration
configuration = Configuration Legendary Dev Nothing