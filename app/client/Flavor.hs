module Flavor where

data Edition
  = -- 2 races
    Vanilla
  | -- 4 races
    Legendary

data Location
  -- Dev version
  = Dev
  --- Version released on https://hgames.itch.io/pixel-card-wars
  | Itch

data Configuration = Configuration Edition Location

-- | Given the value of PCW_LOCATION, the configuration
configuration :: Maybe String -> Configuration
configuration Nothing = Configuration Legendary Dev
configuration (Just "itch") = Configuration Legendary Itch
configuration (Just s) = error $ "Unrecognized PCW_LOCATION: " ++ s
