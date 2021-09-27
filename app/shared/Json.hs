{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Json
  ( loadJson,
    LoadedJson,
  )
where

import Card
import Constants (defaultManaCost)
import Data.Aeson
import Data.Aeson.Types (Parser)
import Data.ByteString.Lazy hiding (map)
import Data.Function ((&))
import Data.List.Extra (lower)
import Data.Text (Text, splitOn)
import qualified Data.Text as Text
import qualified Data.Text.Encoding
import GHC.Generics
import JsonData
import Nat
import Text.Read
import Tile

instance FromJSON Team where
  parseJSON = genericParseJSON toLowerConstructorOptions

instance FromJSON CreatureKind where
  parseJSON = genericParseJSON toLowerConstructorOptions

creatureIDOptions :: Options
creatureIDOptions =
  defaultOptions
    { fieldLabelModifier = \case
        "creatureKind" -> "name"
        s -> s
    }

instance FromJSON CreatureID where
  parseJSON = genericParseJSON creatureIDOptions

filepathOptions :: Options
filepathOptions =
  defaultOptions
    { fieldLabelModifier = \case
        "fpX" -> "x"
        "fpY" -> "y"
        s -> s
    }

instance FromJSON Filepath where
  parseJSON = genericParseJSON filepathOptions

toLowerConstructorOptions :: Options
toLowerConstructorOptions =
  defaultOptions
    { constructorTagModifier = lower
    }

instance FromJSON Skill where
  parseJSON = withText "Skill" go
    where
      go :: Text -> Parser Skill =
        \case
          "Blow" -> return Blow
          "BreathIce" -> return BreathIce
          "Discipline" -> return Discipline
          "DrawCard" -> return DrawCard
          "Fear" -> return Fear
          "LongReach" -> return LongReach
          "Ranged" -> return Ranged
          "Stupid4" -> return Stupid4
          "Terror" -> return Terror
          "Unique" -> return Unique
          s ->
            case Data.Text.splitOn " " s of
              ["Source", n] ->
                case readMaybe $ Text.unpack n of
                  Nothing -> fail $ "Invalid Source suffix (not a Nat): " ++ Text.unpack n
                  Just n -> return $ Source n
              _ -> fail $ "Invalid Skill string: " ++ show s ++ " (did you forget to update Json.hs?)"

-- TODO @smelc Rename me into skillPackOptions
skillUIOptions :: Options
skillUIOptions =
  defaultOptions
    { fieldLabelModifier = impl
    }
  where
    impl "skillText" = "text"
    impl "skillTitle" = "title"
    impl s = s

data CreatureObjectJSON = CreatureObjectJSON
  { creatureId :: CreatureID,
    hp :: Nat,
    attack :: Nat,
    mana :: Nat,
    skills :: [Skill],
    text :: Maybe String,
    tile :: Tile
  }
  deriving (Show)

instance FromJSON CreatureObjectJSON where
  parseJSON = withObject "Creature" $ \v ->
    CreatureObjectJSON
      <$> v .: "id"
      <*> v .: "hp"
      <*> v .: "attack"
      <*> v .:? "mana" .!= defaultManaCost
      <*> v .:? "skills" .!= []
      <*> v .:? "text"
      <*> v .: "tile"

instance FromJSON Neutral where
  parseJSON = genericParseJSON toLowerConstructorOptions

data NeutralObjectJSON = NeutralObjectJSON
  { neutral :: Neutral,
    neutralTeams :: [Team],
    nmana :: Nat,
    ntext :: String,
    ntile :: Tile,
    ntitle :: String
  }
  deriving (Show)

instance FromJSON NeutralObjectJSON where
  parseJSON = withObject "Neutral" $ \v ->
    NeutralObjectJSON
      <$> v .: "name"
      <*> v .: "teams"
      <*> v .:? "mana" .!= defaultManaCost
      <*> v .: "text"
      <*> v .: "tile"
      <*> v .: "title"

instance FromJSON Item where
  parseJSON = genericParseJSON toLowerConstructorOptions

data ItemObjectJSON = ItemObjectJSON
  { item :: Item,
    imana :: Nat,
    teams :: [Team],
    itext :: String,
    itextSzOffset :: Int,
    itile :: Tile,
    ititle :: String,
    ititleSzOffset :: Int
  }
  deriving (Generic, Show)

instance FromJSON ItemObjectJSON where
  parseJSON = withObject "Item" $ \v ->
    ItemObjectJSON
      <$> v .: "name"
      <*> v .:? "mana" .!= defaultManaCost
      <*> v .:? "teams" .!= allTeams
      <*> v .: "text"
      <*> v .:? "text_sz_offset" .!= 0
      <*> v .: "tile"
      <*> v .: "title"
      <*> v .:? "title_sz_offset" .!= 0

instance FromJSON Tile

instance FromJSON TileUI

instance FromJSON SkillPack where
  parseJSON = genericParseJSON skillUIOptions

data AllData = AllData
  { creatures :: [CreatureObjectJSON],
    neutrals :: [NeutralObjectJSON],
    items :: [ItemObjectJSON],
    skillsPack :: [SkillPack],
    tiles :: [TileUI]
  }
  deriving (Generic, Show)

instance FromJSON AllData where
  parseJSON =
    genericParseJSON
      defaultOptions
        { fieldLabelModifier = \case
            "skillsPack" -> "skills"
            s -> s
        }

type LoadedJson = ([Card 'UI], [SkillPack], [TileUI])

parseJson ::
  -- | The content of data.json
  ByteString ->
  Either String LoadedJson
parseJson json = do
  AllData creatures neutral items skills tiles <- eitherDecode json
  let creatureCards = map mkCreatureCard creatures
      itemCards = map mkItemCard items
      neutralCards = map mkNeutralCard neutral
      allCards = creatureCards ++ itemCards ++ neutralCards
  return (allCards, skills, tiles)
  where
    mkCreatureCard :: CreatureObjectJSON -> Card 'UI
    mkCreatureCard CreatureObjectJSON {..} =
      CreatureCard
        (CardCommon mana tile)
        (Creature {items = [], moral = 0, transient = (), ..})
    mkItemCard :: ItemObjectJSON -> Card 'UI
    mkItemCard ItemObjectJSON {..} =
      ItemCard
        (CardCommon {mana = imana, tile = itile})
        (ItemObject {..})
    mkNeutralCard :: NeutralObjectJSON -> Card 'UI
    mkNeutralCard NeutralObjectJSON {..} =
      NeutralCard
        (CardCommon {mana = nmana, tile = ntile})
        (NeutralObject {..})

loadJson :: Either String LoadedJson
loadJson =
  parseJson bs
  where
    bs = Data.Text.Encoding.encodeUtf8 jsonData & Data.ByteString.Lazy.fromStrict
