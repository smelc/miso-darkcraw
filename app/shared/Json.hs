{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
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
import Damage (Damage (..))
import Data.Aeson
import Data.Aeson.Types (Parser)
import qualified Data.Bifunctor
import Data.ByteString.Lazy hiding (map)
import Data.Function ((&))
import Data.List.Extra (lower)
import Data.Text (Text, splitOn)
import qualified Data.Text as Text
import qualified Data.Text.Encoding
import GHC.Generics
import JsonData
import qualified Mana
import Nat
import Skill (Skill)
import qualified Skill
import Text.Read
import Tile (Filepath, Tile, TileUI)

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
          "Ace" -> return Skill.Ace
          "Assassin" -> return Skill.Assassin
          "Blow" -> return $ Skill.Blow ()
          "Brainless" -> return Skill.Brainless
          "BreathIce" -> return Skill.BreathIce
          "Charge" -> return Skill.Charge
          "Discipline" -> return Skill.Discipline
          "DrawCard" -> return $ Skill.DrawCard ()
          "Falconer" -> return Skill.Falconer
          "Fear" -> return $ Skill.Fear ()
          "FearTmp" -> return Skill.FearTmp
          "Flying" -> return Skill.Flying
          "GreenAffinity" -> return $ Skill.GreenAffinity ()
          "Growth" -> return $ Skill.Growth ()
          "Imprecise" -> return Skill.Imprecise
          "King" -> return Skill.King
          "Knight" -> return Skill.Knight
          "LongReach" -> return Skill.LongReach
          "Powerful" -> return Skill.Powerful
          "Ranged" -> return Skill.Ranged
          "Rampage" -> return $ Skill.Rampage
          "Sadism" -> return Skill.Sadism
          "Slow" -> return $ Skill.Slow ()
          "Sylvan" -> return Skill.Sylvan
          "Squire" -> return Skill.Squire
          "StrengthPot" -> return $ Skill.StrengthPot
          "Stupid4" -> return $ Skill.Stupid4 ()
          "Support" -> return Skill.Support
          "Terror" -> return $ Skill.Terror ()
          "Unique" -> return Skill.Unique
          "Veteran" -> return Skill.Veteran
          "Zealot" -> return Skill.Zealot
          s ->
            case Data.Text.splitOn " " s of
              ["Source", n] -> ofNat Skill.Source n
              ["Fame", n] -> ofNat Skill.Fame n
              ["Regeneration", n] -> ofNat Skill.Regeneration n
              _ -> fail $ "Invalid Skill string: " ++ show s ++ " (did you forget to handle a new Skill in FromJSON Skill in Json.hs?)"
      ofNat mkS n = case readMaybe $ Text.unpack n of
        Nothing -> fail $ "Invalid Source suffix (not a Nat): " ++ Text.unpack n
        Just n -> return $ mkS n

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
    attack :: Damage,
    mana :: Mana.Mana,
    skills :: [Skill],
    text :: Maybe String,
    textSzOffset :: Int,
    tile :: Tile
  }
  deriving (Show)

instance FromJSON Mana.Mana where
  parseJSON = withText "Mana" go
    where
      go :: Text -> Parser Mana.Mana
      go s =
        case Mana.read (Text.unpack s) of
          Just m -> return m
          Nothing -> fail $ "Invalid mana string: " ++ show (Text.unpack s)

instance FromJSON Damage where
  parseJSON = withText "Attack" go
    where
      go :: Text -> Parser Damage
      go s =
        case readMaybe $ Text.unpack s of
          -- Parse "1", "2", etc.
          Just (n :: Nat) -> return $ mempty {base = n}
          Nothing ->
            -- Parse "1-3", "0-2", etc.
            case Data.Text.splitOn "-" s of
              [base, variance] ->
                case both (readMaybe . Text.unpack) (base, variance) of
                  (Just base, Just variance) -> return $ Damage {base, variance}
                  _ -> err s
              _ -> err s
      err s = fail $ "Invalid attack string: " ++ (show $ Text.unpack s)
      both f = Data.Bifunctor.bimap f f

instance FromJSON CreatureObjectJSON where
  parseJSON = withObject "Creature" $ \v ->
    CreatureObjectJSON
      <$> v .: "id"
      <*> v .: "hp"
      <*> v .: "attack"
      <*> v .:? "mana" .!= (Mana.Const defaultManaCost)
      <*> v .:? "skills" .!= []
      <*> v .:? "text"
      <*> v .:? "text_sz_offset" .!= 0
      <*> v .: "tile"

instance FromJSON Neutral where
  parseJSON = genericParseJSON toLowerConstructorOptions

data NeutralObjectJSON = NeutralObjectJSON
  { neutral :: Neutral,
    teams :: [Team],
    mana :: Nat,
    text :: String,
    textSzOffset :: Int,
    tile :: Tile,
    title :: String,
    titleSzOffset :: Int
  }
  deriving (Show)

instance FromJSON NeutralObjectJSON where
  parseJSON = withObject "Neutral" $ \v ->
    NeutralObjectJSON
      <$> v .: "name"
      <*> v .: "teams"
      <*> v .:? "mana" .!= defaultManaCost
      <*> v .: "text"
      <*> v .:? "text_sz_offset" .!= 0
      <*> v .: "tile"
      <*> v .: "title"
      <*> v .:? "title_sz_offset" .!= 0

instance FromJSON Item where
  parseJSON = genericParseJSON toLowerConstructorOptions

data ItemObjectJSON = ItemObjectJSON
  { item :: Item,
    mana :: Nat,
    teams :: [Team],
    text :: String,
    textSzOffset :: Int,
    tile :: Tile,
    title :: String,
    titleSzOffset :: Int
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

instance FromJSON Skill.Pack where
  parseJSON = genericParseJSON skillUIOptions

data AllData = AllData
  { creatures :: [CreatureObjectJSON],
    neutrals :: [NeutralObjectJSON],
    items :: [ItemObjectJSON],
    skillsPack :: [Skill.Pack],
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

type LoadedJson = ([Card 'UI], [Skill.Pack], [TileUI])

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
        (CardCommon {..})
        (Creature {items = [], moral = 0, transient = (), ..})
    mkItemCard :: ItemObjectJSON -> Card 'UI
    mkItemCard ItemObjectJSON {text = t, ..} =
      ItemCard
        (CardCommon {mana = Mana.Const mana, text = Just t, ..})
        (ItemObject {..})
    mkNeutralCard :: NeutralObjectJSON -> Card 'UI
    mkNeutralCard NeutralObjectJSON {text = t, ..} =
      NeutralCard
        (CardCommon {mana = Mana.Const mana, text = Just t, ..})
        (NeutralObject {..})

loadJson :: Either String LoadedJson
loadJson =
  parseJson bs
  where
    bs = Data.Text.Encoding.encodeUtf8 jsonData & Data.ByteString.Lazy.fromStrict
