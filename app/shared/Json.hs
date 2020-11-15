{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Json
  ( loadJson,
    LoadedJson,
  )
where

import Card
import Data.Aeson
import Data.ByteString.Lazy
import Data.Function ((&))
import Data.List.Extra (lower)
import qualified Data.Text.Encoding
import GHC.Generics
import JsonData
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

instance FromJSON Skill

neutralObjectOptions :: Options
neutralObjectOptions =
  defaultOptions
    { fieldLabelModifier = impl
    }
  where
    impl "neutral" = "name"
    impl "neutralTeams" = "teams"
    impl s = s

itemObjectOptions :: Options
itemObjectOptions =
  defaultOptions
    { fieldLabelModifier = impl
    }
  where
    impl "item" = "name"
    impl s = s

skillUIOptions :: Options
skillUIOptions =
  defaultOptions
    { fieldLabelModifier = impl
    }
  where
    impl "skillText" = "text"
    impl "skillTitle" = "title"
    impl s = s

instance FromJSON (Creature UI) where
  parseJSON = withObject "Creature" $ \v ->
    Creature
      <$> v .: "id"
      <*> v .: "hp"
      <*> v .: "attack"
      <*> v .:? "moral"
      <*> v .: "victory_points"
      <*> v .:? "skills" .!= []
      <*> v .: "filepath"

instance FromJSON Neutral where
  parseJSON = genericParseJSON toLowerConstructorOptions

instance FromJSON (NeutralObject UI) where
  parseJSON = genericParseJSON neutralObjectOptions

instance FromJSON Item where
  parseJSON = genericParseJSON toLowerConstructorOptions

instance FromJSON ItemObject where
  parseJSON = genericParseJSON itemObjectOptions

instance FromJSON Tile

instance FromJSON TileUI

instance FromJSON SkillUI where
  parseJSON = genericParseJSON skillUIOptions

-- TODO @smelc remove Phase parameter, only UI makes sense
data AllData (p :: Phase) = AllData
  { creatures :: [Creature p],
    neutral :: [NeutralObject p],
    items :: [ItemObject],
    skills :: [SkillUI],
    tiles :: [TileUI]
  }
  deriving (Generic)

deriving instance Forall Show p => Show (AllData p)

instance FromJSON (AllData UI)

type LoadedJson = ([Card UI], [SkillUI], [TileUI])

parseJson ::
  -- | The content of data.json
  ByteString ->
  Either String LoadedJson
parseJson json = do
  AllData creatures neutral items skills tiles <- eitherDecode json
  let creatureCards = Prelude.map CreatureCard creatures
      neutralCards = Prelude.map NeutralCard neutral
      itemCards = Prelude.map (ItemCard . Card.item) items
      allCards = creatureCards ++ neutralCards ++ itemCards
  return (allCards, skills, tiles)

loadJson :: Either String LoadedJson
loadJson =
  parseJson bs
  where
    bs = Data.Text.Encoding.encodeUtf8 jsonData & Data.ByteString.Lazy.fromStrict
