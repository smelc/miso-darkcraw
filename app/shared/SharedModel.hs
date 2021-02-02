{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

-- |
-- This module contains the subset of the model that is needed by 'Game'
-- I didn't want to make 'Game' depend on 'Model', because it seemed overkill
-- and dangerous cyclewise in the long run. Hence I introduced this module.
-- |
module SharedModel
  ( identToCard,
    idToCreature,
    liftCard,
    SharedModel,
    tileToFilepath,
    unsafeGet,
    unsafeGetSeed,
    unsafeIdentToCard,
    unsafeLiftCard,
    SharedModel.liftSkill,
    shuffle,
    create,
    SharedModel.getStdGen,
    getCards,
    getCmd,
    withCmd,
    withStdGen,
    getCardIdentifiers,
    cardToFilepath,
    withSeed,
    getAllCommands,
    identToItem,
    identToNeutral,
  )
where

import Card hiding (ID)
import qualified Card
import Command
import Control.Monad.Identity (Identity (runIdentity))
import Control.Monad.Random
import Data.Foldable (find)
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.List
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe
import GHC.Generics (Generic)
import Json (loadJson)
import System.Random.Shuffle (shuffleM)
import Tile (Filepath, Tile, TileUI (..), default16Filepath, default24Filepath)

instance Eq StdGen where
  std1 == std2 = show std1 == show std2

-- | The part of the model that is likely to be used by all pages
-- | i.e. all possible models
data SharedModel = SharedModel
  -- XXX @smelc, turn those into maps, for efficiency
  { -- | Data obtained at load time, that never changes
    sharedCards :: Map Card.ID (Card UI),
    -- | The current debug command (in dev mode only)
    sharedCmd :: Maybe String,
    sharedSkills :: Map Skill SkillPack,
    sharedTiles :: Map Tile TileUI,
    -- | RNG obtained at load time, to be user whenever randomness is needed
    sharedStdGen :: StdGen
  }
  deriving (Eq, Generic, Show)

create :: [Card UI] -> [SkillPack] -> [TileUI] -> StdGen -> SharedModel
create cards skills tiles sharedStdGen =
  SharedModel {..}
  where
    groupBy f l = map (\e -> (f e, e)) l & Map.fromList
    sharedCards = groupBy cardToIdentifier cards
    sharedSkills = groupBy skill skills
    sharedTiles = groupBy Tile.tile tiles
    sharedCmd = Nothing

cardToFilepath :: SharedModel -> Card UI -> Filepath
cardToFilepath SharedModel {..} = \case
  CreatureCard Creature {tile} ->
    fromMaybe default24Filepath $ sharedTiles Map.!? tile <&> filepath
  NeutralCard NeutralObject {ntile} ->
    fromMaybe default16Filepath $ sharedTiles Map.!? ntile <&> filepath
  ItemCard ItemObject {itile} ->
    fromMaybe default16Filepath $ sharedTiles Map.!? itile <&> filepath

getAllCommands :: SharedModel -> [Command]
getAllCommands shared =
  map Gimme cids ++ [Goto v | v <- Command.allViews]
  where
    cids =
      getCardIdentifiers shared
        & map (\case IDC cid -> Just cid; _ -> Nothing)
        & catMaybes

-- & sortBy (\a b -> compare (show a) (show b)) TODO @smelc sort by team, then by string

getCardIdentifiers :: SharedModel -> [Card.ID]
getCardIdentifiers SharedModel {sharedCards} = Map.keys sharedCards

getCards :: SharedModel -> [Card UI]
getCards SharedModel {sharedCards} = Map.elems sharedCards

getCmd :: SharedModel -> Maybe String
getCmd SharedModel {sharedCmd} = sharedCmd

getStdGen :: SharedModel -> StdGen
getStdGen SharedModel {sharedStdGen} = sharedStdGen

withCmd :: SharedModel -> Maybe String -> SharedModel
withCmd shared cmd = shared {sharedCmd = cmd}

withSeed :: SharedModel -> Int -> SharedModel
withSeed shared seed = shared {sharedStdGen = mkStdGen seed}

withStdGen :: SharedModel -> StdGen -> SharedModel
withStdGen shared stdgen = shared {sharedStdGen = stdgen}

-- | An instance of 'SharedModel' that is fine for debugging. Don't use
-- it in production!
unsafeGet :: SharedModel
unsafeGet = unsafeGetSeed 42

-- TODO @smelc Rename me into createWithSeed. This method is not unsafe
-- because obviously, if loadJson fails, the entire game is broken;
-- so it's fine erroring out.

-- | An instance of 'SharedModel' obtained by reading the Json data
unsafeGetSeed :: Int -> SharedModel
unsafeGetSeed seed =
  case loadJson of
    Left err -> error err
    Right (cards, _, skills, tiles) -> create cards skills tiles $ mkStdGen seed

-- Instead of returning Maybe here, I should add a test that all
-- Card.ID are mapped by SharedModel and error out
identToCard :: SharedModel -> Card.ID -> Maybe (Card UI)
identToCard SharedModel {sharedCards} cid = sharedCards Map.!? cid

-- Instead of returning Maybe here, I should add a test that all
-- Item are mapped by SharedModel and error out
identToItem :: SharedModel -> Item -> Maybe (ItemObject UI)
identToItem SharedModel {sharedCards} i =
  sharedCards Map.!? IDI i >>= cardToItemObject

-- Instead of returning Maybe here, I should add a test that all
-- CreatureID are mapped by SharedModel and error out
idToCreature :: SharedModel -> CreatureID -> Maybe (Creature UI)
idToCreature SharedModel {sharedCards} cid =
  sharedCards Map.!? IDC cid >>= cardToCreature

-- Instead of returning Maybe here, I should add a test that all
-- Neutral are mapped by SharedModel and error out
identToNeutral :: SharedModel -> Neutral -> Maybe (NeutralObject UI)
identToNeutral SharedModel {sharedCards} n =
  sharedCards Map.!? IDN n >>= cardToNeutralObject

liftCard :: SharedModel -> Card Core -> Maybe (Card UI)
liftCard shared = \case
  CreatureCard creature -> CreatureCard <$> liftCreature shared creature
  NeutralCard n -> NeutralCard <$> liftNeutralObject shared n
  ItemCard i -> ItemCard <$> liftItemObject shared i

liftItemObject :: SharedModel -> ItemObject Core -> Maybe (ItemObject UI)
liftItemObject shared ItemObject {item} =
  identToItem shared item

liftNeutralObject :: SharedModel -> NeutralObject Core -> Maybe (NeutralObject UI)
liftNeutralObject shared NeutralObject {neutral} =
  identToNeutral shared neutral

-- | Translates a 'Core' 'Creature' into an 'UI' one, keeping its stats
-- An alternative implementation could return the pristine, formal, UI card.
liftCreature :: SharedModel -> Creature Core -> Maybe (Creature UI)
liftCreature SharedModel {sharedCards} c@Creature {..} =
  case sharedCards Map.!? IDC creatureId of
    Nothing -> Nothing
    Just (CreatureCard Creature {tile}) -> Just $ Creature {skills = map Card.liftSkill skills, ..}
    Just card -> error $ "Creature " ++ show c ++ " mapped in UI to: " ++ show card

liftSkill :: SharedModel -> Skill -> SkillPack
liftSkill SharedModel {sharedSkills} skill =
  fromMaybe
    default_
    $ find (\SkillPack {skill = sk} -> sk == skill) sharedSkills
  where
    default_ = SkillPack {skill = LongReach, ..}
    skillText = show skill ++ " not found!"
    skillTitle = skillText

shuffle :: SharedModel -> [a] -> (SharedModel, [a])
shuffle shared@SharedModel {sharedStdGen} l =
  (shared {sharedStdGen = stdgen'}, l')
  where
    (l', stdgen') = shuffleM l & flip runRandT sharedStdGen & runIdentity

tileToFilepath :: SharedModel -> Tile -> Filepath
tileToFilepath SharedModel {sharedTiles} tile =
  case find (\TileUI {tile = t} -> t == tile) sharedTiles of
    Nothing -> default24Filepath
    Just TileUI {Tile.filepath} -> filepath

unsafeIdentToCard :: SharedModel -> Card.ID -> Card UI
unsafeIdentToCard smodel ci = identToCard smodel ci & fromJust

unsafeLiftCard :: SharedModel -> Card Core -> Card UI
unsafeLiftCard s c = liftCard s c & fromJust
