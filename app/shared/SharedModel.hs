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
    unsafeIdentToCard,
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
    pick,
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
import GHC.Base (assert)
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

-- | An instance of 'SharedModel' obtained by reading the Json data
createWithSeed :: Int -> SharedModel
createWithSeed seed =
  case loadJson of
    Left err -> error err
    Right (cards, _, skills, tiles) -> create cards skills tiles $ mkStdGen seed

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
  map Gimme cids
    ++ map Gimme items
    ++ map Gimme neutrals
    ++ [Goto v | v <- Command.allViews]
  where
    cids =
      getCardIdentifiers shared
        & map (\case IDC cid _ -> Just cid; _ -> Nothing)
        & catMaybes
        & sortBy compareCID
        & map (\cid -> IDC cid [])
    compareCID
      CreatureID {creatureKind = ck1, team = t1}
      CreatureID {creatureKind = ck2, team = t2} =
        case compare t1 t2 of
          EQ -> compare ck1 ck2
          x -> x
    items =
      getCardIdentifiers shared
        & map (\case IDI i -> Just i; _ -> Nothing)
        & catMaybes
        & sortOn show
        & map IDI
    neutrals =
      getCardIdentifiers shared
        & map (\case IDN n -> Just n; _ -> Nothing)
        & catMaybes
        & sortOn show
        & map IDN

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
unsafeGet = createWithSeed 42

identToCard :: SharedModel -> Card.ID -> Maybe (Card UI)
identToCard s@SharedModel {sharedCards} (IDC creatureId items) =
  CreatureCard <$> idToCreature s creatureId items
identToCard SharedModel {sharedCards} id = sharedCards Map.!? id

identToItem :: SharedModel -> Item -> ItemObject UI
identToItem SharedModel {sharedCards} i =
  case sharedCards Map.!? IDI i of
    -- This should not happen, see 'testShared'
    Nothing -> error $ "Unmapped item: " ++ show i
    Just (ItemCard res) -> res
    -- To avoid this case, I could split the cards in SharedModel
    Just w -> error $ "Item " ++ show i ++ " not mapped to ItemCard, found " ++ show w ++ " instead."

idToCreature :: SharedModel -> CreatureID -> [Item] -> Maybe (Creature UI)
idToCreature s@SharedModel {sharedCards} cid items =
  -- Because creatures in data.json don't have items, we send []:
  case sharedCards Map.!? IDC cid [] of
    Nothing -> Nothing
    Just (CreatureCard c@Creature {items = is}) ->
      -- and then we fill the result with the expected items:
      assert (null is) $
        let itemsUI = map (liftItemObject s . mkCoreItemObject) items
         in Just $ c {items = itemsUI}
    Just c -> error "Unexpected card. Expected CreatureCard."

identToNeutral :: SharedModel -> Neutral -> NeutralObject UI
identToNeutral SharedModel {sharedCards} n =
  case sharedCards Map.!? IDN n of
    Nothing -> error $ "Unmapped neutral: " ++ show n
    Just (NeutralCard res) -> res
    -- To avoid this case, I could split the cards in SharedModel
    Just w -> error $ "Neutral " ++ show n ++ " not mapped to NeutralCard, found " ++ show w ++ " instead."

liftCard :: SharedModel -> Card Core -> Maybe (Card UI)
liftCard shared = \case
  CreatureCard creature -> CreatureCard <$> liftCreature shared creature
  NeutralCard n -> Just $ NeutralCard $ liftNeutralObject shared n
  ItemCard i -> Just $ ItemCard $ liftItemObject shared i

liftItemObject :: SharedModel -> ItemObject Core -> ItemObject UI
liftItemObject shared ItemObject {item} = identToItem shared item

liftNeutralObject :: SharedModel -> NeutralObject Core -> NeutralObject UI
liftNeutralObject shared NeutralObject {neutral} =
  identToNeutral shared neutral

-- | Translates a 'Core' 'Creature' into an 'UI' one, keeping its stats
-- An alternative implementation could return the pristine, formal, UI card.
liftCreature :: SharedModel -> Creature 'Core -> Maybe (Creature UI)
liftCreature s@SharedModel {sharedCards} c@Creature {..} =
  case sharedCards Map.!? IDC creatureId items of
    Nothing -> Nothing
    Just (CreatureCard Creature {tile}) ->
      Just $
        Creature
          { items = map (liftItemObject s . mkCoreItemObject) items,
            skills = map Card.liftSkill skills,
            ..
          }
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

-- | Pick one element at random from the second argument, using the random
-- generator of 'SharedModel'. Returns the updated 'SharedModel' and the
-- picked element (being 'Just' if the list non-empty)
pick :: SharedModel -> [a] -> (SharedModel, Maybe a)
pick shared@SharedModel {sharedStdGen = stdgen} l =
  case length l of
    0 -> (shared, Nothing)
    len ->
      let (i, stdgen') = randomR (0, len - 1) stdgen
       in (shared {sharedStdGen = stdgen'}, Just $ l !! i)

-- | Shuffles the second argument with the random generator
-- of 'SharedModel'. Returns the shuffle and the updated 'SharedModel'
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
