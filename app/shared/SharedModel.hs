{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- |
-- This module contains the subset of the model that is needed by 'Game'
-- I didn't want to make 'Game' depend on 'Model', because it seemed overkill
-- and dangerous cyclewise in the long run. Hence I introduced this module.
-- |
module SharedModel
  ( creatureToFilepath,
    identToCard,
    idToCreature,
    Lift (..),
    Mlift (..),
    SharedModel,
    tileToFilepath,
    unsafeGet,
    unsafeIdentToCard,
    SharedModel.liftSkill,
    shuffle,
    SharedModel.shuffleM,
    create,
    SharedModel.getStdGen,
    getCards,
    getCmd,
    roll,
    withCmd,
    withStdGen,
    getCardIdentifiers,
    cardToFilepath,
    withSeed,
    SharedModel.allCommands,
    toCardCommon,
    unsafeToCardCommon,
    identToItem,
    identToNeutral,
    pick,
    getInitialDeck,
    oneof,
  )
where

import Card hiding (ID)
import qualified Card
import Command
import Control.Monad.Identity (Identity (runIdentity))
import Control.Monad.Random hiding (lift)
import Control.Monad.State hiding (lift)
import Data.Foldable (find)
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe
import GHC.Base (assert)
import GHC.Generics (Generic)
import Json (loadJson)
import Skill (Skill)
import qualified Skill
import System.Random.Shuffle (shuffleM)
import Tile (Filepath, Tile, TileUI (..))
import qualified Tile

-- | The part of the model that is likely to be used by all pages
-- | i.e. all possible models
data SharedModel = SharedModel
  { -- | Data obtained at load time, that never changes
    sharedCards :: Map Card.ID (Card 'UI),
    -- | The current debug command (in dev mode only)
    sharedCmd :: Maybe String,
    sharedSkills :: Map Skill Skill.Pack,
    sharedTiles :: Map Tile TileUI,
    -- | RNG obtained at load time, to be user whenever randomness is needed
    sharedStdGen :: StdGen
  }
  deriving (Eq, Generic, Show)

create :: [Card 'UI] -> [Skill.Pack] -> [TileUI] -> StdGen -> SharedModel
create cards skills tiles sharedStdGen =
  SharedModel {..}
  where
    groupBy f l = map (\e -> (f e, e)) l & Map.fromList
    sharedCards = groupBy cardToIdentifier cards
    sharedSkills = groupBy Skill.skill skills
    sharedTiles = groupBy Tile.tile tiles
    sharedCmd = Nothing

-- | An instance of 'SharedModel' obtained by reading the Json data
createWithSeed :: Int -> SharedModel
createWithSeed seed =
  case loadJson of
    Left err -> error err
    Right (cards, skills, tiles) -> create cards skills tiles $ mkStdGen seed

cardToFilepath :: SharedModel -> Card 'UI -> Filepath
cardToFilepath shared = \case
  CreatureCard (CardCommon {tile}) _ -> go tile Tile.TwentyFour
  NeutralCard (CardCommon {tile}) _ -> go tile Tile.Sixteen
  ItemCard (CardCommon {tile}) _ -> go tile Tile.Sixteen
  where
    go = tileToFilepath shared

-- | Maps a creature to the filepath of its tile.
creatureToFilepath :: SharedModel -> Creature 'UI -> Maybe Filepath
creatureToFilepath shared Creature {creatureId} =
  cardToFilepath shared <$> ui
  where
    ui = identToCard shared (IDC creatureId [])

allCommands :: SharedModel -> [Command]
allCommands shared = Command.allCommands cids
  where
    cids =
      getCardIdentifiers shared
        & map (\case IDC cid _ -> Just cid; _ -> Nothing)
        & catMaybes

getCardIdentifiers :: SharedModel -> [Card.ID]
getCardIdentifiers SharedModel {sharedCards} = Map.keys sharedCards

getCards :: SharedModel -> [Card 'UI]
getCards SharedModel {sharedCards} = Map.elems sharedCards

getCmd :: SharedModel -> Maybe String
getCmd SharedModel {sharedCmd} = sharedCmd

-- | The starting deck of the given team, not shuffled
getInitialDeck :: SharedModel -> Team -> [Card 'Core]
getInitialDeck shared team = Card.teamDeck (getCards shared) team

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

identToCard :: SharedModel -> Card.ID -> Maybe (Card 'UI)
identToCard s@SharedModel {sharedCards} (IDC cid items) =
  case sharedCards Map.!? IDC cid [] of
    Nothing -> Nothing
    Just (CreatureCard cc c@Creature {items = is}) ->
      -- and then we fill the result with the expected items:
      assert (null is) $
        let itemsUI = map (lift s . mkCoreItemObject) items
         in Just $ CreatureCard cc (c {items = itemsUI})
    Just _ -> error "Unexpected card. Expected CreatureCard."
identToCard SharedModel {sharedCards} id = sharedCards Map.!? id

identToItem :: SharedModel -> Item -> ItemObject 'UI
identToItem SharedModel {sharedCards} i =
  case sharedCards Map.!? IDI i of
    -- This should not happen, see 'testShared'
    Nothing -> error $ "Unmapped item: " ++ show i
    Just (ItemCard _ res) -> res
    -- To avoid this case, I could split the cards in SharedModel
    Just w -> error $ "Item " ++ show i ++ " not mapped to ItemCard, found " ++ show w ++ " instead."

idToCreature :: SharedModel -> CreatureID -> [Item] -> Maybe (Creature 'UI)
idToCreature shared cid items =
  case identToCard shared $ IDC cid items of
    Nothing -> Nothing
    Just (CreatureCard _ c) -> Just c
    Just _ -> error "Unexpected card. Expected CreatureCard."

identToNeutral :: SharedModel -> Neutral -> NeutralObject 'UI
identToNeutral SharedModel {sharedCards} n =
  case sharedCards Map.!? IDN n of
    Nothing -> error $ "Unmapped neutral: " ++ show n
    Just (NeutralCard _ res) -> res
    -- To avoid this case, I could split the cards in SharedModel
    Just w -> error $ "Neutral " ++ show n ++ " not mapped to NeutralCard, found " ++ show w ++ " instead."

toCardCommon :: SharedModel -> Card.ID -> Maybe (CardCommon 'UI)
toCardCommon shared id =
  identToCard shared id <&> dispatch
  where
    dispatch =
      \case
        CreatureCard common _ -> common
        NeutralCard common _ -> common
        ItemCard common _ -> common

unsafeToCardCommon :: SharedModel -> Card.ID -> CardCommon 'UI
unsafeToCardCommon shared id =
  case toCardCommon shared id of
    Nothing -> error $ "unsafeToCardCommon: Card.ID not found: " ++ show id
    Just res -> res

class Mlift p where
  mlift :: SharedModel -> p 'Core -> Maybe (p 'UI)

instance Mlift Card where
  mlift :: SharedModel -> Card 'Core -> Maybe (Card 'UI)
  mlift shared card =
    case (common, card) of
      (Nothing, _) -> Nothing
      (Just common, CreatureCard _ creature) -> CreatureCard common <$> mlift shared creature
      (Just common, NeutralCard _ n) -> go NeutralCard common n
      (Just common, ItemCard _ i) -> go ItemCard common i
    where
      common = toCardCommon shared $ Card.cardToIdentifier card
      go constructor common liftable = pure $ constructor common $ lift shared liftable

class Lift p where
  lift :: SharedModel -> p 'Core -> p 'UI

instance Lift ItemObject where
  lift shared ItemObject {item} = identToItem shared item

instance Lift NeutralObject where
  lift shared NeutralObject {neutral} =
    identToNeutral shared neutral

-- | Translates a 'Core' 'Creature' into an 'UI' one, keeping its stats
-- An alternative implementation could return the pristine, formal, UI card.
instance Mlift Creature where
  mlift :: SharedModel -> Creature 'Core -> Maybe (Creature 'UI)
  mlift s@SharedModel {sharedCards} c@Creature {..} =
    -- Because creatures in data.json don't have items, we send []:
    case sharedCards Map.!? IDC creatureId [] of
      Nothing -> Nothing
      Just (CreatureCard _ (Creature {items = is})) ->
        -- and then we fill the result with the expected items:
        assert (null is) $
          Just $
            Creature
              { items = map (lift s . mkCoreItemObject) items,
                skills = map Skill.lift skills,
                transient = (),
                ..
              }
      Just card -> error $ "Creature " ++ show c ++ " mapped in UI to: " ++ show card

liftSkill :: SharedModel -> Skill -> Skill.Pack
liftSkill SharedModel {sharedSkills} skill =
  fromMaybe
    default_
    $ find (\Skill.Pack {skill = sk} -> sk == skill) sharedSkills
  where
    default_ = Skill.Pack {skill = Skill.Support, ..}
    -- Dummy value
    text = show skill ++ " not found!"
    title = text

-- | Pick one element at random from the second argument, using the random
-- generator of 'SharedModel'. Returns the updated 'SharedModel' and the
-- picked element (being 'Just' if the list non-empty). See 'oneof' for
-- a variant of this function.
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
    (l', stdgen') =
      System.Random.Shuffle.shuffleM l
        & flip runRandT sharedStdGen
        & runIdentity

-- | Shuffles the given list with the random generator of 'SharedModel'
shuffleM :: MonadState SharedModel m => [a] -> m [a]
shuffleM =
  \case
    [] -> pure []
    l -> do
      shared <- get
      let (shared', l') = shuffle shared l
      put shared'
      return l'

-- | Returns a random element from the list, using 'sharedStdGen' as
-- the random generator.
oneof ::
  MonadState SharedModel m =>
  -- | The elements from which something must be picked
  NonEmpty a ->
  m a
oneof elems = do
  shared@SharedModel {sharedStdGen = stdgen} <- get
  let (idx, stdgen') = randomR (0, nbElems - 1) stdgen
  put (shared {sharedStdGen = stdgen'})
  return $ elems NE.!! idx
  where
    nbElems :: Int = NE.length elems

-- XXX @smelc make SharedModel an instance of StdGen, use that in this
-- function and other neighbors functions too.

-- | 'roll min max' returns a value between 'min' (included) and 'max'
-- (included). 'min <= max' should hold.
roll ::
  MonadState SharedModel m =>
  Random p =>
  Ord p =>
  -- | The minimum element
  p ->
  -- | The maximum element
  p ->
  m p
roll min max = do
  shared@SharedModel {sharedStdGen = stdgen} <- get
  let (res, stdgen') = assert (min <= max) $ randomR (min, max) stdgen
  put (shared {sharedStdGen = stdgen'})
  return res

tileToFilepath :: SharedModel -> Tile -> Tile.Size -> Filepath
tileToFilepath SharedModel {sharedTiles} tile defaultSize =
  case (find (\TileUI {tile = t} -> t == tile) sharedTiles, defaultSize) of
    (Nothing, Tile.Sixteen) -> Tile.default16Filepath
    (Nothing, Tile.TwentyFour) -> Tile.default24Filepath
    (Just TileUI {Tile.filepath}, _) -> filepath

unsafeIdentToCard :: SharedModel -> Card.ID -> Card 'UI
unsafeIdentToCard smodel ci = identToCard smodel ci & fromJust
