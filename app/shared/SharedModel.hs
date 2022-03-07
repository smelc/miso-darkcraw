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
    Model (cmd),
    tileToFilepath,
    unsafeGet,
    unsafeIdentToCard,
    liftSkill,
    create,
    SharedModel.getStdGen,
    getCards,
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
    getInitialDeck,
  )
where

import Card hiding (ID)
import qualified Card
import Command
import Control.Monad.Random hiding (lift)
import Data.Foldable (find)
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe
import GHC.Base (assert)
import GHC.Generics (Generic)
import Json (loadJson)
import Skill (Skill)
import qualified Skill
import Tile (Filepath, Tile, TileUI (..))
import qualified Tile

instance Eq StdGen where
  std1 == std2 = show std1 == show std2

-- | Instance to be able to use 'SharedModel' like a random generator.
instance RandomGen Model where
  next sh@Model {stdGen} =
    let (i, gen') = next stdGen
     in (i, sh {stdGen = gen'})
  genRange = genRange . stdGen
  split sh@Model {stdGen} =
    let (gen1, gen2) = split stdGen
     in (sh {stdGen = gen1}, sh {stdGen = gen2})

-- | The part of the model that is likely to be used by all pages
-- i.e. all possible models /!\ If you add a field, extend the 'EQ' instance below. /!\
-- Note that we MUST have an 'EQ' instance, because ultimately, our top-level
-- model should have one. It is required by miso in the @startApp@ function.
-- It makes sense, since miso does diffs on models.
data Model = Model
  { -- | Data obtained at load time, that never changes
    cards :: Map Card.ID (Card 'UI),
    -- | The current debug command (in dev mode only)
    cmd :: Maybe String,
    skills :: Map Skill Skill.Pack,
    tiles :: Map Tile TileUI,
    -- | RNG obtained at load time, to be user whenever randomness is needed
    stdGen :: StdGen
  }
  deriving (Generic, Show)

instance Eq Model where
  (==) Model {cmd = cmd1, stdGen = gen1} Model {cmd = cmd2, stdGen = gen2} =
    -- Not comparing the other fields, because they should all the time be
    -- the same, since they are loaded from static Json.
    cmd1 == cmd2 && gen1 == gen2

create :: [Card 'UI] -> [Skill.Pack] -> [TileUI] -> StdGen -> Model
create c sks tls stdGen =
  Model {..}
  where
    groupBy f l = map (\e -> (f e, e)) l & Map.fromList
    cards = groupBy cardToIdentifier c
    skills = groupBy Skill.skill sks
    tiles = groupBy Tile.tile tls
    cmd = Nothing

-- | An instance of 'Model' obtained by reading the Json data
createWithSeed :: Int -> Model
createWithSeed seed =
  case loadJson of
    Left err -> error err
    Right (cards, skills, tiles) -> create cards skills tiles $ mkStdGen seed

cardToFilepath :: Model -> Card 'UI -> Filepath
cardToFilepath shared = \case
  CreatureCard (CardCommon {tile}) _ -> go tile Tile.TwentyFour
  NeutralCard (CardCommon {tile}) _ -> go tile Tile.Sixteen
  ItemCard (CardCommon {tile}) _ -> go tile Tile.Sixteen
  where
    go = tileToFilepath shared

-- | Maps a creature to the filepath of its tile.
creatureToFilepath :: Model -> Creature 'UI -> Maybe Filepath
creatureToFilepath shared Creature {creatureId} =
  cardToFilepath shared <$> ui
  where
    ui = identToCard shared (IDC creatureId [])

allCommands :: Model -> [Command]
allCommands shared = Command.allCommands cids
  where
    cids =
      getCardIdentifiers shared
        & map (\case IDC cid _ -> Just cid; _ -> Nothing)
        & catMaybes

getCardIdentifiers :: Model -> [Card.ID]
getCardIdentifiers = Map.keys . cards

getCards :: Model -> [Card 'UI]
getCards = Map.elems . cards

-- | The starting deck of the given team, not shuffled
getInitialDeck :: Model -> Team -> [Card 'Core]
getInitialDeck shared team = Card.teamDeck (getCards shared) team

getStdGen :: Model -> StdGen
getStdGen Model {stdGen} = stdGen

withCmd :: Model -> Maybe String -> Model
withCmd shared c = shared {cmd = c}

withSeed :: Model -> Int -> Model
withSeed shared seed = shared {stdGen = mkStdGen seed}

withStdGen :: Model -> StdGen -> Model
withStdGen shared stdgen = shared {stdGen = stdgen}

-- | An instance of 'SharedModel' that is fine for debugging. Don't use
-- it in production!
unsafeGet :: Model
unsafeGet = createWithSeed 42

identToCard :: Model -> Card.ID -> Maybe (Card 'UI)
identToCard s@Model {cards} (IDC cid items) =
  case cards Map.!? IDC cid [] of
    Nothing -> Nothing
    Just (CreatureCard cc c@Creature {items = is}) ->
      -- and then we fill the result with the expected items:
      assert (null is) $
        let itemsUI = map (lift s . mkCoreItemObject) items
         in Just $ CreatureCard cc (c {items = itemsUI})
    Just _ -> error "Unexpected card. Expected CreatureCard."
identToCard Model {cards} id = cards Map.!? id

identToItem :: Model -> Item -> ItemObject 'UI
identToItem Model {cards} i =
  case cards Map.!? IDI i of
    -- This should not happen, see 'testShared'
    Nothing -> error $ "Unmapped item: " ++ show i
    Just (ItemCard _ res) -> res
    -- To avoid this case, I could split the cards in SharedModel
    Just w -> error $ "Item " ++ show i ++ " not mapped to ItemCard, found " ++ show w ++ " instead."

idToCreature :: Model -> CreatureID -> [Item] -> Maybe (Creature 'UI)
idToCreature shared cid items =
  case identToCard shared $ IDC cid items of
    Nothing -> Nothing
    Just (CreatureCard _ c) -> Just c
    Just _ -> error "Unexpected card. Expected CreatureCard."

identToNeutral :: Model -> Neutral -> NeutralObject 'UI
identToNeutral Model {cards} n =
  case cards Map.!? IDN n of
    Nothing -> error $ "Unmapped neutral: " ++ show n
    Just (NeutralCard _ res) -> res
    -- To avoid this case, I could split the cards in SharedModel
    Just w -> error $ "Neutral " ++ show n ++ " not mapped to NeutralCard, found " ++ show w ++ " instead."

toCardCommon :: Model -> Card.ID -> Maybe (CardCommon 'UI)
toCardCommon shared id =
  identToCard shared id <&> dispatch
  where
    dispatch =
      \case
        CreatureCard common _ -> common
        NeutralCard common _ -> common
        ItemCard common _ -> common

unsafeToCardCommon :: Model -> Card.ID -> CardCommon 'UI
unsafeToCardCommon shared id =
  case toCardCommon shared id of
    Nothing -> error $ "unsafeToCardCommon: Card.ID not found: " ++ show id
    Just res -> res

class Mlift p where
  mlift :: Model -> p 'Core -> Maybe (p 'UI)

instance Mlift Card where
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
  lift :: Model -> p 'Core -> p 'UI

instance Lift ItemObject where
  lift shared ItemObject {item} = identToItem shared item

instance Lift NeutralObject where
  lift shared NeutralObject {neutral} =
    identToNeutral shared neutral

-- | Translates a 'Core' 'Creature' into an 'UI' one, keeping its stats
-- An alternative implementation could return the pristine, formal, UI card.
instance Mlift Creature where
  mlift :: Model -> Creature 'Core -> Maybe (Creature 'UI)
  mlift s@Model {cards} c@Creature {..} =
    -- Because creatures in data.json don't have items, we send []:
    case cards Map.!? IDC creatureId [] of
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

liftSkill :: Model -> Skill -> Skill.Pack
liftSkill Model {skills} skill =
  fromMaybe default_ (find (\Skill.Pack {skill = sk} -> sk == skill) skills)
  where
    default_ = Skill.Pack {skill = Skill.Support, ..}
    -- Dummy value
    text = show skill ++ " not found!"
    title = text

tileToFilepath :: Model -> Tile -> Tile.Size -> Filepath
tileToFilepath Model {tiles} tile defaultSize =
  case (find (\TileUI {tile = t} -> t == tile) tiles, defaultSize) of
    (Nothing, Tile.Sixteen) -> Tile.default16Filepath
    (Nothing, Tile.TwentyFour) -> Tile.default24Filepath
    (Just TileUI {Tile.filepath}, _) -> filepath

unsafeIdentToCard :: Model -> Card.ID -> Card 'UI
unsafeIdentToCard smodel ci = identToCard smodel ci & fromJust
