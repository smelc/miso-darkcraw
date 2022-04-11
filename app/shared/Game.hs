{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Game
  ( allEnemySpots,
    Animation (..),
    Game.appliesTo,
    applyFearNTerror,
    applyFillTheFrontline,
    applyPlague,
    attackOrder, -- exported for tests only
    cardsToDraw,
    drawCard,
    drawCards,
    enemySpots,
    idToHandIndex,
    DrawSource (..),
    eventToAnim,
    EnemySpots (..),
    Event (..),
    keepEffectfull,
    maybePlay,
    meetsRequirement,
    MessageText (..),
    mkPlayable,
    nextAttackSpot,
    Place (..),
    Playable (..),
    Result (..),
    play,
    playE,
    playAll,
    playAllE,
    toSpot,
    toSpots,
    tryPlayM,
    StatChange (..), -- exported for tests only
    transferCards,
    Target (..),
    whichPlayerTarget,
    WhichPlayerTarget (..),
  )
where

import qualified Board
import BoardInstances
import Card hiding (ID)
import qualified Card
import qualified Constants
import Contains (with)
import qualified Contains
import Control.Exception (assert)
import Control.Monad.Except
import Control.Monad.Identity (runIdentity)
import Control.Monad.Random
import Control.Monad.State
import Control.Monad.Writer
import Damage (Damage (..), (+^))
import qualified Damage
import qualified Data.Bifunctor as Bifunctor
import Data.Either.Extra
import Data.Foldable
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.List
import Data.List.Index (deleteAt, setAt)
import qualified Data.List.NonEmpty as NE
import Data.Map.Strict (Map, (!?))
import qualified Data.Map.Strict as Map
import Data.Maybe
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Tuple (swap)
import Debug.Trace
import GHC.Generics (Generic)
import qualified Mana
import qualified Mechanics
import Nat
import qualified Random
import qualified Shared
import Skill (Skill)
import qualified Skill
import Spots (Card (..), Player (..))
import qualified Spots
import System.Random.Shuffle (shuffleM)
import qualified Tile
import qualified Total
import qualified Turn

-- | On what a card can be applied
data Target
  = -- | Neutral card applies to all in place cards of a player
    PlayerTarget Spots.Player
  | -- | Creature card placed at given spot
    -- or Neutral card applies to a given in place card of a player
    CardTarget Spots.Player Spots.Card
  deriving (Eq, Generic, Show)

-- | The input type to most @play*@ functions
data Playable e = Playable
  { board :: Board.T 'Core,
    event :: e,
    turn :: Turn.T
  }

-- | To allow callers to hide the implementation of 'Playable', to avoid
-- fields names conflicts.
mkPlayable :: Board.T 'Core -> e -> Turn.T -> Playable e
mkPlayable board event turn = Playable {..}

instance Contains.Contains (Playable a) (Board.T 'Core) where
  to = board

instance Contains.With (Playable a) (Board.T 'Core) where
  with p b = p {board = b}

instance Contains.Contains (Playable a) a where
  to = event

instance Contains.With (Playable a) a where
  with p e = p {event = e}

instance
  (Contains.Contains (Playable a) (Board.T 'Core), Contains.Contains (Playable a) a) =>
  Contains.Contains (Playable a) (Board.T 'Core, a)
  where
  to p = (Contains.to p, Contains.to p)

instance
  (Contains.With (Playable a) (Board.T 'Core), Contains.With (Playable a) a) =>
  Contains.With (Playable a) (Board.T 'Core, a)
  where
  with p (b, e) = p {board = b, event = e}

targetToPlayerSpot :: Target -> Spots.Player
targetToPlayerSpot (PlayerTarget pSpot) = pSpot
targetToPlayerSpot (CardTarget pSpot _) = pSpot

-- | Whether a card makes sense on the playing player
-- or the  opponent. We could even try both, but we don't do that
-- for now
data WhichPlayerTarget = Playing | Opponent

-- | To which part to apply a card. Beneficial card apply to 'Playing'
-- while cards giving maluses, curses, etc. apply to 'Opponent'.
whichPlayerTarget :: Card.ID -> WhichPlayerTarget
whichPlayerTarget = \case
  IDC {} -> Playing
  IDI _ -> Playing
  IDN Health -> Playing
  IDN HuntingHorn -> Playing
  IDN InfernalHaste -> Playing
  IDN Life -> Playing
  IDN Pandemonium -> Opponent
  IDN Plague -> Opponent
  IDN StrengthPot -> Playing

-- | Placing events. These events are the ones that the 'AI' generates (as
-- opposed to the more general 'Event' type).
data Place
  = -- | Player puts a card from his hand on its part of the board. First
    -- argument is the player, second argument is the target, third argument
    -- is the card being played.
    Place Spots.Player Target Board.HandIndex
  | -- | AI puts a card from his hand. This constructor has better
    -- testing behavior than 'Place': it makes the generated events commute.
    Place' Spots.Player Target Card.ID
  deriving (Eq, Generic, Show)

-- | The 'Spots.Player' in a 'Place', i.e. the player playing an event
toSpot :: Place -> Spots.Player
toSpot = \case Place pSpot _ _ -> pSpot; Place' pSpot _ _ -> pSpot

data Event
  = -- | Apply assassins of the creatures at the given 'Spots.Player'
    ApplyAssassins Spots.Player
  | -- | Apply brainless of the creatures at the given 'Spots.Player'
    ApplyBrainless Spots.Player
  | -- | Apply church of the creatures at the given 'Spots.Player'
    ApplyChurch Spots.Player
  | -- | Apply the create forest spell of the creatures at the given 'Spots.Player'
    ApplyCreateForest Spots.Player
  | -- | Apply fear caused by the creatures at the given 'Spots.Player'
    ApplyFearNTerror Spots.Player
  | -- | Apply growth of the creatures at the given 'Spots.Player'
    ApplyGrowth Spots.Player
  | -- | Apply king of the creatures at the given 'Spots.Player'
    ApplyKing Spots.Player
  | -- | A card attacks at the given spot. The first Boolean indicates
    -- whether the next spot (as defined by 'nextAttackSpot') should
    -- be enqueued after solving this attack. The second Boolean indicates
    -- whether 'GameIncrTurn' (change player turn) should be performed
    -- after solving this stream of attacks.
    Attack Spots.Player Spots.Card Bool Bool
  | -- | Ranged creatures with the 'Ranged' skill, that: 1/ are in the
    -- back line and 2/ have no creature in from of them; move frontward
    FillTheFrontline Spots.Player
  | -- | A Nothing case, for convenience
    NoPlayEvent
  | -- | A 'Place' event
    PEvent Place
  deriving (Eq, Generic, Show)

-- | The result of playing game events. If you add a field, extend
-- the 'Eq' instance below. TODO @smelc find a better name.
data Result e = Result
  { board :: Board.T 'Core,
    anims :: Board.T 'UI,
    event :: e,
    shared :: Shared.Model
  }
  deriving (Show)

instance Eq a => Eq (Result a) where
  (==)
    Result {board = b1, anims = a1, event = e1, shared = s1}
    Result {board = b2, anims = a2, event = e2, shared = s2} =
      b1 == b2 && a1 == a2 && e1 == e2 && s1 == s2

toResult :: Shared.Model -> Board.T 'Core -> Result (Maybe a)
toResult s b = Result {board = b, anims = mempty, event = Nothing, shared = s}

data MessageText
  = -- | Simple text to display
    Text Text.Text
  | -- | Constructor to display an image. The image should be fine
    -- for passing to 'assetsPath'
    Image Tile.Filepath
  deriving (Eq, Generic)

-- | An animation that makes sense at the 'Game' level. If you
-- consider extending this variant, consider whether it would be
-- be better in @Board.T 'UI@
data Animation
  = -- | Player plays the given card played on a target. This is used for example
    -- to display 'Plague' when played or when adding an item to a creature.
    Application Spots.Player Target (Card.Card 'Core)
  | -- No animation
    NoAnimation
  | -- | Game view should fadeout
    Fadeout
  | -- | Message to show centered. The 'Nat' is the duration during
    -- which to show the message. Then it fades out during one second.
    Message [MessageText] Nat
  deriving (Eq, Generic)

-- | When this type is returned, the function name must start with 'try'
type Possible a = Either Impossible a

-- | An alias for building 'Possible' values
impossible :: Impossible -> Possible b
impossible x = Left x

-- | Values denoting that some request (such as a 'Game.Request')
-- cannot be performed.
data Impossible
  = CannotDraw
  | CannotPlaceCreature
  | CannotPlaceItem
  | CardNotFound
  | NotEnoughMana
  deriving (Show)

-- | An error which covers both the case where it must be sent to the
-- caller unaltered ('Critical') and other cases.
-- It is best to avoid this type altogether if possible, that is why
-- it is named 'Internal*' and not exposed.
data InternalError
  = -- | @Critical t@ denotes a hard bug. Something that should never happen.
    -- It gets displayed in a div.
    Critical Text
  | -- | @CannotDo@ denotes an event that cannot be applied. This
    -- can happen because events are computed in a context and applied in another,
    -- and randomness happens in between (flail of the damned, pandemonium, etc.)
    CannotDo Impossible
  deriving (Show)

-- | A change in stats of a creature. Using 'Nat' for now because it suffices
-- for the use cases. But really it could be a 'Int', it just makes 'apply' harder.
data StatChange = StatChange {attackDiff :: Nat, hpDiff :: Nat}
  deriving (Eq, Generic, Show)

instance Semigroup StatChange where
  StatChange {attackDiff = a1, hpDiff = hp1}
    <> StatChange {attackDiff = a2, hpDiff = hp2} =
      StatChange {attackDiff = a1 + a2, hpDiff = hp1 + hp2}

instance Monoid StatChange where
  mempty = StatChange {attackDiff = 0, hpDiff = 0}

changeToEffect :: StatChange -> Board.InPlaceEffect
changeToEffect StatChange {attackDiff, hpDiff} =
  mempty {Board.attackChange = natToInt attackDiff, Board.hitPointsChange = natToInt hpDiff}

apply :: StatChange -> Creature p -> Creature p
apply StatChange {attackDiff, hpDiff} c@Creature {attack, hp} =
  c {Card.attack = attack +^ attackDiff, hp = hp + hpDiff}

reportEffect ::
  MonadWriter (Board.T 'UI) m =>
  Spots.Player ->
  Spots.Card ->
  Board.InPlaceEffect ->
  m ()
reportEffect pSpot cSpot effect =
  tell $ Board.T {playerTop = pTop, playerBottom = pBot}
  where
    effectfull = Map.singleton cSpot effect
    effectless = mempty
    (botInPlace, topInPlace) =
      case pSpot of
        PlayerBot -> (effectfull, effectless)
        PlayerTop -> (effectless, effectfull)
    pTop :: Board.PlayerPart 'UI = mempty {Board.inPlace = topInPlace}
    pBot :: Board.PlayerPart 'UI = mempty {Board.inPlace = botInPlace}

-- | Play a single 'Event' on the given 'Board'. Monad version is 'playM' below.
-- If the event cannot be played, the input board is returned as is. Use
-- 'tryPlayM' for the version distinguishing the error and success cases.
play :: Shared.Model -> Playable Event -> Either Text (Result (Maybe Event))
play shared p@Playable {board} =
  tryPlayM p
    & runWriterT
    & flip runStateT shared
    & runExcept
    & fmap f
  where
    f ::
      ((Possible (Board.T 'Core, Maybe Event), Board.T 'UI), Shared.Model) ->
      Result (Maybe Event)
    f ((Left _, _), _) = toResult shared board -- Impossible case
    f (((Right (b, e), bui), s)) = Result {anims = bui, board = b, event = e, shared = s}

-- | A variant of 'play' and 'playE' that is only in the error monad. This
-- function returns the input board if the event is unapplicable. That is
-- why this function doesn't have 'Impossible' in its return type.
playE ::
  MonadError Text m =>
  Shared.Model ->
  Playable Event ->
  m (Shared.Model, Board.T 'Core, Board.T 'UI, Maybe Event)
playE shared p@Playable {board} =
  tryPlayM p
    & runWriterT
    & flip runStateT shared
    & fmap f
  where
    f ::
      ((Possible (Board.T 'Core, Maybe Event), Board.T 'UI), Shared.Model) ->
      (Shared.Model, Board.T 'Core, Board.T 'UI, Maybe Event)
    f (((Left _), _), _) = (shared, board, mempty, Nothing)
    f (((Right (b, e), bui), s)) = (s, b, bui, e)

-- | Try to play an event. If the event cannot be played or a hard error
-- occurs (@MonadError Text _@ in other functions of this API), simply return 'Nothing'.
maybePlay ::
  Shared.Model ->
  Playable Event ->
  Maybe (Shared.Model, Board.T 'Core, Maybe Event)
maybePlay shared p =
  tryPlayM p
    & runWriterT
    & flip runStateT shared
    & runExcept
    & f
  where
    f ::
      Either Text ((Possible (Board.T 'Core, Maybe Event), Board.T 'UI), Shared.Model) ->
      Maybe (Shared.Model, Board.T 'Core, Maybe Event)
    f =
      \case
        Left _ -> Nothing
        Right ((Left _, _bui), _s) -> Nothing
        Right ((Right (b, e), _bui), s) -> Just (s, b, e)

-- | Play a list of events, playing newly produced events as they are being
-- produced. That is why, contrary to 'play', this function doesn't return
-- events: it consumes them all eagerly. See 'playAllM' for the monad version
playAll :: Shared.Model -> Playable [Event] -> Either Text (Result ())
playAll shared p =
  playAllM p
    & runWriterT
    & flip runStateT shared
    & runExcept
    & fmap f
  where
    f :: ((Board.T 'Core, Board.T 'UI), Shared.Model) -> Result ()
    f ((b, anims), sh) = Result {anims, board = b, event = (), shared = sh}

-- | Like 'playAll', but in the error monad. This function skips unapplicable
-- events. That is why its return type doesn't have 'Impossible'.
playAllE ::
  MonadError Text m =>
  Shared.Model ->
  Playable [Event] ->
  m (Shared.Model, Board.T 'Core, Board.T 'UI)
playAllE shared p =
  playAllM p
    & runWriterT
    & flip runStateT shared
    & fmap (\((b, anims), sh) -> (sh, b, anims))

-- | Like 'playAll', but in the monad. This function skips unapplicable
-- events. That is why its return type doesn't have 'Impossible'.
playAllM ::
  MonadError Text m =>
  MonadWriter (Board.T 'UI) m =>
  MonadState Shared.Model m =>
  Playable [Event] ->
  m (Board.T 'Core)
playAllM p@Playable {board, event} =
  case event of
    [] -> return board
    event : rest -> do
      e <- tryPlayM p {event = event}
      let (board', generated) = eitherToMaybe e & fromMaybe (board, Nothing)
      -- /!\ We're enqueueing the event created by playing 'event' i.e. 'generated',
      -- before the rest of the events ('rest'). This means 'rest' will be played in a state
      -- that is NOT the one planned when building 'event : rest' (except if the
      -- caller is very smart). We have no way around that: this is caused
      -- by Infernal Haste that creates events when playing it. But this means
      -- we shouldn't be too hard with events that yield failures. Maybe
      -- they are being played in an unexpected context (imagine if
      -- Infernal Haste clears the opponent's board, next spells may be useless).
      playAllM $ p `with` (board', maybeToList generated <> rest)

-- | 'keepEffectfull board es' returns the elements of 'es'
-- that have an effect. Elements of 'es' are played in sequence.
keepEffectfull :: Shared.Model -> Playable [Event] -> [Event]
keepEffectfull shared p@Playable {board, event} =
  case event of
    [] -> []
    (e : es) ->
      case play shared p {event = e} of
        Left err -> traceShow err (keepEffectfull shared (p `with` es))
        Right (Result {shared = shared', board = board', event = new}) ->
          case playAll shared' (p `with` (board', maybeToList new)) of
            Left err -> traceShow err (keepEffectfull shared' (p `with` (board', es)))
            Right (Result {shared = shared'', board = board''}) ->
              if board'' == board
                then {- 'e' had no effect -} keepEffectfull shared (p `with` es)
                else {- 'e' had an effect -} e : keepEffectfull shared'' (p `with` (board'', es))

tryPlayM ::
  MonadError Text m =>
  MonadWriter (Board.T 'UI) m =>
  MonadState Shared.Model m =>
  Playable Event ->
  m (Possible (Board.T 'Core, Maybe Event))
tryPlayM p@Playable {board, turn, event} =
  case event of
    ApplyAssassins pSpot -> do
      board' <- Game.applyAssassinsM board pSpot
      return $ pure $ (board', Nothing)
    ApplyBrainless pSpot -> do
      board' <- Game.applyBrainlessM board pSpot
      return $ pure $ (board', Nothing)
    ApplyChurch pSpot -> do
      board' <- Game.applyChurchM board pSpot
      return $ pure $ (board', Nothing)
    ApplyCreateForest pSpot -> do
      board' <- Game.applyCreateForestM board pSpot
      return $ pure $ (board', Nothing)
    ApplyFearNTerror pSpot -> do
      board' <- Game.applyFearNTerrorM board pSpot
      return $ pure $ (board', Nothing)
    ApplyGrowth pSpot -> do
      board' <- foldM (\b cSpot -> Game.applyGrowthM b pSpot cSpot) board (Spots.allCards)
      return $ pure $ (board', Nothing)
    ApplyKing pSpot -> do
      board' <- Game.applyKingM board pSpot
      return $ pure $ (board', Nothing)
    Attack pSpot cSpot _ _ -> do
      board' <- Game.attack board pSpot cSpot
      return $ pure $ (board', Nothing)
    FillTheFrontline pSpot -> do
      return $ pure $ (applyFillTheFrontline board pSpot, Nothing)
    NoPlayEvent ->
      return $ pure $ (board, Nothing)
    PEvent (Place pSpot target (handhi :: Board.HandIndex)) -> do
      shared <- get
      ident <- Board.lookupHandM hand handi
      let uiCard = Shared.identToCard shared ident
      let card = unlift <$> uiCard
      case (target, card, uiCard <&> Card.toCommon) of
        (_, Nothing, _) ->
          throwError $ Text.pack $ "ident not found: " ++ show ident
        (_, _, Nothing) ->
          throwError $ Text.pack $ "Unexpected state, CardCommon should be there if Card is there"
        (_, _, Just (CardCommon {mana = manaCost}))
          | (Mana.>) turn manaCost manaAvail ->
              -- Not enough mana
              return $ impossible NotEnoughMana
        (CardTarget pSpot cSpot, Just (CreatureCard _ creature), Just CardCommon {mana}) -> do
          e <- playCreatureM shared board' pSpot cSpot creature & runExceptT
          e >=> (\board'' -> (decreaseMana mana board'', Nothing))
        (CardTarget pSpot cSpot, Just (ItemCard _ itemObj), Just CardCommon {mana}) -> do
          e <- playItemM board' pSpot cSpot (Card.item itemObj) & runExceptT
          e >=> (\board'' -> (decreaseMana mana board'', Nothing))
        (_, Just (NeutralCard _ NeutralObject {neutral}), Just CardCommon {mana}) -> do
          (board'', event) <- playNeutralM board' pSpot target neutral
          return $ pure $ (decreaseMana mana board'', event)
        _ ->
          let pair = show target ++ ", " ++ show card
           in throwError $ Text.pack $ "Wrong (Target, card) combination: (" ++ pair ++ ")"
      where
        handi = Board.unHandIndex handhi
        (hand, hand') = (Board.toHand board pSpot, deleteAt handi hand)
        board' = Board.setHand board pSpot hand'
        manaAvail :: Nat = Board.toPart board pSpot & Board.mana
        decreaseMana (manaCost :: Mana.Mana) (b :: Board.T 'Core) =
          Board.setMana (manaAvail - (Mana.amount turn manaCost)) pSpot b
        -- Apply continuation 'f' on 'i' if a success, otherwise return
        (>=>) i f =
          case i of
            Left imp -> return $ impossible imp
            Right b -> return $ pure $ f b
    PEvent (Place' pSpot target id) ->
      case idToHandIndex board pSpot id of
        Nothing -> return $ impossible CardNotFound
        Just i -> tryPlayM $ p `with` (PEvent (Place pSpot target i))

-- | Translates an 'Event' into an animation displayed in the
-- middle of the 'Board'.
eventToAnim ::
  MonadError Text m =>
  Shared.Model ->
  Board.T 'Core ->
  Event ->
  m Animation
eventToAnim shared board =
  -- Note that, in this function, we do not need to check if the
  -- Event has an effect. This is done by the caller of 'keepEffectfull'
  \case
    Game.ApplyAssassins pSpot ->
      pure $ Message [Text msg] duration
      where
        assassins =
          Board.toInPlace board pSpot
            & Map.filter (`has` (Skill.Assassin :: Skill.State))
        msg =
          if Map.size assassins == 1
            then "The assassin closes in on its target"
            else "Assassins close in on their targets"
    Game.ApplyBrainless pSpot ->
      pure $ Message [Text msg] duration
      where
        brainless =
          Board.toInPlace board pSpot
            & Map.filter (`has` (Skill.Brainless :: Skill.State))
        msg =
          if Map.size brainless == 1
            then "Brainless creature moves randomly"
            else "Brainless creatures move randomly"
    Game.ApplyChurch pSpot ->
      case (Board.mana part /= Board.mana part', hps < hps', attack < attack') of
        (True, _, _) -> pure $ fill [Text "+1 mana"]
        (_, True, _) -> pure $ fill [Text "adds +1", Image heartTile]
        (_, _, True) -> pure $ fill [Text "adds +1", Image attackTile]
        _ -> pure $ fill [Text " has no effect without any believers"]
      where
        parts@(part, part') = (Board.toPart board pSpot, Board.toPart board' pSpot)
        board' = applyChurch shared board pSpot & (\(b, _, _) -> b)
        churchTile = Shared.tileToFilepath shared Tile.HumanChurch Tile.TwentyFour
        creatures :: ([Creature 'Core], [Creature 'Core]) =
          both (Map.elems . Board.inPlace) parts
        toHPs cs = sum $ map Card.hp cs
        toAttack cs = mconcat $ map Card.attack cs
        (hps :: Nat, hps' :: Nat) = both toHPs creatures
        (attack :: Damage, attack' :: Damage) = both toAttack creatures
        both f = Bifunctor.bimap f f
        fill suffix = Message ([Text "Church", Image churchTile] ++ suffix) duration
    Game.ApplyCreateForest {} -> pure $ Message [Text "Let be forests 🌲"] duration
    Game.ApplyFearNTerror {} -> pure NoAnimation
    Game.ApplyGrowth {} -> pure $ Message [Text "Unstoppable growth 🌳"] duration
    Game.ApplyKing {} ->
      pure $
        Message
          [ Text "Hail to the king: +1 ",
            Image heartTile,
            Text " +1",
            Image attackTile,
            Text " to all knights"
          ]
          duration
    Game.Attack {} -> pure NoAnimation
    Game.FillTheFrontline pSpot ->
      pure $
        Message
          ( [Text "Shooters! "]
              ++ map Image movedTiles
              ++ [Text " Fill the frontline! ⚔️"]
          )
          duration
      where
        part = Board.toInPlace board pSpot
        board' = applyFillTheFrontline board pSpot
        part' = Board.toInPlace board' pSpot
        movedSpots :: Set.Set Spots.Card =
          Set.difference (Map.keysSet part') (Map.keysSet part)
        movedCreatures :: [Creature 'UI] =
          (map (part' Map.!?) $ Set.toList movedSpots)
            & catMaybes
            & map (Shared.mlift shared)
            & catMaybes
        movedTiles :: [Tile.Filepath] =
          map (Shared.creatureToFilepath shared) movedCreatures
            & catMaybes
    Game.NoPlayEvent -> pure NoAnimation
    Game.PEvent (Game.Place pSpot target (Board.HandIndex {unHandIndex = idx})) -> do
      id <- Board.lookupHandM (Board.toHand board pSpot) idx
      return $ go pSpot target id
    Game.PEvent (Game.Place' pSpot target id) ->
      pure $ go pSpot target id
  where
    duration = 2
    attackTile = Shared.tileToFilepath shared Tile.Sword1 Tile.Sixteen
    heartTile = Shared.tileToFilepath shared Tile.Heart Tile.Sixteen
    go pSpot target =
      \case
        IDC {} -> NoAnimation
        IDI {} -> NoAnimation -- We should rather highlight the new item in Board.T 'UI
        id@(IDN _) ->
          Game.Application pSpot target (Shared.unsafeIdentToCard shared id & Card.unlift)

-- | The index of the card with this 'Card.ID', in the hand of the
-- player at the given spot
idToHandIndex :: Board.T 'Core -> Spots.Player -> Card.ID -> Maybe Board.HandIndex
idToHandIndex board pSpot id =
  find
    (\(_, m) -> m == id)
    (Board.toHand board pSpot & zip [Board.HandIndex 0 ..])
    <&> fst

-- | Play a 'Neutral'. Doesn't deal with consuming mana (done by caller)
playNeutralM ::
  MonadError Text m =>
  MonadWriter (Board.T 'UI) m =>
  MonadState Shared.Model m =>
  Board.T 'Core ->
  Spots.Player ->
  Target ->
  Neutral ->
  m (Board.T 'Core, Maybe Event)
playNeutralM board _playingPlayer target n =
  case (n, target) of
    (InfernalHaste, PlayerTarget pSpot) ->
      return (board, event)
      where
        event =
          nextAttackSpot board pSpot Nothing
            <&> (\cSpot -> Attack pSpot cSpot True False)
    (Health, CardTarget pSpot cSpot) -> do
      let increase = 1
      reportEffect pSpot cSpot (mempty {Board.hitPointsChange = increase})
      return (addHitpoints pSpot cSpot increase, Nothing)
    (HuntingHorn, PlayerTarget pSpot) -> do
      -- TODO @smelc return an Animation.Message value
      let forests = Board.toPart board pSpot & Board.deco & Map.filter ((==) Board.Forest) & Map.keys
          board' = Board.adjustMany @_ @(Creature 'Core) pSpot forests (addSkill Skill.FearTmp) board
      return (board', Nothing)
    (Life, CardTarget pSpot cSpot) -> do
      let increase = 3
      reportEffect pSpot cSpot (mempty {Board.hitPointsChange = increase})
      return (addHitpoints pSpot cSpot increase, Nothing)
    (Pandemonium, PlayerTarget pSpot) -> do
      -- Take existing creatures
      let creatures = Board.toInPlace board pSpot & Map.elems
      -- Generate random spots
      spots <- Random.shuffleM Spots.allCards
      -- Put existing creatures on new spots
      let board' = Board.setInPlace pSpot (Map.fromList $ zip spots creatures) board
      -- Report fadeIn effects on each new spot
      traverse_ (\cSpot -> reportEffect pSpot cSpot (mempty {Board.fade = Constants.FadeIn})) spots
      return (board', Nothing)
    (Plague, PlayerTarget pSpot) -> do
      board' <- applyPlagueM board pSpot
      return (board', Nothing)
    (StrengthPot, CardTarget pSpot cSpot) ->
      case Board.toInPlaceCreature board pSpot cSpot of
        Nothing -> return (board, Nothing)
        Just c@Creature {skills} -> do
          reportEffect pSpot cSpot (mempty {Board.attackChange = Nat.natToInt Constants.strengthPotAttackBonus})
          let board' = Board.insert pSpot cSpot (c {skills = skills ++ [Skill.StrengthPot]}) board
          return (board', Nothing)
    _ -> throwError $ Text.pack $ "Wrong (Target, Neutral) combination: (" ++ show target ++ ", " ++ show n ++ ")"
  where
    addHitpoints pSpot cSpot hps =
      Board.adjust @_ @(Creature 'Core) pSpot cSpot (\c@Creature {hp} -> c {hp = hp + hps}) board
    addSkill sk c@Creature {skills} = c {skills = skills ++ [sk]}

-- | Play a 'Creature'. Doesn't deal with consuming mana (done by caller)
playCreatureM ::
  MonadError Impossible m =>
  MonadWriter (Board.T 'UI) m =>
  Shared.Model ->
  Board.T 'Core ->
  Spots.Player ->
  Spots.Card ->
  Creature 'Core ->
  m (Board.T 'Core)
playCreatureM shared board pSpot cSpot creature@Creature {skills} = do
  board <- playOneM board cSpot creature
  board <- foldM (\b (spot, card) -> tryPlayOneM b spot card) board extras
  return board
  where
    extras
      | Skill.Falconer `elem` skills =
          [(c, Shared.idToCreature shared falcon []) | c <- Spots.frontSpots \\ [cSpot]]
            & map liftJust
      | otherwise = []
    liftJust = \case
      (c, Just crea) ->
        (c, Card.unlift crea)
      (_, Nothing) ->
        {- Should never happen because IDs in 'extra' are valid -} error "playCreatureM::liftJust failure"
    falcon :: Card.CreatureID = CreatureID Card.Falcon Card.Sylvan
    transientize (s :: Spots.Card) =
      Board.adjust pSpot s (\(c :: Creature 'Core) -> c {transient = True})
    -- Play one create on the player spot of 'playCreatureM'. Fails in
    -- the error monad if impossible, so should be used for the main creature.
    playOneM ::
      MonadWriter (Board.T 'UI) m =>
      MonadError Impossible m =>
      Board.T 'Core ->
      Spots.Card ->
      Creature 'Core ->
      m (Board.T 'Core)
    playOneM b c creature =
      case Map.member c $ Board.toInPlace b pSpot of
        True ->
          -- This used to be an error, but now this can happen with
          -- Infernal Haste + Flail of the Damned. Haste makes
          -- the flail spawn an unexpected creature, which may make
          -- an event computed by the AI fail.
          throwError CannotPlaceCreature
        _ -> do
          reportEffect pSpot c $ mempty {Board.fade = Constants.FadeIn} -- report creature addition effect
          b <- pure $ Board.insert pSpot c creature b -- set creature
          b <- applyDiscipline b creature pSpot c
          b <- applySquire b creature pSpot c
          b <- applySylvan pSpot c b
          return b
    -- Play one creature, return the initial board if unplayable. Adequate
    -- for playing extra creatures. If the extra creature is successfully
    -- plaed, it is marked transient.
    tryPlayOneM (b :: Board.T 'Core) (s :: Spots.Card) (creature :: Creature 'Core) =
      playOneM b s creature
        & runExceptT
        <&> eitherToMaybe
        <&> (fmap (transientize s))
        <&> fromMaybe b

-- | Play an 'Item'. Doesn't deal with consuming mana (done by caller)
playItemM ::
  MonadError Impossible m =>
  MonadWriter (Board.T 'UI) m =>
  Board.T 'Core ->
  Spots.Player ->
  Spots.Card ->
  Item ->
  m (Board.T 'Core)
playItemM board pSpot cSpot item =
  case Board.toInPlaceCreature board pSpot cSpot of
    Nothing ->
      throwError CannotPlaceItem
    Just creature
      | not (meetsRequirement item creature) ->
          throwError CannotPlaceItem
    Just creature -> do
      reportEffect pSpot cSpot $ mempty {Board.fade = Constants.FadeIn}
      -- TODO @smelc record animation for item arrival
      let creature' = installItem item creature
      return $ Board.insert pSpot cSpot creature' board

meetsRequirement :: Item -> Creature 'Core -> Bool
meetsRequirement item Creature {skills} =
  case Card.requirement item of
    NoReq -> True
    SomeReq s -> s `elem` (map Skill.lift skills)

installItem ::
  Item ->
  Creature 'Core ->
  Creature 'Core
installItem item c@Creature {hp, items} =
  assert (meetsRequirement item c) $
    c {hp = hp + hpChange, items = item : items}
  where
    -- Items adding health are resolved here, as opposed to items
    -- adding attack, which are dealt with in 'Total'
    hpChange =
      case item of
        AxeOfRage -> 0
        BowOfStrength -> 0
        CloakOfGaia -> 0
        Crown -> 0
        CrushingMace -> 0
        FlailOfTheDamned -> 0
        SkBanner -> 0
        SpikyMace -> 0
        SwordOfMight -> 1

applyDiscipline ::
  MonadWriter (Board.T 'UI) m =>
  -- | The input board
  Board.T 'Core ->
  -- | The creature being played
  Creature 'Core ->
  -- | The part where the creature is being played
  Spots.Player ->
  -- | The spot where the creature arrives. It has already been filled with 'creature'.
  Spots.Card ->
  m (Board.T 'Core)
applyDiscipline board creature pSpot cSpot
  | not $ Total.isDisciplined creature = return board
  | otherwise = do
      traverse_ ((\dSpot -> reportEffect pSpot dSpot effect)) disciplinedNeighbors
      return $ Board.adjustMany @_ @(Creature 'Core) pSpot disciplinedNeighbors (apply change) board
  where
    change = StatChange {attackDiff = 1, hpDiff = 1}
    effect = changeToEffect change
    disciplinedNeighbors :: [Spots.Card] =
      Board.toNeighbors board pSpot cSpot Board.Cardinal
        & filter (\(_, c) -> Total.isDisciplined c)
        & filter (\(spot, _) -> spot /= cSpot)
        & map fst

applyFillTheFrontline ::
  Board.T 'Core ->
  Spots.Player ->
  Board.T 'Core
applyFillTheFrontline board pSpot =
  Board.setPart board pSpot part {Board.inPlace = Map.fromList bindings'}
  where
    part = Board.toPart board pSpot
    inPlace = Board.inPlace part
    spots = Map.keys inPlace
    bindings' =
      Map.toList inPlace
        & map
          ( \(cSpot, v) ->
              ( if Spots.inTheBack cSpot
                  && applies v
                  && Board.switchLine cSpot `notElem` spots
                  then Board.switchLine cSpot
                  else cSpot,
                v
              )
          )
    applies Creature {skills} = Skill.Ranged `elem` skills

applyPlague ::
  Board.T 'Core ->
  -- | The part on which to apply plague
  Spots.Player ->
  Board.T 'Core
applyPlague board actingPlayer = applyPlagueM board actingPlayer & runWriter & fst

applyPlagueM ::
  MonadWriter (Board.T 'UI) m =>
  -- | The input board
  Board.T 'Core ->
  -- | The part on which to apply plague
  Spots.Player ->
  m (Board.T 'Core)
applyPlagueM board pSpot = do
  -- Record animation
  traverse_ (\(cSpot, c) -> reportEffect pSpot cSpot $ plagueEffect c) $ Map.toList affecteds
  -- Do core stuff
  return $
    Map.foldrWithKey
      (\cSpot c b -> applyInPlaceEffectOnBoard (plagueEffect c) b (pSpot, cSpot, c))
      board
      affecteds
  where
    affecteds = Board.toInPlace board pSpot
    baseEffect = mempty {Board.hitPointsChange = -1}
    plagueEffect Creature {hp} | hp <= 1 = baseEffect {Board.death = Board.UsualDeath}
    plagueEffect _ = baseEffect {Board.fadeOut = [Tile.HeartBroken]}

applySquire ::
  MonadWriter (Board.T 'UI) m =>
  -- | The input board
  Board.T 'Core ->
  -- | The creature being played
  Creature 'Core ->
  -- | The part where the creature is being played
  Spots.Player ->
  -- | The spot where the creature arrives. It has already been filled with the creature.
  Spots.Card ->
  m (Board.T 'Core)
applySquire board Creature {skills} pSpot cSpot =
  if Skill.Squire `notElem` skills || Spots.inFront cSpot || not knightInFront
    then return board -- No change
    else do
      reportEffect pSpot frontSpot effect
      return $ Board.adjustMany @_ @(Creature 'Core) pSpot [frontSpot] (apply change) board
  where
    change :: StatChange = mempty {hpDiff = 1}
    effect = changeToEffect change
    frontSpot = Board.switchLine cSpot
    knightInFront =
      Board.toInPlaceCreature board pSpot frontSpot
        <&> (\Creature {skills = skills'} -> Skill.Knight `elem` skills')
        & fromMaybe False

drawCards ::
  Shared.Model ->
  Board.T 'Core ->
  -- | The player drawing cards
  Spots.Player ->
  -- | The sources from which to draw the cards
  [DrawSource] ->
  (Shared.Model, Board.T 'Core, Board.T 'UI)
drawCards shared board pSpot =
  \case
    [] -> (shared, board, mempty)
    (hd : rest) -> runIdentity $ do
      -- Dummy monad, to avoid variable numbering TODO @smelc remove numbering
      (shared', board', boardui) <- pure $ drawCard shared board pSpot hd
      (shared'', board'', boardui') <- pure $ drawCards shared' board' pSpot rest
      return (shared'', board'', boardui <> boardui')

drawCard ::
  Shared.Model ->
  Board.T 'Core ->
  -- | The player drawing cards
  Spots.Player ->
  -- | The reason for drawing a card
  DrawSource ->
  (Shared.Model, Board.T 'Core, Board.T 'UI)
drawCard shared board pSpot src =
  drawCardM board pSpot src
    & runWriterT
    & flip runState shared
    & (\((b, bui), s) -> (s, b, bui))

drawCardM ::
  MonadWriter (Board.T 'UI) m =>
  MonadState Shared.Model m =>
  Board.T 'Core ->
  -- | The playing player, used to find the hand to draw from if the
  -- 'DrawSource' is 'Native'.
  Spots.Player ->
  DrawSource ->
  m (Board.T 'Core)
drawCardM board p src =
  case (srcKind board & runExcept, stack) of
    (_, []) -> return board -- cannot draw: stack is empty
    (Left (_ :: Impossible), _) -> return board -- cannot draw: 'src' is invalid
    (Right witness, _) -> do
      let hand = Board.toHand board pSpot
      shared <- get
      let stdgen = Shared.getStdGen shared
      let (idrawn, stdgen') = randomR (0, assert (stackLen >= 1) $ stackLen - 1) stdgen
      put $ Shared.withStdGen shared stdgen'
      let ident :: Card.ID = stack !! idrawn
      let stack' = deleteAt idrawn stack
      let hand' = hand ++ [ident]
      tell $ Board.addToHand mempty pSpot $ length hand
      let board' = Board.setStack board pSpot stack'
      let board'' = Board.setHand board' pSpot hand'
      let board''' = consumeSrc board'' witness
      return board'''
  where
    (stack, stackLen) = (Board.toStack board pSpot, length stack)
    -- The spots to draw cards from
    pSpot = case src of Native -> p; CardDrawer custom _ -> custom
    srcKind ::
      MonadError Impossible m =>
      Board.T 'Core ->
      m (Maybe (Spots.Card, Creature 'Core, Int, Skill.State))
    srcKind b =
      case src of
        Native -> pure Nothing -- Nothing to consume when drawing
        CardDrawer pSpot' cSpot' ->
          -- Need to consume 'DrawCard' skill when drawing
          case Board.toInPlaceCreature (assert (pSpot == pSpot') b) pSpot' cSpot' of
            Nothing -> throwError CannotDraw
            Just c@Creature {skills} ->
              case findIndex (\case Skill.DrawCard b -> b; _ -> False) skills of
                Nothing -> throwError CannotDraw
                Just i -> pure $ Just (cSpot', c, i, Skill.DrawCard False)
    consumeSrc b Nothing = b -- No change
    consumeSrc b (Just (cSpot, c@Creature {..}, skilli, skill')) =
      -- Set new skill
      Board.insert pSpot cSpot (c {skills = setAt skilli skill' skills}) b

transferCards ::
  Shared.Model ->
  Board.T 'Core ->
  Spots.Player ->
  (Shared.Model, Board.T 'Core, Board.T 'UI)
transferCards shared board pSpot =
  (Shared.withStdGen shared stdgen', board', boardui')
  where
    (board', boardui', stdgen') =
      transferCards' (Shared.getStdGen shared) board pSpot

transferCards' ::
  StdGen ->
  Board.T 'Core ->
  Spots.Player ->
  (Board.T 'Core, Board.T 'UI, StdGen)
transferCards' stdgen board pSpot =
  transferCardsM board pSpot & flip runRandT stdgen & runWriter & reorg
  where
    reorg ((b, s), bui) = (b, bui, s)

-- | Transfer cards from the discarded stack to the other stack
transferCardsM ::
  MonadRandom m =>
  MonadWriter (Board.T 'UI) m =>
  Board.T 'Core ->
  Spots.Player ->
  m (Board.T 'Core)
transferCardsM board pSpot =
  if not needTransfer
    then pure board
    else do
      tell $ Board.mapDiscarded pSpot (const $ -length discarded) mempty
      tell $ Board.setStack mempty pSpot (length discarded)
      discarded' <- shuffleM discarded
      let part' = part {Board.discarded = [], Board.stack = stack ++ discarded'}
      return $ Board.setPart board pSpot part'
  where
    nbCardsToDraw = cardsToDraw board pSpot False & length
    (stack, stackSize) = (Board.toStack board pSpot, length stack)
    needTransfer = nbCardsToDraw > stackSize
    discarded = Board.toDiscarded board pSpot
    part = Board.toPart board pSpot

-- | board id pSpot target holds iff player at 'pSpot' can play card 'id'
-- on 'target'
appliesTo :: Board.T 'Core -> Card.ID -> Spots.Player -> Target -> Bool
appliesTo board id playingPlayer target =
  correctPlayer && correctHoleyness && satisfiedConstraints
  where
    correctPlayer =
      case whichPlayerTarget id of
        Playing -> targetToPlayerSpot target == playingPlayer
        Opponent -> targetToPlayerSpot target /= playingPlayer
    correctHoleyness =
      case (target, Card.targetType id) of
        (CardTarget pSpot cSpot, CardTargetType Hole) ->
          Board.toInPlaceCreature board pSpot cSpot & isNothing
        (CardTarget pSpot cSpot, CardTargetType Occupied) ->
          Board.toInPlaceCreature board pSpot cSpot & isJust
        (PlayerTarget _, PlayerTargetType) -> True
        _ -> False
    satisfiedConstraints =
      case (id, target) of
        (IDC {}, _) -> True
        (IDN {}, _) -> True
        (IDI {}, PlayerTarget _) -> False
        (IDI item, CardTarget pSpot cSpot) ->
          case Board.toInPlaceCreature board pSpot cSpot of
            Nothing -> False
            Just c -> meetsRequirement item c

applyAssassinsM ::
  MonadWriter (Board.T 'UI) m =>
  MonadState Shared.Model m =>
  -- | The input board
  Board.T 'Core ->
  -- | The part where assassin takes effect
  Spots.Player ->
  m (Board.T 'Core)
applyAssassinsM board pSpot =
  foldM (\b pair -> applyAssassinM b pSpot pair) board assassins
  where
    inPlace :: Map.Map Spots.Card (Creature 'Core) = Board.toInPlace board pSpot
    assassins :: [(Spots.Card, Creature 'Core)] =
      Map.filter (`has` (Skill.Assassin :: Skill.State)) inPlace
        & Map.toList

applyAssassinM ::
  MonadWriter (Board.T 'UI) m =>
  MonadState Shared.Model m =>
  -- | The input board
  Board.T 'Core ->
  -- | The part where assassin takes effect
  Spots.Player ->
  -- | The concerned assassin. It is at @board[pSpot]@.
  (Spots.Card, Creature 'Core) ->
  m (Board.T 'Core)
applyAssassinM board pSpot (cSpot, creature) =
  -- We reverse the result, because sorting is ascending (0, 2, 4, etc.); but
  -- we want the bigger value.
  case reverse $ sortBy (\(_, c1) (_, c2) -> compare (eval c1) (eval c2)) targetFrontSpots of
    [] -> return board -- Cannot move the assassin
    ((bestSpot, _) : _) ->
      return $ Mechanics.move (Mechanics.mkEndoMove pSpot cSpot bestSpot) board
  where
    other :: Spots.Player =
      assert
        (Board.toInPlaceCreature board pSpot cSpot == Just creature)
        (Spots.other pSpot)
    enemyMap :: Map Spots.Card (Creature 'Core) = Board.toInPlace board other
    -- The free front spots of 'board', with the enemy in front of them. Left
    -- member is in 'pSpot' while right member is in 'other'
    targetFrontSpots :: [(Spots.Card, Creature 'Core)] =
      Spots.frontSpots \\ (Board.toInPlace board pSpot & Map.keys)
        & map (\c -> (c, c & enemySpots creature & toSpots & map (enemyMap Map.!?) & catMaybes))
        & map (Bifunctor.second listToMaybe)
        & map liftMaybe
        & catMaybes
    liftMaybe = \case (_, Nothing) -> Nothing; (x, Just y) -> Just (x, y)
    eval Creature {attack, hp} = Damage.mean attack + hp

applyBrainlessM ::
  MonadWriter (Board.T 'UI) m =>
  MonadState Shared.Model m =>
  -- | The input board
  Board.T 'Core ->
  -- | The part where brainless takes effect
  Spots.Player ->
  m (Board.T 'Core)
applyBrainlessM board pSpot
  | null brainless = return board -- Nothing to do, avoid useless things
  | otherwise = do
      shuffledFreeSpots :: [Spots.Card] <- Random.shuffleM freeSpots
      let shuffled = Map.fromList (zip shuffledFreeSpots (Map.elems brainless))
          inPlace' = Map.union shuffled rest
      return (Board.setInPlace pSpot inPlace' board)
  where
    inPlace :: Map.Map Spots.Card (Creature 'Core) = Board.toInPlace board pSpot
    freeSpots :: [Spots.Card] = Spots.allCards \\ Map.keys inPlace
    (brainless, rest) = Map.partition (`has` (Skill.Brainless :: Skill.State)) inPlace

-- | The effect of the 'Church' card. If this enum is changed, beware
-- to adapt 'allChurchEffects'.
data ChurchEffect
  = -- | All creatures in part get +1 attack
    PlusOneAttack
  | -- | All creatures in part get +1 health
    PlusOneHealth
  | -- | Player of this part gets +1 mana
    PlusOneMana
  deriving (Bounded, Enum)

-- | All possible effects of the 'Church' card
allChurchEffects :: NE.NonEmpty ChurchEffect
allChurchEffects = PlusOneAttack NE.:| [PlusOneHealth ..]

applyChurch :: Shared.Model -> Board.T 'Core -> Spots.Player -> (Board.T 'Core, Board.T 'UI, Shared.Model)
applyChurch shared board pSpot =
  applyChurchM board pSpot
    & runWriterT
    & flip runState shared
    & reorg
  where
    reorg ((x, y), z) = (x, y, z)

applyChurchM ::
  MonadWriter (Board.T 'UI) m =>
  MonadState Shared.Model m =>
  -- | The input board
  Board.T 'Core ->
  -- | The part where churchs take effect
  Spots.Player ->
  m (Board.T 'Core)
applyChurchM board pSpot = do
  -- TODO @smelc generate one effect per church
  effect <- Random.oneof allChurchEffects
  case effect of
    PlusOneAttack ->
      go (\c@Creature {attack} -> c {Card.attack = attack +^ 1}) (mempty {Board.attackChange = 1})
    PlusOneHealth ->
      go (\c@Creature {hp} -> c {hp = hp + 1}) (mempty {Board.attackChange = 1})
    PlusOneMana -> do
      tell (Board.setMana 1 pSpot mempty :: Board.T 'UI)
      return $ Board.setMana (Board.toPart board pSpot & Board.mana) pSpot board
  where
    go creatureFun effect = do
      traverse_ (\cSpot -> reportEffect pSpot cSpot effect) (map fst others)
      return $ Board.adjustMany @_ @(Creature 'Core) pSpot (Set.toList affectedSpots) creatureFun board
    creatures = Board.toInPlace board pSpot
    (churchs :: [(Spots.Card, Creature 'Core)], others :: [(Spots.Card, Creature 'Core)]) =
      creatures & Map.filter isChurch & Map.toList & partition (isChurch . snd)
    affectedSpots :: Set.Set Spots.Card =
      (Set.fromList (map fst others)) Set.\\ (Set.fromList (map fst churchs))
    isChurch Creature {creatureId = CreatureID {creatureKind = kind}} = kind == Card.Church

applyCreateForestM ::
  MonadWriter (Board.T 'UI) m =>
  MonadState Shared.Model m =>
  -- | The input board
  Board.T 'Core ->
  -- | The part where churchs take effect
  Spots.Player ->
  m (Board.T 'Core)
applyCreateForestM board pSpot = do
  shared <- get
  case (priests, Random.pick shared targetSpots) of
    ([], _) -> return board
    (_, (Nothing, _)) -> return board
    (priestSpot : _, (Just forestSpot, shared')) -> do
      put shared'
      board' <-
        Board.adjust @_ @(Creature 'Core) pSpot priestSpot consume board
          & Board.insert pSpot forestSpot Board.Forest
          & applySylvan pSpot forestSpot
      applyCreateForestM board' pSpot
  where
    part = Board.toPart board pSpot
    priests =
      part
        & Board.inPlace
        & Map.filter (`has` (Skill.GreenAffinity True :: Skill.State))
        & Map.keys
    targetSpots =
      [ cSpot
        | cSpot <- Spots.allCards,
          Board.deco part & Map.notMember cSpot
      ]
    consume c@Creature {skills} = c {skills = consume' skills}
    consume' =
      \case
        [] -> []
        (Skill.GreenAffinity True : rest) -> Skill.GreenAffinity False : rest
        (fst : rest) -> fst : consume' rest

-- | Trigger the sylvan skill at the given place, if relevant. Returns the
-- updated board.
applySylvan ::
  MonadWriter (Board.T 'UI) m =>
  Spots.Player ->
  Spots.Card ->
  Board.T 'Core ->
  m (Board.T 'Core)
applySylvan pSpot cSpot board =
  case Board.toInPlaceCreature board pSpot cSpot of
    Nothing -> pure board
    Just c | not (c `has` (Skill.Sylvan :: Skill.State)) -> pure board
    Just _ | (Board.toPart board pSpot & Board.deco & Map.lookup cSpot) /= Just (Board.Forest) -> pure board
    Just c@Creature {attack, hp} -> do
      let c' = c {Card.attack = attack Damage.+^ 1, hp = hp + 1}
      reportEffect pSpot cSpot $ mempty {Board.attackChange = 1, Board.hitPointsChange = 1}
      pure $ Board.insert pSpot cSpot c' board

applyFearNTerror ::
  -- | The input board
  Board.T 'Core ->
  -- | The part causing fear
  Spots.Player ->
  (Board.T 'Core, Board.T 'UI)
applyFearNTerror board pSpot =
  applyFearNTerrorM board pSpot & runWriter

applyFearNTerrorM ::
  MonadWriter (Board.T 'UI) m =>
  -- | The input board
  Board.T 'Core ->
  -- | The part causing fear
  Spots.Player ->
  m (Board.T 'Core)
applyFearNTerrorM board affectingSpot = do
  traverse_ (\spot -> reportEffect affectedSpot spot $ deathBy Board.DeathByTerror) terrorAffected
  traverse_ (\spot -> reportEffect affectedSpot spot $ deathBy Board.DeathByFear) fearAffected
  return $
    board
      & Board.setInPlace affectedSpot affectedInPlace''
      & Board.setInPlace affectingSpot affectingInPlace'
      & Board.mapDiscarded affectedSpot (++ killedToDiscard)
  where
    affectedSpot = Spots.other affectingSpot
    affectingInPlace = Board.toInPlace board affectingSpot
    causingFear = Map.filter Total.causesFear affectingInPlace
    causingTerror = Map.filter Total.causesTerror affectingInPlace
    switch :: Spots.Card -> Maybe Spots.Card -- From affecting spot to affected spot, and back
    switch cSpot = allEnemySpots cSpot & listToMaybe
    terrorAffectedSpots :: [Spots.Card] = Map.keys causingTerror & mapMaybe switch
    fearAffectedSpots :: [Spots.Card] =
      Map.keys causingFear
        & mapMaybe switch
    removeAll l1 l2 = [x | x <- l1, x `notElem` l2]
    affectedInPlace = Board.toInPlace board affectedSpot
    fearAffected :: [Spots.Card] =
      affectedInPlace
        & Map.filterWithKey
          (\spot c -> spot `elem` fearAffectedSpots && Total.affectedByFear True c)
        & Map.keys
    terrorAffected :: [Spots.Card] =
      affectedInPlace
        & Map.filterWithKey
          (\spot c -> spot `elem` terrorAffectedSpots && Total.affectedByTerror True c)
        & Map.keys
    killedToDiscard :: [Card.ID] =
      map (Board.toInPlaceCreature board affectedSpot) (terrorAffected ++ fearAffected)
        & catMaybes
        & filter (\Creature {transient} -> not transient) -- Transient creatures do not got to discarded stack
        & map (\Creature {creatureId, items} -> IDC creatureId items)
    affectedInPlace' :: Map Spots.Card (Creature 'Core) =
      -- remove creatures killed by terror
      removeKeys affectedInPlace terrorAffected
    terrorKillers :: [Spots.Card] =
      -- Affecting spots that cause a death by terror
      removeAll (Map.keys affectedInPlace) (Map.keys affectedInPlace')
        & mapMaybe switch
    affectedInPlace'' :: Map Spots.Card (Creature 'Core) =
      -- remove creatures killed by fear
      removeKeys affectedInPlace' fearAffected
    fearKillers :: [Spots.Card] =
      -- Affecting spots that cause a death by fear
      removeAll (Map.keys affectedInPlace') (Map.keys affectedInPlace'')
        & mapMaybe switch
    removeKeys map [] = map
    removeKeys map (k : rest) = removeKeys (Map.delete k map) rest
    affectingInPlace' =
      -- consume skills
      Map.mapWithKey consumeTerror affectingInPlace
        & Map.mapWithKey consumeFear
    deathBy cause = mempty {Board.death = cause}
    consumeFear cSpot c | cSpot `notElem` fearKillers = c
    consumeFear _ c@Creature {skills} = c {skills = consumeFearSkill skills}
    consumeFearSkill [] = []
    consumeFearSkill ((Skill.Fear True) : rest) = Skill.Fear False : rest
    consumeFearSkill (s : rest) = s : consumeFearSkill rest
    consumeTerror cSpot c | cSpot `notElem` terrorKillers = c
    consumeTerror _ c@Creature {skills} = c {skills = consumeTerrorSkill skills}
    consumeTerrorSkill [] = []
    consumeTerrorSkill ((Skill.Terror True) : rest) = Skill.Terror False : rest
    consumeTerrorSkill (s : rest) = s : consumeTerrorSkill rest

applyGrowthM ::
  MonadWriter (Board.T 'UI) m =>
  -- | The input board
  Board.T 'Core ->
  -- | The part where growth triggers
  Spots.Player ->
  -- | The part where to try apply growth
  Spots.Card ->
  m (Board.T 'Core)
applyGrowthM board pSpot cSpot =
  case Board.toInPlaceCreature board pSpot cSpot of
    Nothing -> return board
    Just (Creature {skills}) | (Skill.Growth True) `notElem` skills -> return board
    Just _ | Map.lookup cSpot deco /= (Just Board.Forest) -> return board
    Just c@(Creature {attack, skills, hp}) -> do
      let effect = mempty {Board.attackChange = 1, Board.hitPointsChange = 1}
          c' = c {Card.attack = attack +^ 1, hp = hp + 1, skills = consume skills}
      reportEffect pSpot cSpot effect
      return $ Board.insert pSpot cSpot c' board
  where
    consume [] = []
    consume (Skill.Growth True : rest) = Skill.Growth False : rest
    consume (fst : rest) = fst : consume rest
    deco = Board.toPart board pSpot & Board.deco

-- | Applies the effect of kings, i.e. give +1/+1 to all knights, for every
-- king in place.
applyKingM ::
  MonadWriter (Board.T 'UI) m =>
  -- | The input board
  Board.T 'Core ->
  -- | The part where kings are used
  Spots.Player ->
  m (Board.T 'Core)
applyKingM board pSpot = do
  traverse_ ((\dSpot -> reportEffect pSpot dSpot effect)) knightSpots
  return $ Board.adjustMany @_ @(Creature 'Core) pSpot knightSpots (apply change) board
  where
    inPlace = Board.toInPlace board pSpot
    nbKings = Map.filter (\Creature {skills} -> Skill.King `elem` skills) inPlace & Map.size
    baseChange :: StatChange = StatChange {attackDiff = 1, hpDiff = 1}
    change :: StatChange = mconcat $ replicate nbKings baseChange
    effect = changeToEffect change
    knightSpots =
      Map.filter (\Creature {skills} -> Skill.Knight `elem` skills) inPlace
        & Map.keys

-- | Card at [pSpot],[cSpot] attacks; causing changes to a board
attack ::
  MonadWriter (Board.T 'UI) m =>
  MonadState Shared.Model m =>
  Board.T 'Core ->
  -- The attacker's player spot
  Spots.Player ->
  -- The attacker's card spot
  Spots.Card ->
  m (Board.T 'Core)
attack board pSpot cSpot =
  case (attacker, allyBlocker) of
    (Nothing, _) -> return board -- no attacker
    (Just _, _)
      | isStupid board pSpot cSpot ->
          -- TODO @smelc record an animation
          return board
    (_, Just _) -> return board -- an ally blocks the way
    (Just hitter, _) ->
      case enemySpots hitter cSpot of
        Ace -> do
          -- Attacker has the Ace skill. Attack an occupied spot.
          -- Never contributes to score.
          let occupiedSlots = Board.toInPlace board attackeePSpot & Map.toList
          occupiedSlots <- Random.shuffleM occupiedSlots <&> safeHead
          case occupiedSlots of
            Nothing -> return board -- Nothing to attack. XXX @smelc record question mark animation
            Just (attackedSpot, attacked) -> do
              attackOneSpot board (hitter, pSpot, cSpot) (attacked, attackedSpot)
        Imprecise -> do
          -- Attacker has the Imprecise skill. Attack a spot at random.
          -- Never contribute to score.
          attackedSpot :: Spots.Card <- Random.oneof Spots.allCardsNE
          case Board.toInPlaceCreature board attackeePSpot attackedSpot of
            Nothing -> do
              -- Imprecise creature targets an empty spot, report attack
              -- and exposion.
              reportEffect pSpot cSpot $ mempty {Board.attackBump = True}
              reportEffect attackeePSpot attackedSpot $ mempty {Board.fadeOut = [Tile.Explosion]}
              pure board -- No change
            Just attacked ->
              -- Imprecise creature attacks an occupied spot
              attackOneSpot board (hitter, pSpot, cSpot) (attacked, attackedSpot)
        Spots [] -> do
          -- Cannot attack: attack is 0 or fighter is in the back. XXX @smelc record an animation?
          return board
        Spots attackedSpots -> do
          let attackedCreatures :: [(Creature 'Core, Spots.Card)] = spotsToEnemies attackedSpots
          if null attackedCreatures
            then do
              -- Creature can attack an enemy spot, but it is empty: contributed to the score
              let place = Total.mkPlace board pSpot cSpot
              hit :: Nat <- deal $ Total.attack (Just place) hitter
              reportEffect pSpot cSpot $ mempty {Board.attackBump = True, Board.scoreChange = natToInt hit}
              return $ Board.increaseScore board pSpot hit
            else do
              -- or something to attack; attack it
              foldM
                (\b attackee -> attackOneSpot b (hitter, pSpot, cSpot) attackee)
                board
                attackedCreatures
  where
    safeHead = \case [] -> Nothing; x : _ -> Just x
    attackeePSpot = Spots.other pSpot
    attackersInPlace :: Map Spots.Card (Creature 'Core) = Board.toInPlace board pSpot
    attackeesInPlace :: Map Spots.Card (Creature 'Core) = Board.toInPlace board attackeePSpot
    attacker :: Maybe (Creature 'Core) = attackersInPlace !? cSpot
    attackerSkills :: [Skill] = (Card.to attacker) & map Skill.lift
    allyBlocker :: Maybe (Creature 'Core) =
      if any (`elem` attackerSkills) [Skill.Imprecise, Skill.LongReach, Skill.Support, Skill.Ranged]
        then Nothing -- attacker bypasses ally blocker (if any)
        else allyBlockerSpot cSpot >>= (attackersInPlace !?)
    -- Given attacked spots, restrict to the ones with enemies, and return the
    -- enemies along the spots.
    spotsToEnemies :: [Spots.Card] -> [(Creature 'Core, Spots.Card)]
    spotsToEnemies spots =
      [(spot,) <$> (attackeesInPlace !? spot) | spot <- spots]
        & catMaybes
        -- Breath attack makes the attacker attack the two spots, otherwise
        -- take the first one
        & (if Skill.BreathIce `elem` attackerSkills then id else take 1)
        & map swap

-- | @attackOneSpot board (hitter, pSpot, cSpot) (hit, hitSpot)@
-- returns the board after having @hitter@ (at @(pSpot, cSpot)@) attacked
-- @hit@ at @hitSpot@.
attackOneSpot ::
  MonadWriter (Board.T 'UI) m =>
  MonadState Shared.Model m =>
  Board.T 'Core ->
  (Creature 'Core, Spots.Player, Spots.Card) ->
  (Creature 'Core, Spots.Card) ->
  m (Board.T 'Core)
attackOneSpot board (hitter, pSpot, cSpot) (hit, hitSpot) = do
  (effect@Board.InPlaceEffect {death, extra}, flyingSpot) <-
    singleAttack (place, hitter) (hitPlace, hit)
  board <-
    pure $
      applyInPlaceEffectOnBoard effect board (hitPspot, hitSpot, hit)
        & moveFlyerFun flyingSpot
  reportEffect pSpot cSpot $ mempty {Board.attackBump = True} -- make hitter bump
  reportEffect hitPspot hitSpot effect -- hittee
  for_ flyingSpot (\flyingSpot -> reportEffect hitPspot flyingSpot flyingToEffect) -- fly to spot effect
  board <-
    if Board.isDead death
      then applyFlailOfTheDamned board hitter pSpot
      else pure board
  let behind b =
        (if Spots.inFront hitSpot then Just (Spots.switchLine hitSpot) else Nothing)
          <&> (\s -> (Board.toInPlaceCreature b hitPspot s, s))
          & (\case Just (Just c, s) -> Just (c, s); _ -> Nothing)
  case behind board of
    Just behind | 0 < extra && Total.hasRampage hitter -> do
      -- Rampage applies
      let hitter' = hitter {Card.attack = Damage.const extra}
      attackOneSpot board (hitter', pSpot, cSpot) behind -- Note that,
      -- because we recurse, the rampage attack can trigger powerful!
    _ -> do
      -- Rampage doesn't apply, powerful may
      return $ fPowerful board extra
  where
    hitPspot = Spots.other pSpot
    place = Total.mkPlace board pSpot cSpot
    hitPlace = Total.mkPlace board (Spots.other pSpot) hitSpot
    fPowerful b extra
      | extra == 0 = b
      | Total.isPowerful hitter = Board.mapScore b pSpot ((+) extra)
      | otherwise = b
    moveFlyerFun = \case
      Nothing -> id
      Just flyingSpot -> Mechanics.move (Mechanics.mkEndoMove hitPspot hitSpot flyingSpot)
    flyingToEffect = mempty {Board.fade = Constants.FadeIn}

applyInPlaceEffectOnBoard ::
  -- | The effect of the attacker on the hittee
  Board.InPlaceEffect ->
  -- | The input board
  Board.T 'Core ->
  -- | The creature being hit
  (Spots.Player, Spots.Card, Creature 'Core) ->
  -- | The updated board
  Board.T 'Core
applyInPlaceEffectOnBoard effect board (pSpot, cSpot, hittee@Creature {creatureId, items}) =
  case hittee' of
    Just _ -> board'
    Nothing | Card.transient hittee -> board' -- Dont' put hittee in discarded stack
    Nothing -> Board.mapDiscarded pSpot (++ [IDC creatureId items]) board'
  where
    hittee' = applyInPlaceEffect effect hittee
    -- Update the hittee in the board, putting Nothing or Just _:
    board' = Board.update pSpot cSpot (const hittee') board

applyInPlaceEffect ::
  -- | The effect of the attacker on the hittee
  Board.InPlaceEffect ->
  -- | The creature being hit
  Creature 'Core ->
  -- | The creature being hit, after applying the effect; or None if dead
  Maybe (Creature 'Core)
applyInPlaceEffect effect creature@Creature {..} =
  case effect of
    Board.InPlaceEffect {death} | Board.isDead death -> Nothing
    Board.InPlaceEffect {hitPointsChange = i} -> Just $ creature {hp = intToClampedNat (natToInt hp + i)}

applyFlailOfTheDamned ::
  MonadWriter (Board.T 'UI) m =>
  MonadState Shared.Model m =>
  -- The input board
  Board.T 'Core ->
  -- The hitter
  Creature 'Core ->
  -- The hitter's position
  Spots.Player ->
  m (Board.T 'Core)
applyFlailOfTheDamned board creature pSpot =
  if not hasFlailOfTheDamned
    then return noChange
    else do
      shared <- get
      let spots =
            Board.toPlayerHoleyInPlace board pSpot
              & filter (isNothing . snd)
              & map fst
      let (spawningSpot, shared') = Random.pick shared spots
      case spawningSpot of
        Nothing -> return noChange
        Just spawningSpot -> do
          put shared'
          let spawned =
                CreatureID Skeleton Undead
                  & (\cid -> Shared.idToCreature shared' cid [])
                  & fromJust
                  & Card.unlift
          let spawned' = spawned {transient = True}
          let board' = Board.insert pSpot spawningSpot spawned' board
          -- TODO @smelc record an animation highlighting the flail
          reportEffect pSpot spawningSpot $ mempty {Board.fade = Constants.FadeIn}
          return board'
  where
    hasFlailOfTheDamned = Card.items creature & elem FlailOfTheDamned
    noChange = board

type AtPlace = (Total.Place, Creature 'Core)

-- | The effect of an attack on the defender. Note that this function
-- cannot return a 'StatChange'. It needs the full expressivity of 'InPlaceEffect'.
singleAttack ::
  MonadState Shared.Model m =>
  p ~ 'Core =>
  -- | The attacker
  AtPlace ->
  -- | The defender
  AtPlace ->
  m (Board.InPlaceEffect, Maybe Spots.Card)
singleAttack
  (place, attacker@Creature {skills})
  (Total.Place {Total.place = dplace}, defender@Creature {skills = dskills}) = do
    flyingSpot <-
      if Skill.Flying `elem` dskills && not (Mechanics.isRanged attacker)
        then Mechanics.flySpot dplace
        else pure Nothing
    hit :: Nat <- deal damage
    case (flyingSpot) of
      Just flyingSpot ->
        -- Attackee flies away
        return
          ( mempty {Board.extra = hit, Board.fade = Constants.FadeOut, Board.fadeOut = [Tile.Wings]},
            Just flyingSpot
          )
      Nothing -> do
        -- Regular case (no flying involved)
        let afterHit :: Int = (natToInt (Card.hp defender)) - (natToInt hit)
        let extra :: Nat = if afterHit < 0 then Nat.intToNat (abs afterHit) else 0
        let hps' :: Nat = Nat.intToClampedNat afterHit
        let hpChangeEffect =
              if hps' <= 0
                then mempty {Board.death}
                else mempty {Board.hitPointsChange = Nat.negate hit}
        return $ ((hpChangeEffect {Board.extra}) <> ace <> imprecise, Nothing)
    where
      damage = Total.attack (Just place) attacker
      death =
        if any (\skill -> case skill of Skill.BreathIce -> True; _ -> False) skills
          then Board.DeathByBreathIce
          else Board.UsualDeath
      ace = mkFadeOut Skill.Ace Tile.Arrow
      imprecise = mkFadeOut Skill.Imprecise Tile.Explosion
      mkFadeOut skill tile =
        mempty {Board.fadeOut = if skill `elem` skills then [tile] else []}

-- | Apply a 'Damage'
deal :: MonadState Shared.Model m => Damage -> m Nat
deal Damage {base, variance}
  | variance == 0 = return base -- No need to roll a dice
  | otherwise = Random.roll base (base + variance)

-- | The spot that blocks a spot from attacking, which happens
-- | if the input spot is in the back line
allyBlockerSpot :: Spots.Card -> Maybe Spots.Card
allyBlockerSpot TopLeft = Just BottomLeft
allyBlockerSpot Top = Just Bottom
allyBlockerSpot TopRight = Just BottomRight
allyBlockerSpot _ = Nothing

-- | The other spot in the column in the spot's part
-- otherYSpot :: Spots.Card -> Spots.Card
-- otherYSpot TopLeft = BottomLeft
-- otherYSpot Top = Bottom
-- otherYSpot TopRight = BottomRight
-- otherYSpot BottomLeft = TopLeft
-- otherYSpot Bottom = Top
-- otherYSpot BottomRight = TopRight

-- | Spots that can be attacked from a spot. Spot as argument is
-- in one player part while spots returned are in the other player part.
-- The order in the result matters, the first element is the first spot
-- attacked, then the second element is attacked if the first spot is empty
-- or if the creature can attack multiple spots for some reasons.
allEnemySpots ::
  -- | The attacker's position
  Spots.Card ->
  -- | Spots that can be attacked
  [Spots.Card]
allEnemySpots cSpot =
  map Board.bottomSpotOfTopVisual spotsInSight
  where
    spotsInSight =
      case cSpot of
        TopLeft -> [TopLeft, BottomLeft]
        Top -> [Top, Bottom]
        TopRight -> [TopRight, BottomRight]
        BottomLeft -> [TopLeft, BottomLeft]
        Bottom -> [Top, Bottom]
        BottomRight -> [TopRight, BottomRight]

-- | Type to handle various custom skills when resolving attacks
data EnemySpots a
  = -- | Creature has 'Skill.Ace'
    Ace
  | -- | Creature has 'Skill.Imprecise', enemy spots don't make sense
    Imprecise
  | -- | A creature without 'Skill.Imprecise'
    Spots a
  deriving (Functor)

-- | Spots that can be attacked by a creature.
-- Spot as argument is in one player part while spots returned
-- are in the other player part.
-- The order in the result matters, the first element is the first spot
-- attacked, then the second element is attacked if the first spot is empty
-- or if the creature can attack multiple spots for some reasons.
enemySpots ::
  -- | The attacker
  Creature 'Core ->
  -- | Where the attack is
  Spots.Card ->
  EnemySpots [Spots.Card]
enemySpots c@Creature {skills} cSpot =
  case (Damage.dealer c, Skill.Imprecise `elem` skills, Skill.Ace `elem` skills) of
    (False, _, _) -> Spots [] -- Creature cannot attack
    (True, _, True) -> Ace -- Ace has precedence over Imprecise
    (True, True, _) -> Imprecise
    (True, False, False) ->
      ( case (ranged, Spots.inFront cSpot) of
          (True, _) -> spotsInSight -- ranged
          (False, True) | longReach || breathIce -> spotsInSight -- in front, longReach || breathIce
          (False, True) -> take 1 spotsInSight -- in front, no relevant skill
          (False, False) | longReach || support -> take 1 spotsInSight -- in the back, longReach || support
          (False, False) -> [] -- in the back, no relevant skill
      )
        & Spots
      where
        spotsInSight = allEnemySpots cSpot
        breathIce = Skill.BreathIce `elem` skills
        longReach = Skill.LongReach `elem` skills
        support = Skill.Support `elem` skills
        ranged = Skill.Ranged `elem` skills

toSpots :: EnemySpots [Spots.Card] -> [Spots.Card]
toSpots = \case Ace -> []; Imprecise -> []; Spots s -> s

-- | The order in which cards attack
attackOrder :: Spots.Player -> [Spots.Card]
attackOrder PlayerTop =
  [BottomRight, Bottom, BottomLeft, TopRight, Top, TopLeft]
attackOrder PlayerBot =
  map Board.bottomSpotOfTopVisual $ reverse $ attackOrder PlayerTop

-- | @nextAttackSpot b pSpot cSpot@ returns the spots to attack after @cSpot@.
-- If @cSpot@ is @Nothing@, the first spot in the order is considered; if
-- @cSpot@ is @Just _@, then spots after @cSpot@ are considered.
--
-- If pre End Turn events are changed in 'Move', this function may have
-- to be adapted to play the events beforehand. This should be discovered
-- automatically, as this property is checked with a PBT.
nextAttackSpot :: Board.T 'Core -> Spots.Player -> Maybe Spots.Card -> Maybe Spots.Card
nextAttackSpot board pSpot cSpot =
  case cSpot of
    Nothing -> find hasCreature spots
    Just cSpot ->
      let idx = elemIndex cSpot spots
       in case idx of
            Nothing -> error "wrong use of nextAttackSpot"
            Just idx ->
              let spots' = splitAt idx spots & snd & drop 1
               in find hasCreature spots'
  where
    spots :: [Spots.Card] = attackOrder pSpot
    hasCreature c = Board.toPart board pSpot & Board.inPlace & Map.member c

-- | The reason for drawing a card
data DrawSource
  = -- | Drawing one of the [nbDrawCards] cards allowed
    Native
  | -- | Drawing a card because of a creature with the [DrawCard] skill at the
    -- given position
    CardDrawer Spots.Player Spots.Card
  deriving (Eq, Ord, Show)

-- | The cards to draw, the Boolean indicates whether to bound by the
-- stack's length or not
cardsToDraw :: Board.T 'Core -> Spots.Player -> Bool -> [DrawSource]
cardsToDraw board pSpot considerStack =
  map (const Native) [0 .. natives - 1] ++ map (CardDrawer pSpot) cardsDrawer
  where
    stackLen = length $ Board.toStack board pSpot
    natives =
      let base = Constants.nbCardsToDraw
       in min base (if considerStack then stackLen else base)
    cardsDrawer =
      map (\cSpot -> (cSpot, Board.toInPlaceCreature board pSpot cSpot)) (attackOrder pSpot)
        & liftOpt
        & map (\(cSpot, c) -> nbAvailDrawCardSkill c & flip replicate cSpot)
        & concat
    liftOpt [] = []
    liftOpt ((_, Nothing) : rest) = liftOpt rest
    liftOpt ((x, Just y) : rest) = (x, y) : liftOpt rest
    nbAvailDrawCardSkill Creature {skills} =
      filter (\case Skill.DrawCard b -> b; _ -> False) skills & length
