{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
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
    drawCards,
    enemySpots,
    idToHandIndex,
    DrawSource (..),
    eventToAnim,
    keepEffectfull,
    EnemySpots (..),
    Event (..),
    MessageText (..),
    PolyResult (..),
    Result (),
    nextAttackSpot,
    play,
    playAll,
    playM,
    StatChange (..), -- exported for tests only
    transferCards,
    Target (..),
    whichPlayerTarget,
    WhichPlayerTarget (..),
  )
where

import Board
import BoardInstances
import Card hiding (ID)
import qualified Card
import qualified Constants
import Control.Exception (assert)
import Control.Lens hiding (has)
import Control.Monad.Except
import Control.Monad.Random
import Control.Monad.State
import Control.Monad.Writer
import Damage (Damage (..), (+^))
import qualified Damage
import qualified Data.Bifunctor as Bifunctor
import Data.Foldable
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
import Nat
import SharedModel (SharedModel)
import qualified SharedModel
import Skill (Skill)
import qualified Skill
import Spots
import System.Random.Shuffle (shuffleM)
import qualified Tile
import qualified Total

-- | On what a card can be applied
data Target
  = -- | Neutral card applies to all in place cards of a player
    PlayerTarget Spots.Player
  | -- | Creature card placed at given spot
    -- or Neutral card applies to a given in place card of a player
    CardTarget Spots.Player Spots.Card
  deriving (Eq, Generic, Show)

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
  IDN InfernalHaste -> Playing
  IDN Life -> Playing
  IDN Pandemonium -> Opponent
  IDN Plague -> Opponent
  IDN StrengthPot -> Playing

data Event
  = -- | Apply church of the creatures at the given 'Spots.Player'
    ApplyChurch Spots.Player
  | -- | Apply fear caused by the creatures at the given 'Spots.Player'
    ApplyFearNTerror Spots.Player
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
  | -- | Player puts a card from his hand on its part of the board. First
    -- argument is the player, second argument is the target, third argument
    -- is the card being played.
    Place Spots.Player Target HandIndex
  | -- | AI puts a card from his hand. This constructor has better
    -- testing behavior than 'Place': it makes the generated events commute.
    Place' Spots.Player Target Card.ID
  deriving (Eq, Generic, Show)

-- | The polymorphic version of 'Result'. Used for implementors that
-- do not return events and hence instantiate 'a' by ()
data PolyResult a = PolyResult SharedModel (Board 'Core) a (Board 'UI)
  deriving (Eq, Show)

-- | The result of playing an 'Event': an updated board, the next event
-- (if any), and the animations
type Result = PolyResult (Maybe Event)

data MessageText
  = -- | Simple text to display
    Text Text.Text
  | -- | Constructor to display an image. The image should be fine
    -- for passing to 'assetsPath'
    Image Tile.Filepath
  deriving (Eq, Generic)

-- | An animation that makes sense at the 'Game' level. If you
-- consider extending this variant, consider whether it would be
-- be better in @Board 'UI@
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

changeToEffect :: StatChange -> InPlaceEffect
changeToEffect StatChange {attackDiff, hpDiff} =
  mempty {attackChange = natToInt attackDiff, hitPointsChange = natToInt hpDiff}

apply :: StatChange -> Creature p -> Creature p
apply StatChange {attackDiff, hpDiff} c@Creature {attack, hp} =
  c {Card.attack = attack +^ attackDiff, hp = hp + hpDiff}

reportEffect ::
  MonadWriter (Board 'UI) m =>
  Spots.Player ->
  Spots.Card ->
  InPlaceEffect ->
  m ()
reportEffect pSpot cSpot effect =
  tell $ Board {playerTop = pTop, playerBottom = pBot}
  where
    effectfull = Map.singleton cSpot effect
    effectless = mempty
    (botInPlace, topInPlace) =
      case pSpot of
        PlayerBot -> (effectfull, effectless)
        PlayerTop -> (effectless, effectfull)
    pTop :: PlayerPart 'UI = mempty {inPlace = topInPlace}
    pBot :: PlayerPart 'UI = mempty {inPlace = botInPlace}

-- | Play a single 'Event' on the given 'Board'
play :: SharedModel -> Board 'Core -> Event -> Either Text Result
play shared board action =
  playM board action
    & runWriterT
    & flip runStateT shared
    & runExcept
    & fmap mkResult
  where
    mkResult (((b, e), bui), s) = PolyResult s b e bui

-- | Please avoid calling this function if you can. Its effect is
-- difficult to predict because of cards that create events, as these
-- events are played right away (instead of being enqueued in the main loop
-- like when playing Infernal Haste interactively). FIXME @smelc
-- take an additional parameter telling whether to mimic the main loop
-- (for Game.Attack events).
playAll :: SharedModel -> Board 'Core -> [Event] -> Either Text (PolyResult ())
playAll shared board es = go board mempty es
  where
    go b anims [] = Right $ PolyResult shared b () anims
    go b anims (e : tl) = do
      PolyResult _ board' new anims' <- play shared b e
      -- /!\ We're enqueueing the event created by playing 'e' i.e. 'new',
      -- before the rest of the events ('tl'). This means 'tl' will be played in a state
      -- that is NOT the one planned when building 'e : tl' (except if the
      -- caller is very smart). We have no way around that: this is caused
      -- by Infernal Haste that creates events when playing it. But this means
      -- we shouldn't be too hard with events that yield failures. Maybe
      -- they are being played in an unexpected context (imagine if
      -- Infernal Haste clears the opponent's board, next spells may be useless).
      go board' (anims <> anims') $ maybeToList new ++ tl

-- | 'keepEffectfull board es' returns the elements of 'es'
-- that have an effect. Elements of 'es' are played in sequence.
keepEffectfull :: SharedModel -> Board 'Core -> [Event] -> [Event]
keepEffectfull _ _ [] = []
keepEffectfull shared board (e : es) =
  case play shared board e of
    Left err -> traceShow err (keepEffectfull shared board es)
    Right (PolyResult shared' board' new _) ->
      case playAll shared' board' (maybeToList new) of
        Left err -> traceShow err (keepEffectfull shared' board' es)
        Right (PolyResult shared'' board'' () _) ->
          if board'' == board
            then {- 'e' had no effect -} keepEffectfull shared board es
            else {- 'e' had an effect -} e : keepEffectfull shared'' board'' es

playM ::
  MonadError Text m =>
  MonadWriter (Board 'UI) m =>
  MonadState SharedModel m =>
  Board 'Core ->
  Event ->
  m (Board 'Core, Maybe Event)
playM board (ApplyChurch pSpot) = do
  board' <- Game.applyChurchM board pSpot
  return (board', Nothing)
playM board (ApplyFearNTerror pSpot) = do
  board' <- Game.applyFearNTerrorM board pSpot
  return (board', Nothing)
playM board (ApplyKing pSpot) = do
  board' <- Game.applyKingM board pSpot
  return (board', Nothing)
playM board (Attack pSpot cSpot _ _) = do
  board' <- Game.attack board pSpot cSpot
  return (board', Nothing)
playM board (FillTheFrontline pSpot) = do
  return (applyFillTheFrontline board pSpot, Nothing)
playM board NoPlayEvent = return (board, Nothing)
playM board (Place pSpot target (handhi :: HandIndex)) = do
  shared <- get
  ident <- lookupHand hand handi
  let uiCard = SharedModel.identToCard shared ident
  let card = unlift <$> uiCard
  case (target, card, uiCard <&> Card.toCommon) of
    (_, Nothing, _) ->
      throwError $ Text.pack $ "ident not found: " ++ show ident
    (_, _, Nothing) ->
      throwError $ Text.pack $ "Unexpected state, CardCommon should be there if Card is there"
    (_, _, Just (CardCommon {mana = manaCost}))
      | manaCost > manaAvail ->
        throwError $ Text.pack $ "Cannot play " ++ show card ++ ": mana available is " ++ show manaAvail ++ ", whereas card costs " ++ show manaCost ++ " mana"
    (CardTarget pSpot cSpot, Just (CreatureCard _ creature), Just CardCommon {mana}) -> do
      board'' <- playCreatureM board' pSpot cSpot creature
      return (board'' & decreaseMana mana, Nothing)
    (CardTarget pSpot cSpot, Just (ItemCard _ itemObj), Just CardCommon {mana}) -> do
      board'' <- playItemM board' pSpot cSpot $ Card.item itemObj
      return (board'' & decreaseMana mana, Nothing)
    (_, Just (NeutralCard _ NeutralObject {neutral}), Just CardCommon {mana}) -> do
      (board'', event) <- playNeutralM board' pSpot target neutral
      return (board'' & decreaseMana mana, event)
    _ ->
      throwError $ Text.pack $ "Wrong (Target, card) combination: (" ++ show target ++ ", " ++ show card ++ ")"
  where
    handi = unHandIndex handhi
    (hand, hand') = (Board.toHand board pSpot, deleteAt handi hand)
    board' = Board.setHand board pSpot hand'
    manaAvail :: Nat = Board.toPart board pSpot & Board.mana
    decreaseMana manaCost (b :: Board 'Core) = Board.setMana (manaAvail - manaCost) pSpot b
playM board (Place' pSpot target id) =
  case idToHandIndex board pSpot id of
    Nothing -> throwError $ Text.pack $ "Card not found in " ++ show pSpot ++ ": " ++ show id
    Just i -> playM board (Place pSpot target i)

-- | Translates an 'Event' into an animation displayed in the
-- middle of the 'Board'.
eventToAnim :: SharedModel -> Board 'Core -> Event -> Animation
eventToAnim shared board =
  -- Note that, in this function, we do not need to check if the
  -- Event has an effect. This is done by the caller of 'keepEffectfull'
  \case
    Game.ApplyChurch pSpot ->
      case (Board.mana part /= Board.mana part', hps < hps', attack < attack') of
        (True, _, _) -> fill [Text "+1 mana"]
        (_, True, _) -> fill [Text "adds +1", Image heartTile]
        (_, _, True) -> fill [Text "adds +1", Image attackTile]
        _ -> fill [Text " has no effect without any believers"]
      where
        parts@(part, part') = (Board.toPart board pSpot, Board.toPart board' pSpot)
        board' = applyChurch shared board pSpot & (\(b, _, _) -> b)
        churchTile = SharedModel.tileToFilepath shared Tile.HumanChurch Tile.TwentyFour
        creatures :: ([Creature 'Core], [Creature 'Core]) =
          both (Map.elems . Board.inPlace) parts
        toHPs cs = sum $ map Card.hp cs
        toAttack cs = mconcat $ map Card.attack cs
        (hps :: Nat, hps' :: Nat) = both toHPs creatures
        (attack :: Damage, attack' :: Damage) = both toAttack creatures
        both f = Bifunctor.bimap f f
        fill suffix = Message ([Text "Church", Image churchTile] ++ suffix) duration
    Game.ApplyFearNTerror {} -> NoAnimation
    Game.ApplyKing {} ->
      Message
        [ Text "Hail to the king: +1 ",
          Image heartTile,
          Text " +1",
          Image attackTile,
          Text " to all knights"
        ]
        duration
    Game.Attack {} -> NoAnimation
    Game.FillTheFrontline pSpot ->
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
            & map (SharedModel.mlift shared)
            & catMaybes
        movedTiles :: [Tile.Filepath] =
          map (SharedModel.creatureToFilepath shared) movedCreatures
            & catMaybes
    Game.NoPlayEvent -> NoAnimation
    Game.Place pSpot target (HandIndex {unHandIndex = idx}) ->
      case runExcept (lookupHand (Board.toHand board pSpot) idx) of
        Left msg -> error (Text.unpack msg)
        Right id -> go pSpot target id
    Game.Place' pSpot target id ->
      go pSpot target id
  where
    duration = 2
    attackTile = SharedModel.tileToFilepath shared Tile.Sword1 Tile.Sixteen
    heartTile = SharedModel.tileToFilepath shared Tile.Heart Tile.Sixteen
    go pSpot target =
      \case
        IDC {} -> NoAnimation
        IDI {} -> NoAnimation -- We should rather highlight the new item in Board 'UI
        id@(IDN _) ->
          Game.Application pSpot target (SharedModel.unsafeIdentToCard shared id & Card.unlift)

-- | The index of the card with this 'Card.ID', in the hand of the
-- player at the given spot
idToHandIndex :: Board 'Core -> Spots.Player -> Card.ID -> Maybe HandIndex
idToHandIndex board pSpot id =
  find
    (\(_, m) -> m == id)
    (Board.toHand board pSpot & zip [HandIndex 0 ..])
    <&> fst

-- | Play a 'Neutral'. Doesn't deal with consuming mana (done by caller)
playNeutralM ::
  MonadError Text m =>
  MonadWriter (Board 'UI) m =>
  MonadState SharedModel m =>
  Board 'Core ->
  Spots.Player ->
  Target ->
  Neutral ->
  m (Board 'Core, Maybe Event)
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
      reportEffect pSpot cSpot (mempty {hitPointsChange = increase})
      board' <- addHitpoints pSpot cSpot increase
      return (board', Nothing)
    (Life, CardTarget pSpot cSpot) -> do
      let increase = 3
      reportEffect pSpot cSpot (mempty {hitPointsChange = increase})
      board' <- addHitpoints pSpot cSpot increase
      return (board', Nothing)
    (Pandemonium, PlayerTarget pSpot) -> do
      -- Take existing creatures
      let creatures = Board.toInPlace board pSpot & Map.elems
      -- Generate random spots
      spots <- SharedModel.shuffleM Spots.allCards
      -- Put existing creatures on new spots
      let board' = Board.setInPlace board pSpot (Map.fromList $ zip spots creatures)
      -- Report fadeIn effects on each new spot
      traverse_ (\cSpot -> reportEffect pSpot cSpot (mempty {fadeIn = True})) spots
      return (board', Nothing)
    (Plague, PlayerTarget pSpot) -> do
      board' <- applyPlagueM board pSpot
      return (board', Nothing)
    (StrengthPot, CardTarget pSpot cSpot) ->
      case Board.toInPlaceCreature board pSpot cSpot of
        Nothing -> return (board, Nothing)
        Just c@Creature {skills} -> do
          reportEffect pSpot cSpot (mempty {attackChange = Nat.natToInt Constants.strengthPotAttackBonus})
          let board' = Board.setCreature board pSpot cSpot (c {skills = skills ++ [Skill.StrengthPot]})
          return (board', Nothing)
    _ -> throwError $ Text.pack $ "Wrong (Target, Neutral) combination: (" ++ show target ++ ", " ++ show n ++ ")"
  where
    addHitpoints pSpot cSpot hps =
      case Board.toInPlaceCreature board pSpot cSpot of
        Nothing -> return board
        Just c@Creature {hp} ->
          return board'
          where
            c' = c {hp = hp + hps}
            board' = Board.setCreature board pSpot cSpot c'

-- | Play a 'Creature'. Doesn't deal with consuming mana (done by caller)
playCreatureM ::
  MonadWriter (Board 'UI) m =>
  Board 'Core ->
  Spots.Player ->
  Spots.Card ->
  Creature 'Core ->
  m (Board 'Core)
playCreatureM board pSpot cSpot creature =
  case Map.member cSpot $ Board.toInPlace board pSpot of
    True ->
      -- This used to be an error, but now this can happen with
      -- Infernal Haste + Flail of the Damned. Haste makes
      -- the flail spawn an unexpected creature, which may make
      -- an event computed by the AI fail.
      return $
        traceShow
          ("[WARN] Cannot place card on non-empty spot: " <> Text.pack (show cSpot))
          board
    _ -> do
      reportEffect pSpot cSpot $ mempty {fadeIn = True} -- report creature addition effect
      board <- pure $ Board.setCreature board pSpot cSpot creature -- set creature
      board <- applyDiscipline board creature pSpot cSpot
      board <- applySquire board creature pSpot cSpot
      return board

-- | Play an 'Item'. Doesn't deal with consuming mana (done by caller)
playItemM ::
  MonadError Text m =>
  MonadWriter (Board 'UI) m =>
  Board 'Core ->
  Spots.Player ->
  Spots.Card ->
  Item ->
  m (Board 'Core)
playItemM board pSpot cSpot item =
  case inPlace Map.!? cSpot of
    Nothing ->
      throwError $ "Cannot place item on empty spot: " <> Text.pack (show item) <> " at " <> Text.pack (show cSpot ++ " " ++ show pSpot)
    Just creature -> do
      reportEffect pSpot cSpot $ mempty {fadeIn = True}
      -- TODO @smelc record animation for item arrival
      let creature' = installItem creature item
      return $ Board.setPart board pSpot $ part' creature'
  where
    part@PlayerPart {inPlace} = Board.toPart board pSpot
    part' c' = part {inPlace = Map.insert cSpot c' inPlace}

installItem ::
  Creature 'Core ->
  Item ->
  Creature 'Core
installItem c@Creature {hp, items} item =
  c {hp = hp + hpChange, items = item : items}
  where
    -- Items adding health are resolved here, as opposed to items
    -- adding attack, which are dealt with in 'Total'
    hpChange =
      case item of
        SwordOfMight -> 1
        _ -> 0 -- wildcard intentional

applyDiscipline ::
  MonadWriter (Board 'UI) m =>
  -- | The input board
  Board 'Core ->
  -- | The creature being played
  Creature 'Core ->
  -- | The part where the creature is being played
  Spots.Player ->
  -- | The spot where the creature arrives. It has already been filled with 'creature'.
  Spots.Card ->
  m (Board 'Core)
applyDiscipline board creature pSpot cSpot =
  if not $ Total.isDisciplined creature
    then return board -- No change
    else do
      traverse_ ((\dSpot -> reportEffect pSpot dSpot effect)) disciplinedNeighbors
      return $ Board.mapInPlace (apply change) pSpot disciplinedNeighbors board
  where
    change = StatChange {attackDiff = 1, hpDiff = 1}
    effect = changeToEffect change
    disciplinedNeighbors :: [Spots.Card] =
      Board.toNeighbors board pSpot cSpot Board.Cardinal
        & filter (\(_, c) -> Total.isDisciplined c)
        & filter (\(spot, _) -> spot /= cSpot)
        & map fst

applyFillTheFrontline ::
  Board 'Core ->
  Spots.Player ->
  Board 'Core
applyFillTheFrontline board pSpot =
  Board.setPart board pSpot part {inPlace = Map.fromList bindings'}
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
  Board 'Core ->
  -- | The part on which to apply plague
  Spots.Player ->
  Board 'Core
applyPlague board actingPlayer = applyPlagueM board actingPlayer & runWriter & fst

applyPlagueM ::
  MonadWriter (Board 'UI) m =>
  -- | The input board
  Board 'Core ->
  -- | The part on which to apply plague
  Spots.Player ->
  m (Board 'Core)
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
    baseEffect = mempty {hitPointsChange = -1}
    plagueEffect Creature {hp} | hp <= 1 = baseEffect {death = UsualDeath}
    plagueEffect _ = baseEffect {fadeOut = [Tile.HeartBroken]}

applySquire ::
  MonadWriter (Board 'UI) m =>
  -- | The input board
  Board 'Core ->
  -- | The creature being played
  Creature 'Core ->
  -- | The part where the creature is being played
  Spots.Player ->
  -- | The spot where the creature arrives. It has already been filled with the creature.
  Spots.Card ->
  m (Board 'Core)
applySquire board Creature {skills} pSpot cSpot =
  if Skill.Squire `notElem` skills || Spots.inFront cSpot || not knightInFront
    then return board -- No change
    else do
      reportEffect pSpot frontSpot effect
      return $ Board.mapInPlace (apply change) pSpot [frontSpot] board
  where
    change :: StatChange = mempty {hpDiff = 1}
    effect = changeToEffect change
    frontSpot = Board.switchLine cSpot
    knightInFront =
      Board.toInPlaceCreature board pSpot frontSpot
        <&> (\Creature {skills = skills'} -> Skill.Knight `elem` skills')
        & fromMaybe False

drawCards ::
  SharedModel ->
  Board 'Core ->
  -- | The player drawing cards
  Spots.Player ->
  -- | The sources from which to draw the cards
  [DrawSource] ->
  Either Text (SharedModel, Board 'Core, Board 'UI)
drawCards shared board _ [] = return (shared, board, mempty)
drawCards shared board pSpot (hd : rest) = do
  (board', boardui, shared') <- drawCard shared board pSpot hd
  (shared'', board'', boardui') <- drawCards shared' board' pSpot rest
  return (shared'', board'', boardui <> boardui')

drawCard ::
  SharedModel ->
  Board 'Core ->
  -- | The player drawing cards
  Spots.Player ->
  -- | The reason for drawing a card
  DrawSource ->
  Either Text (Board 'Core, Board 'UI, SharedModel)
drawCard shared board pSpot src =
  drawCardM board pSpot src
    & runWriterT
    & flip runStateT shared
    & runExcept
    & fmap flatten
  where
    flatten ((x, y), z) = (x, y, z)

drawCardM ::
  MonadError Text m =>
  MonadWriter (Board 'UI) m =>
  MonadState SharedModel m =>
  Board 'Core ->
  Spots.Player ->
  DrawSource ->
  m (Board 'Core)
drawCardM board pSpot src =
  case (srcKind board, stack) of
    (Left _msg, _) -> return board -- cannot draw: 'src' is invalid
    (_, []) -> return board -- cannot draw: stack is empty
    (Right witness, _) -> do
      let hand = Board.toHand board pSpot
      stdgen <- use #sharedStdGen
      let (idrawn, stdgen') = randomR (0, assert (stackLen >= 1) $ stackLen - 1) stdgen
      #sharedStdGen .= stdgen'
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
    srcKind b =
      case src of
        Native -> Right Nothing
        CardDrawer pSpot' cSpot | pSpot' == pSpot ->
          case Board.toInPlaceCreature b pSpot cSpot of
            Nothing -> Left ("No creature at pSpot cSpot" :: Text)
            Just c@Creature {skills} ->
              case findIndex (\case Skill.DrawCard b -> b; _ -> False) skills of
                Nothing -> Left "Not a creature with avail DrawCard"
                Just i -> Right $ Just (cSpot, c, i, Skill.DrawCard False)
        CardDrawer _ _ -> Left "Wrong Spots.Player"
    consumeSrc b Nothing = b -- No change
    consumeSrc b (Just (cSpot, c@Creature {..}, skilli, skill')) =
      -- Set new skill
      Board.setCreature b pSpot cSpot $ c {skills = setAt skilli skill' skills}

transferCards ::
  SharedModel ->
  Board 'Core ->
  Spots.Player ->
  (SharedModel, Board 'Core, Board 'UI)
transferCards shared board pSpot =
  (SharedModel.withStdGen shared stdgen', board', boardui')
  where
    (board', boardui', stdgen') =
      transferCards' (SharedModel.getStdGen shared) board pSpot

transferCards' ::
  StdGen ->
  Board 'Core ->
  Spots.Player ->
  (Board 'Core, Board 'UI, StdGen)
transferCards' stdgen board pSpot =
  transferCardsM board pSpot & flip runRandT stdgen & runWriter & reorg
  where
    reorg ((b, s), bui) = (b, bui, s)

-- | Transfer cards from the discarded stack to the other stack
transferCardsM ::
  MonadRandom m =>
  MonadWriter (Board 'UI) m =>
  Board 'Core ->
  Spots.Player ->
  m (Board 'Core)
transferCardsM board pSpot =
  if not needTransfer
    then pure board
    else do
      tell $ Board.setDiscarded mempty pSpot (- length discarded)
      tell $ Board.setStack mempty pSpot (length discarded)
      discarded' <- shuffleM discarded
      let part' = part {discarded = [], stack = stack ++ discarded'}
      return $ Board.setPart board pSpot part'
  where
    nbCardsToDraw = cardsToDraw board pSpot False & length
    (stack, stackSize) = (Board.toStack board pSpot, length stack)
    needTransfer = nbCardsToDraw > stackSize
    discarded = Board.toDiscarded board pSpot
    part = Board.toPart board pSpot

-- | board id pSpot target holds iff player at 'pSpot' can play card 'id'
-- on 'target'
appliesTo :: Board 'Core -> Card.ID -> Spots.Player -> Target -> Bool
appliesTo board id playingPlayer target =
  correctPlayer && correctHoleyness
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

applyChurch :: SharedModel -> Board 'Core -> Spots.Player -> (Board 'Core, Board 'UI, SharedModel)
applyChurch shared board pSpot =
  applyChurchM board pSpot
    & runWriterT
    & flip runState shared
    & reorg
  where
    reorg ((x, y), z) = (x, y, z)

applyChurchM ::
  MonadWriter (Board 'UI) m =>
  MonadState SharedModel m =>
  -- | The input board
  Board 'Core ->
  -- | The part where churchs take effect
  Spots.Player ->
  m (Board 'Core)
applyChurchM board pSpot = do
  -- TODO @smelc generate one effect per church
  effect <- SharedModel.oneof allChurchEffects
  case effect of
    PlusOneAttack ->
      go (\c@Creature {attack} -> c {Card.attack = attack +^ 1}) (mempty {attackChange = 1})
    PlusOneHealth ->
      go (\c@Creature {hp} -> c {hp = hp + 1}) (mempty {attackChange = 1})
    PlusOneMana -> do
      tell (setMana 1 pSpot mempty :: Board 'UI)
      return $ Board.setMana (Board.toPart board pSpot & Board.mana) pSpot board
  where
    go creatureFun effect = do
      traverse_ (\cSpot -> reportEffect pSpot cSpot effect) (map fst others)
      return $ Board.mapInPlace creatureFun pSpot (Set.toList affectedSpots) board
    creatures = Board.toInPlace board pSpot
    (churchs :: [(Spots.Card, Creature 'Core)], others :: [(Spots.Card, Creature 'Core)]) =
      creatures & Map.filter isChurch & Map.toList & partition (isChurch . snd)
    affectedSpots :: Set.Set Spots.Card =
      (Set.fromList (map fst others)) Set.\\ (Set.fromList (map fst churchs))
    isChurch Creature {creatureId = CreatureID {creatureKind = kind}} = kind == Card.Church

applyFearNTerror ::
  -- | The input board
  Board 'Core ->
  -- | The part causing fear
  Spots.Player ->
  (Board 'Core, Board 'UI)
applyFearNTerror board pSpot =
  applyFearNTerrorM board pSpot & runWriter

applyFearNTerrorM ::
  MonadWriter (Board 'UI) m =>
  -- | The input board
  Board 'Core ->
  -- | The part causing fear
  Spots.Player ->
  m (Board 'Core)
applyFearNTerrorM board affectingSpot = do
  traverse_ (\spot -> reportEffect affectedSpot spot $ deathBy DeathByTerror) terrorAffected
  traverse_ (\spot -> reportEffect affectedSpot spot $ deathBy DeathByFear) fearAffected
  let board' = Board.setInPlace board affectedSpot affectedInPlace''
  let board'' = Board.setInPlace board' affectingSpot affectingInPlace'
  let board''' = Board.addToDiscarded board'' affectedSpot killedToDiscard
  return board'''
  where
    affectedSpot = otherPlayerSpot affectingSpot
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
    deathBy cause = mempty {death = cause}
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

-- | Applies the effect of kings, i.e. give +1/+1 to all knights, for every
-- king in place.
applyKingM ::
  MonadWriter (Board 'UI) m =>
  -- | The input board
  Board 'Core ->
  -- | The part where kings are used
  Spots.Player ->
  m (Board 'Core)
applyKingM board pSpot = do
  traverse_ ((\dSpot -> reportEffect pSpot dSpot effect)) knightSpots
  return $ Board.mapInPlace (apply change) pSpot knightSpots board
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
  MonadWriter (Board 'UI) m =>
  MonadState SharedModel m =>
  Board 'Core ->
  -- The attacker's player spot
  Spots.Player ->
  -- The attacker's card spot
  Spots.Card ->
  m (Board 'Core)
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
          occupiedSlots <- SharedModel.shuffleM occupiedSlots <&> safeHead
          case occupiedSlots of
            Nothing -> return board -- Nothing to attack. XXX @smelc record question mark animation
            Just (attackedSpot, attacked) -> do
              attackOneSpot board (hitter, pSpot, cSpot) (attacked, attackedSpot)
        Imprecise -> do
          -- Attacker has the Imprecise skill. Attack a spot at random.
          -- Never contribute to score.
          attackedSpot :: Spots.Card <- SharedModel.oneof Spots.allCardsNE
          case Board.toInPlaceCreature board attackeePSpot attackedSpot of
            Nothing -> do
              -- Imprecise creature targets an empty spot, report attack
              -- and exposion.
              reportEffect pSpot cSpot $ mempty {attackBump = True}
              reportEffect attackeePSpot attackedSpot $ mempty {fadeOut = [Tile.Explosion]}
              pure board -- No change
            Just attacked ->
              -- Imprecise creature attacks an occupied spot
              attackOneSpot board (hitter, pSpot, cSpot) (attacked, attackedSpot)
        Spots [] -> do
          -- Cannot attack: attack is 0 or fighter is in the back. XXX @smelc record an animation?
          return board
        Spots attackedSpots -> do
          -- nothing to attack, contribute to the score!
          let attackedCreatures :: [(Creature 'Core, Spots.Card)] = spotsToEnemies attackedSpots
          if null attackedCreatures
            then do
              -- Creature can attack an enemy spot, but it is empty: contributed to the score
              let place = Total.Place {place = Board.toInPlace board pSpot, cardSpot = cSpot}
              hit :: Nat <- deal $ Total.attack (Just place) hitter
              reportEffect pSpot cSpot $ mempty {attackBump = True, scoreChange = natToInt hit}
              return $ Board.increaseScore board pSpot hit
            else do
              -- or something to attack; attack it
              foldM
                (\b attackee -> attackOneSpot b (hitter, pSpot, cSpot) attackee)
                board
                attackedCreatures
  where
    safeHead = \case [] -> Nothing; x : _ -> Just x
    attackeePSpot = otherPlayerSpot pSpot
    attackersInPlace :: Map Spots.Card (Creature 'Core) = Board.toInPlace board pSpot
    attackeesInPlace :: Map Spots.Card (Creature 'Core) = Board.toInPlace board attackeePSpot
    attacker :: Maybe (Creature 'Core) = attackersInPlace !? cSpot
    attackerSkills :: [Skill] = (Card.to attacker) & map Skill.lift
    allyBlocker :: Maybe (Creature 'Core) =
      if any (`elem` attackerSkills) [Skill.Imprecise, Skill.LongReach, Skill.Ranged]
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
  MonadWriter (Board 'UI) m =>
  MonadState SharedModel m =>
  Board 'Core ->
  (Creature 'Core, Spots.Player, Spots.Card) ->
  (Creature 'Core, Spots.Card) ->
  m (Board 'Core)
attackOneSpot board (hitter, pSpot, cSpot) (hit, hitSpot) = do
  effect@InPlaceEffect {extra} <- singleAttack place hitter hit
  let board' = applyInPlaceEffectOnBoard effect board (hitPspot, hitSpot, hit)
  reportEffect pSpot cSpot $ mempty {attackBump = True} -- hitter
  reportEffect hitPspot hitSpot effect -- hittee
  board' <-
    if (isDead . death) effect
      then applyFlailOfTheDamned board' hitter pSpot
      else pure board'
  return $ fPowerful board' extra
  where
    hitPspot = otherPlayerSpot pSpot
    place = Total.Place {place = Board.toInPlace board pSpot, cardSpot = cSpot}
    fPowerful b extra
      | Total.isPowerful hitter = Board.mapScore b pSpot ((+) extra)
      | otherwise = b

applyInPlaceEffectOnBoard ::
  -- | The effect of the attacker on the hittee
  InPlaceEffect ->
  -- | The input board
  Board 'Core ->
  -- | The creature being hit
  (Spots.Player, Spots.Card, Creature 'Core) ->
  -- | The updated board
  Board 'Core
applyInPlaceEffectOnBoard effect board (pSpot, cSpot, hittee@Creature {creatureId, items}) =
  case hittee' of
    Just _ -> board'
    Nothing | Card.transient hittee -> board' -- Dont' put hittee in discarded stack
    Nothing ->
      let discarded = Board.toDiscarded board' pSpot
       in -- TODO @smelc use Board.addToDiscarded instead
          Board.setDiscarded board' pSpot $ discarded ++ [IDC creatureId items]
  where
    hittee' = applyInPlaceEffect effect hittee
    -- Update the hittee in the board, putting Nothing or Just _:
    board' = board & spotToLens pSpot . #inPlace . at cSpot .~ hittee'

applyInPlaceEffect ::
  -- | The effect of the attacker on the hittee
  InPlaceEffect ->
  -- | The creature being hit
  Creature 'Core ->
  -- | The creature being hit, after applying the effect; or None if dead
  Maybe (Creature 'Core)
applyInPlaceEffect effect creature@Creature {..} =
  case effect of
    InPlaceEffect {death} | isDead death -> Nothing
    InPlaceEffect {hitPointsChange = i} -> Just $ creature {hp = intToClampedNat (natToInt hp + i)}

applyFlailOfTheDamned ::
  MonadWriter (Board 'UI) m =>
  MonadState SharedModel m =>
  -- The input board
  Board 'Core ->
  -- The hitter
  Creature 'Core ->
  -- The hitter's position
  Spots.Player ->
  m (Board 'Core)
applyFlailOfTheDamned board creature pSpot =
  if not hasFlailOfTheDamned
    then return noChange
    else do
      shared <- get
      let spots =
            Board.toPlayerHoleyInPlace board pSpot
              & filter (isNothing . snd)
              & map fst
      let (shared', spawningSpot) = SharedModel.pick shared spots
      case spawningSpot of
        Nothing -> return noChange
        Just spawningSpot -> do
          put shared'
          let spawned =
                CreatureID Skeleton Undead
                  & (\cid -> SharedModel.idToCreature shared' cid [])
                  & fromJust
                  & Card.unlift
          let spawned' = spawned {transient = True}
          let board' = Board.setCreature board pSpot spawningSpot spawned'
          -- TODO @smelc record an animation highlighting the flail
          reportEffect pSpot spawningSpot $ mempty {fadeIn = True}
          return board'
  where
    hasFlailOfTheDamned = Card.items creature & elem FlailOfTheDamned
    noChange = board

-- | The effect of an attack on the defender. Note that this function
-- cannot return a 'StatChange'. It needs the full expressivity of 'InPlaceEffect'.
singleAttack ::
  MonadState SharedModel m =>
  p ~ 'Core =>
  Total.Place ->
  -- | The attacker
  Creature p ->
  -- | The defender
  Creature p ->
  m InPlaceEffect
singleAttack place attacker@Creature {skills} defender = do
  hit :: Nat <- deal damage
  let afterHit :: Int = (natToInt (Card.hp defender)) - (natToInt hit)
  let extra :: Nat = if afterHit < 0 then Nat.intToNat (abs afterHit) else 0
  let hps' :: Nat = Nat.intToClampedNat afterHit
  let hpChangeEffect =
        if hps' <= 0
          then mempty {death}
          else mempty {hitPointsChange = Nat.negate hit}
  return $ (hpChangeEffect {extra}) <> ace <> imprecise
  where
    damage = Total.attack (Just place) attacker
    death =
      if any (\skill -> case skill of Skill.BreathIce -> True; _ -> False) skills
        then DeathByBreathIce
        else UsualDeath
    ace = mkFadeOut Skill.Ace Tile.Arrow
    imprecise = mkFadeOut Skill.Imprecise Tile.Explosion
    mkFadeOut skill tile =
      if skill `elem` skills
        then mempty {fadeOut = [tile]}
        else mempty

-- | Apply a 'Damage'
deal :: MonadState SharedModel m => Damage -> m Nat
deal Damage {base, variance}
  | variance == 0 = return base -- No need to roll a dice
  | otherwise = SharedModel.roll base (base + variance)

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
  map bottomSpotOfTopVisual spotsInSight
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
      Spots $
        if
            | Skill.Ranged `elem` skills -> spotsInSight
            | inTheBack cSpot -> if Skill.LongReach `elem` skills then take 1 spotsInSight else []
            | Skill.BreathIce `elem` skills -> assert (not $ inTheBack cSpot) spotsInSight
            | otherwise -> take 1 spotsInSight
      where
        spotsInSight = allEnemySpots cSpot

-- | The order in which cards attack
attackOrder :: Spots.Player -> [Spots.Card]
attackOrder PlayerTop =
  [BottomRight, Bottom, BottomLeft, TopRight, Top, TopLeft]
attackOrder PlayerBot =
  map bottomSpotOfTopVisual $ reverse $ attackOrder PlayerTop

-- | @nextAttackSpot b pSpot cSpot@ returns the spots to attack after @cSpot@.
-- If @cSpot@ is @Nothing@, the first spot in the order is considered; if
-- @cSpot@ is @Just _@, then spots after @cSpot@ are considered.
nextAttackSpot :: Board 'Core -> Spots.Player -> Maybe Spots.Card -> Maybe Spots.Card
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
    hasCreature c = isJust $ Board.toInPlaceCreature board pSpot c

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
cardsToDraw :: Board 'Core -> Spots.Player -> Bool -> [DrawSource]
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
