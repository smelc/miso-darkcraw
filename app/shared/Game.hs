{-# LANGUAGE DataKinds #-}
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
    applyFearNTerror,
    attackOrder, -- exported for tests only
    nextAttackSpot,
    enemySpots,
    DrawSource (..),
    Event (..),
    PolyResult (..),
    Result (..),
    placePrimeToHandIndex,
    play,
    playAll,
    playM,
    cardsToDraw,
    drawCards,
    transferCards,
    Target (..),
    Game.appliesTo,
    whichPlayerTarget,
    WhichPlayerTarget (..),
  )
where

import Board
import BoardInstances
import Card hiding (ID, isStupid)
import qualified Card
import qualified Constants
import Control.Exception (assert)
import Control.Lens
import Control.Monad.Except
import Control.Monad.Random
import Control.Monad.State
import Control.Monad.Writer
import qualified Data.Bifunctor as Bifunctor
import Data.Foldable
import Data.List
import Data.List.Index (deleteAt, setAt)
import Data.Map.Strict (Map, (!?))
import qualified Data.Map.Strict as Map
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as Text
import Debug.Trace
import GHC.Generics (Generic)
import SharedModel (SharedModel, unsafeIdentToCard)
import qualified SharedModel
import System.Random.Shuffle (shuffleM)
import qualified Total

-- | On what a card can be applied
data Target
  = -- | Neutral card applies to all in place cards of a player
    PlayerTarget PlayerSpot
  | -- | Creature card placed at given spot
    -- or Neutral card applies to a given in place card of a player
    CardTarget PlayerSpot CardSpot
  deriving (Eq, Generic, Show)

targetToPlayerSpot :: Target -> PlayerSpot
targetToPlayerSpot (PlayerTarget pSpot) = pSpot
targetToPlayerSpot (CardTarget pSpot _) = pSpot

-- | Whether a card makes sense on the playing player
-- or the  opponent. We could even try both, but we don't do that
-- for now
data WhichPlayerTarget = Playing | Opponent

whichPlayerTarget :: Card.ID -> WhichPlayerTarget
whichPlayerTarget = \case
  IDC {} -> Playing
  IDI _ -> Playing
  IDN Health -> Playing
  IDN InfernalHaste -> Playing
  IDN Life -> Playing

data Event
  = -- | Apply fear of the creatures at the given 'PlayerSpot'
    ApplyFearNTerror PlayerSpot
  | -- | A card attacks at the given spot. The first Boolean indicates
    -- whether the next spot (as defined by 'nextAttackSpot') should
    -- be enqueued after solving this attack. The second Boolean indicates
    -- whether 'GameIncrTurn' (change player turn) should be performed
    -- after solving this attack.
    Attack PlayerSpot CardSpot Bool Bool
  | -- | A Nothing case, for convenience
    NoPlayEvent
  | -- | Player puts a card from his hand on its part of the board
    Place Target HandIndex
  | -- | AI puts a card from his hand. This constructor has better
    -- testing behavior than 'Place': it makes the generated events commute.
    Place' Target Card.ID
  deriving (Eq, Generic, Show)

-- | The polymorphic version of 'Result'. Used for implementors that
-- do not return events and hence instantiate 'a' by ()
data PolyResult a = Result SharedModel (Board Core) a (Board UI)
  deriving (Eq, Show)

-- | The result of playing an 'Event': an updated board, the next event
-- (if any), and the animations
type Result = PolyResult (Maybe Event)

reportEffect ::
  MonadWriter (Board UI) m =>
  PlayerSpot ->
  CardSpot ->
  InPlaceEffect ->
  m ()
reportEffect pSpot cSpot effect =
  tell $ Board {playerTop = pTop, playerBottom = pBot}
  where
    effectfull = InPlaceEffects $ Map.singleton cSpot effect
    effectless = InPlaceEffects Map.empty
    (botInPlace, topInPlace) =
      case pSpot of
        PlayerBot -> (effectfull, effectless)
        PlayerTop -> (effectless, effectfull)
    pTop :: PlayerPart UI = mempty {inPlace = topInPlace}
    pBot :: PlayerPart UI = mempty {inPlace = botInPlace}

play :: SharedModel -> Board Core -> Event -> Either Text Result
play shared board action =
  playM board action
    & runWriterT
    & flip runStateT shared
    & runExcept
    & fmap mkResult
  where
    mkResult (((b, e), bui), s) = Result s b e bui

-- | Please avoid calling this function if you can. Its effect is
-- difficult to predict because of cards that create events, as these
-- events are played right away (instead of being enqueued in the main loop
-- like when playing Infernal Haste interactively). FIXME @smelc
-- take an additional parameter telling whether to mimic the main loop
-- (for Game.Attack events).
playAll :: SharedModel -> Board Core -> [Event] -> Either Text (PolyResult ())
playAll shared board es = go board mempty es
  where
    go b anims [] = Right $ Result shared b () anims
    go b anims (e : tl) = do
      Result undefined board' new anims' <- play shared b e
      -- /!\ We're enqueueing the event created by playing 'e' i.e. 'new',
      -- before the rest of the events ('tl'). This means 'tl' will be played in a state
      -- that is NOT the one planned when building 'e : tl' (except if the
      -- caller is very smart). We have no way around that: this is caused
      -- by Infernal Haste that creates events when playing it. But this means
      -- we shouldn't be too hard with events that yield failures. Maybe
      -- they are being played in an unexpected context (imagine if
      -- Infernal Haste clears the opponent's board, next spells may be useless).
      go board' (anims <> anims') $ maybeToList new ++ tl

playM ::
  MonadError Text m =>
  MonadWriter (Board UI) m =>
  MonadState SharedModel m =>
  Board Core ->
  Event ->
  m (Board Core, Maybe Event)
playM board (ApplyFearNTerror pSpot) = do
  board' <- Game.applyFearNTerrorM board pSpot
  return (board', Nothing)
playM board (Attack pSpot cSpot _ _) = do
  board' <- Game.attack board pSpot cSpot
  return (board', Nothing)
playM board NoPlayEvent = return (board, Nothing)
playM board (Place target (handhi :: HandIndex)) = do
  shared <- get
  ident <- lookupHand hand handi
  let card = unsafeIdentToCard shared ident & unliftCard
  case (target, card) of
    (CardTarget pSpot cSpot, CreatureCard creature) -> do
      board'' <- playCreatureM board' pSpot cSpot creature
      return (board'', Nothing)
    (CardTarget pSpot cSpot, ItemCard itemObj) -> do
      board'' <- playItemM board' pSpot cSpot $ Card.item itemObj
      return (board'', Nothing)
    (_, NeutralCard NeutralObject {neutral}) ->
      playPlayerTargetM board' playingPlayer target neutral
    _ -> throwError $ Text.pack $ "Wrong (Target, card) combination: (" ++ show target ++ ", " ++ show card ++ ")"
  where
    handi = unHandIndex handhi
    (hand, hand') = (boardToHand board pSpot, deleteAt handi hand)
    board' = boardSetHand board pSpot hand'
    pSpot =
      -- FIXME @smelc Change me, this will be wrong once there are neutral
      -- cards that are applied on the opponent's board. GameModel's
      -- playingPlayer should be passed instead
      case target of PlayerTarget p -> p; CardTarget p _ -> p
    playingPlayer = pSpot
playM board e@(Place' target id) =
  case placePrimeToHandIndex board e of
    Nothing -> throwError $ Text.pack $ "Card not found in " ++ show pSpot ++ ": " ++ show id
    Just i -> playM board (Place target i)
  where
    pSpot = targetToPlayerSpot target

placePrimeToHandIndex :: Board 'Core -> Event -> Maybe HandIndex
placePrimeToHandIndex board =
  \case
    Place' target id -> f target id
    _ -> Nothing
  where
    idxAndIDs pSpot =
      boardToHand board pSpot & zip [HandIndex 0 ..]
    f target id =
      find (\(_, m) -> m == id) (idxAndIDs $ targetToPlayerSpot target) <&> fst

playPlayerTargetM ::
  MonadError Text m =>
  MonadWriter (Board UI) m =>
  Board Core ->
  PlayerSpot ->
  Target ->
  Neutral ->
  m (Board Core, Maybe Event)
playPlayerTargetM board _playingPlayer target n =
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
    _ -> throwError $ Text.pack $ "Wrong (Target, Neutral) combination: (" ++ show target ++ ", " ++ show n ++ ")"
  where
    addHitpoints pSpot cSpot hps =
      case boardToInPlaceCreature board pSpot cSpot of
        Nothing -> return board
        Just c@Creature {hp} ->
          return board'
          where
            c' = c {hp = hp + hps}
            board' = boardSetCreature board pSpot cSpot c'

playCreatureM ::
  MonadWriter (Board UI) m =>
  Board Core ->
  PlayerSpot ->
  CardSpot ->
  Creature Core ->
  m (Board Core)
playCreatureM board pSpot cSpot creature =
  case Map.member cSpot inPlace of
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
      let inPlace' =
            -- Left-biased union
            Map.union
              ( if Total.isDisciplined creature
                  then Map.fromList disciplinedNeighbors'
                  else mempty
              )
              (Map.insert cSpot creature inPlace)
      let part' = part {inPlace = inPlace'}
      reportEffect pSpot cSpot $ mempty {fadeIn = True}
      when (Total.isDisciplined creature) $
        traverse_ ((\(cSpot, _) -> reportEffect pSpot cSpot disciplineEffect)) disciplinedNeighbors'
      return $ boardSetPart board pSpot part'
  where
    part@PlayerPart {inPlace} = boardToPart board pSpot
    disciplinedNeighbors =
      boardToNeighbors board pSpot cSpot Board.Cardinal
        & map liftJust
        & catMaybes
        & filter (\(_, c) -> Total.isDisciplined c)
    liftJust (f, Just s) = Just (f, s)
    liftJust _ = Nothing
    boost = 1
    applyDisciplineBoost Creature {..} = Creature {hp = hp + boost, attack = attack + boost, ..}
    disciplinedNeighbors' =
      map (Bifunctor.second applyDisciplineBoost) disciplinedNeighbors
    disciplineEffect = mempty {attackChange = boost, hitPointsChange = boost}

playItemM ::
  MonadError Text m =>
  MonadWriter (Board UI) m =>
  Board Core ->
  PlayerSpot ->
  CardSpot ->
  Item ->
  m (Board Core)
playItemM board pSpot cSpot item =
  case inPlace Map.!? cSpot of
    Nothing ->
      throwError $ "Cannot place item on empty spot: " <> Text.pack (show cSpot)
    Just creature@Creature {items} -> do
      reportEffect pSpot cSpot $ mempty {fadeIn = True}
      -- TODO @smelc record animation for item arrival
      let creature' = installItem creature item
      return $ boardSetPart board pSpot $ part' creature'
  where
    part@PlayerPart {inPlace} = boardToPart board pSpot
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

drawCards ::
  SharedModel ->
  Board Core ->
  -- | The player drawing cards
  PlayerSpot ->
  -- | The sources from which to draw the cards
  [DrawSource] ->
  Either Text (SharedModel, Board Core, Board UI)
drawCards shared board _ [] = return (shared, board, mempty)
drawCards shared board pSpot (hd : rest) = do
  (board', boardui, shared') <- drawCard shared board pSpot hd
  (shared'', board'', boardui') <- drawCards shared' board' pSpot rest
  return (shared'', board'', boardui <> boardui')

drawCard ::
  SharedModel ->
  Board Core ->
  -- | The player drawing cards
  PlayerSpot ->
  -- | The reason for drawing a card
  DrawSource ->
  Either Text (Board Core, Board UI, SharedModel)
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
  MonadWriter (Board UI) m =>
  MonadState SharedModel m =>
  Board Core ->
  PlayerSpot ->
  DrawSource ->
  m (Board Core)
drawCardM board pSpot src =
  case (srcKind board, stack) of
    (Left msg, _) -> return board -- cannot draw: 'src' is invalid
    (_, []) -> return board -- cannot draw: stack is empty
    (Right witness, _) -> do
      let hand = boardToHand board pSpot
      stdgen <- use #sharedStdGen
      let (idrawn, stdgen') = randomR (0, assert (stackLen >= 1) $ stackLen - 1) stdgen
      #sharedStdGen .= stdgen'
      let ident :: Card.ID = stack !! idrawn
      let stack' = deleteAt idrawn stack
      let hand' = hand ++ [ident]
      tell $ boardAddToHand mempty pSpot $ length hand
      let board' = boardSetStack board pSpot stack'
      let board'' = boardSetHand board' pSpot hand'
      let board''' = consumeSrc board'' witness
      return board'''
  where
    (stack, stackLen) = (boardToStack board pSpot, length stack)
    srcKind b =
      case src of
        Native -> Right Nothing
        CardDrawer pSpot' cSpot | pSpot' == pSpot ->
          case boardToInPlaceCreature b pSpot cSpot of
            Nothing -> Left "No creature at pSpot cSpot"
            Just c@Creature {skills} ->
              case findIndex (\case DrawCard' b -> b; _ -> False) skills of
                Nothing -> Left "Not a creature with avail DrawCard'"
                Just i -> Right $ Just (cSpot, c, i, DrawCard' False)
        CardDrawer _ _ -> Left "Wrong PlayerSpot"
    consumeSrc b Nothing = b -- No change
    consumeSrc b (Just (cSpot, c@Creature {..}, skilli, skill')) =
      -- Set new skill
      boardSetCreature b pSpot cSpot $ c {skills = setAt skilli skill' skills}

transferCards ::
  SharedModel ->
  Board Core ->
  PlayerSpot ->
  (SharedModel, Board Core, Board UI)
transferCards shared board pSpot =
  (SharedModel.withStdGen shared stdgen', board', boardui')
  where
    (board', boardui', stdgen') =
      transferCards' (SharedModel.getStdGen shared) board pSpot

transferCards' ::
  StdGen ->
  Board Core ->
  PlayerSpot ->
  (Board Core, Board UI, StdGen)
transferCards' stdgen board pSpot =
  transferCardsM board pSpot & flip runRandT stdgen & runWriter & reorg
  where
    reorg ((b, s), bui) = (b, bui, s)

-- | Transfer cards from the discarded stack to the other stack
transferCardsM ::
  MonadRandom m =>
  MonadWriter (Board UI) m =>
  Board Core ->
  PlayerSpot ->
  m (Board Core)
transferCardsM board pSpot =
  if not needTransfer
    then pure board
    else do
      tell $ boardSetDiscarded mempty pSpot (- length discarded)
      tell $ boardSetStack mempty pSpot (length discarded)
      discarded' <- shuffleM discarded
      let part' = part {discarded = [], stack = stack ++ discarded'}
      return $ boardSetPart board pSpot part'
  where
    nbCardsToDraw = cardsToDraw board pSpot False & length
    (stack, stackSize) = (boardToStack board pSpot, length stack)
    needTransfer = nbCardsToDraw > stackSize
    discarded = boardToDiscarded board pSpot
    part = boardToPart board pSpot

-- | board id pSpot target holds iff player at 'pSpot' can play card 'id'
-- on 'target'
appliesTo :: Board Core -> Card.ID -> PlayerSpot -> Target -> Bool
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
          boardToInPlaceCreature board pSpot cSpot & isNothing
        (CardTarget pSpot cSpot, CardTargetType Occupied) ->
          boardToInPlaceCreature board pSpot cSpot & isJust
        (PlayerTarget _, PlayerTargetType) -> True
        _ -> False

applyFearNTerror ::
  -- | The input board
  Board 'Core ->
  -- | The part causing fear
  PlayerSpot ->
  (Board 'Core, Board 'UI)
applyFearNTerror board pSpot =
  applyFearNTerrorM board pSpot & runWriter

applyFearNTerrorM ::
  MonadWriter (Board 'UI) m =>
  -- | The input board
  Board 'Core ->
  -- | The part causing fear
  PlayerSpot ->
  m (Board 'Core)
applyFearNTerrorM board affectingSpot = do
  traverse_ (\spot -> reportEffect affectedSpot spot $ deathBy DeathByTerror) terrorAffected
  traverse_ (\spot -> reportEffect affectedSpot spot $ deathBy DeathByFear) fearAffected
  let board' = boardSetInPlace board affectedSpot affectedInPlace''
  let board'' = boardSetInPlace board' affectingSpot affectingInPlace'
  let board''' = boardAddToDiscarded board'' affectedSpot killedToDiscard
  return board'''
  where
    affectedSpot = otherPlayerSpot affectingSpot
    affectingInPlace = boardToInPlace board affectingSpot
    causingFear = Map.filter Total.causesFear affectingInPlace
    causingTerror = Map.filter Total.causesTerror affectingInPlace
    switch :: CardSpot -> Maybe CardSpot -- From affecting spot to affected spot, and back
    switch cSpot = enemySpots True [] cSpot & listToMaybe
    terrorAffectedSpots :: [CardSpot] = Map.keys causingTerror & mapMaybe switch
    fearAffectedSpots :: [CardSpot] =
      Map.keys causingFear
        -- & flip removeAll terrorAffectedSpots -- Terror is stronger than fear: nope, seems to turn Fear off!?
        & mapMaybe switch
    removeAll l1 l2 = [x | x <- l1, x `notElem` l2]
    affectedInPlace = boardToInPlace board affectedSpot
    fearAffected :: [CardSpot] =
      affectedInPlace
        & Map.filterWithKey
          (\spot c -> spot `elem` fearAffectedSpots && Total.affectedByFear c)
        & Map.keys
    terrorAffected :: [CardSpot] =
      affectedInPlace
        & Map.filterWithKey
          (\spot c -> spot `elem` terrorAffectedSpots && Total.affectedByTerror c)
        & Map.keys
    killedToDiscard :: [Card.ID] =
      map (boardToInPlaceCreature board affectedSpot) (terrorAffected ++ fearAffected)
        & catMaybes
        & filter (\Creature {transient} -> not transient) -- Transient creatures do not got to discarded stack
        & map (\Creature {creatureId, items} -> IDC creatureId items)
    affectedInPlace' =
      -- remove creatures killed by terror
      removeKeys affectedInPlace terrorAffected
    terrorKillers :: [CardSpot] =
      -- Affecting spots that cause a death by terror
      removeAll (Map.keys affectedInPlace') $
        Map.keys affectedInPlace
          & mapMaybe switch
    affectedInPlace'' =
      -- remove creatures killed by fear
      removeKeys affectedInPlace' fearAffected
    fearKillers :: [CardSpot] =
      -- Affecting spots that cause a death by fear
      removeAll (Map.keys affectedInPlace'') $
        Map.keys affectedInPlace'
          & mapMaybe switch
    removeKeys map [] = map
    removeKeys map (k : rest) = removeKeys (Map.delete k map) rest
    affectingInPlace' =
      -- consume skills
      Map.mapWithKey consumeTerror affectingInPlace
        & Map.mapWithKey consumeFear
    deathBy cause = InPlaceEffect 0 cause False 0 False 0
    consumeFear cSpot c | cSpot `notElem` fearKillers = c
    consumeFear cSpot c@Creature {skills} = c {skills = consumeFearSkill skills}
    consumeFearSkill [] = []
    consumeFearSkill ((Fear' True) : rest) = Fear' False : rest
    consumeFearSkill (s : rest) = s : consumeFearSkill rest
    consumeTerror cSpot c | cSpot `notElem` terrorKillers = c
    consumeTerror cSpot c@Creature {skills} = c {skills = consumeTerrorSkill skills}
    consumeTerrorSkill [] = []
    consumeTerrorSkill ((Terror' True) : rest) = Terror' False : rest
    consumeTerrorSkill (s : rest) = s : consumeTerrorSkill rest

-- | Card at [pSpot],[cSpot] attacks; causing changes to a board
attack ::
  MonadWriter (Board UI) m =>
  MonadState SharedModel m =>
  Board Core ->
  -- The attacker's player spot
  PlayerSpot ->
  -- The attacker's card spot
  CardSpot ->
  m (Board Core)
attack board pSpot cSpot =
  case (attacker, allyBlocker, attackee) of
    (Nothing, _, _) -> return board -- no attacker
    (Just c, _, _)
      | isStupid board pSpot cSpot ->
        -- TODO @smelc record an animation
        return board
    (_, Just _, _) -> return board -- an ally blocks the way
    (Just hitter, _, Just (hitSpot, hittee)) ->
      -- attack can proceed
      let effect = singleAttack hitter hittee
          board' = applyInPlaceEffectOnBoard effect board (attackeePSpot, hitSpot, hittee)
       in do
            reportEffect pSpot cSpot $ mempty {attackBump = True}
            reportEffect attackeePSpot hitSpot effect -- hittee
            if (isDead . death) effect
              then applyFlailOfTheDamned board' hitter (pSpot, cSpot)
              else pure board'
    (Just hitter, _, Nothing) -> do
      -- nothing to attack, contribute to the score!
      let hit = Total.attack hitter
      reportEffect pSpot cSpot $ mempty {attackBump = True, scoreChange = hit}
      return (board & spotToLens pSpot . #score +~ hit)
  where
    pSpotLens = spotToLens pSpot
    attackeePSpot = otherPlayerSpot pSpot
    pOtherSpotLens :: Lens' (Board Core) (PlayerPart Core)
    pOtherSpotLens = spotToLens attackeePSpot
    attackersInPlace :: Map CardSpot (Creature Core) =
      board ^. pSpotLens . #inPlace
    attackeesInPlace :: Map CardSpot (Creature Core) =
      board ^. pOtherSpotLens . #inPlace
    attacker :: Maybe (Creature Core) = attackersInPlace !? cSpot
    attackerCanAttack = (attacker <&> Card.attack & fromMaybe 0) > 0
    attackerSkills :: [Skill] = attacker <&> skills & fromMaybe [] & map Card.liftSkill
    allyBlocker :: Maybe (Creature Core) =
      if any (`elem` attackerSkills) [Ranged, LongReach]
        then Nothing -- attacker bypasses ally blocker (if any)
        else allyBlockerSpot cSpot >>= (attackersInPlace !?)
    attackedSpots :: [CardSpot] = enemySpots attackerCanAttack attackerSkills cSpot
    -- For the moment a card attacks the first card in front of it. If
    -- later there's a skill Rampage, this will change:
    attackee :: Maybe (CardSpot, Creature Core) =
      asum [(spot,) <$> (attackeesInPlace !? spot) | spot <- attackedSpots]

applyInPlaceEffectOnBoard ::
  -- | The effect of the attacker on the hittee
  InPlaceEffect ->
  -- | The input board
  Board Core ->
  -- | The creature being hit
  (PlayerSpot, CardSpot, Creature Core) ->
  -- | The updated board
  Board Core
applyInPlaceEffectOnBoard effect board (pSpot, cSpot, hittee@Creature {creatureId, items}) =
  case hittee' of
    Just _ -> board'
    Nothing | Card.transient hittee -> board' -- Dont' put hittee in discarded stack
    Nothing ->
      let discarded = boardToDiscarded board' pSpot
       in boardSetDiscarded board' pSpot $ discarded ++ [IDC creatureId items]
  where
    hittee' = applyInPlaceEffect effect hittee
    -- Update the hittee in the board, putting Nothing or Just _:
    board' = board & spotToLens pSpot . #inPlace . at cSpot .~ hittee'

applyInPlaceEffect ::
  -- | The effect of the attacker on the hittee
  InPlaceEffect ->
  -- | The creature being hit
  Creature Core ->
  -- | The creature being hit, after applying the effect; or None if dead
  Maybe (Creature Core)
applyInPlaceEffect effect creature@Creature {..} =
  case effect of
    InPlaceEffect {death} | isDead death -> Nothing
    InPlaceEffect {hitPointsChange = i} -> Just $ creature {hp = hp + i}

applyFlailOfTheDamned ::
  MonadWriter (Board UI) m =>
  MonadState SharedModel m =>
  -- The input board
  Board 'Core ->
  -- The hitter
  Creature 'Core ->
  -- The hitter's position
  (PlayerSpot, CardSpot) ->
  m (Board 'Core)
applyFlailOfTheDamned board creature (pSpot, cSpot) =
  if not hasFlailOfTheDamned
    then return noChange
    else do
      shared <- get
      let spots =
            boardToPlayerHoleyInPlace board pSpot
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
                  & Card.unliftCreature
          let spawned' = spawned {transient = True}
          let board' = boardSetCreature board pSpot spawningSpot spawned'
          -- TODO @smelc record an animation highlighting the flail
          reportEffect pSpot spawningSpot $ mempty {fadeIn = True}
          return board'
  where
    hasFlailOfTheDamned = Card.items creature & elem FlailOfTheDamned
    noChange = board

-- The effect of an attack on the defender
singleAttack :: Creature Core -> Creature Core -> InPlaceEffect
singleAttack attacker defender
  | hps' <= 0 = mempty {death = UsualDeath}
  | otherwise = mempty {hitPointsChange = - hit}
  where
    hit = Total.attack attacker
    hps' = Card.hp defender - hit

-- | The spot that blocks a spot from attacking, which happens
-- | if the input spot is in the back line
allyBlockerSpot :: CardSpot -> Maybe CardSpot
allyBlockerSpot TopLeft = Just BottomLeft
allyBlockerSpot Top = Just Bottom
allyBlockerSpot TopRight = Just BottomRight
allyBlockerSpot _ = Nothing

-- | The other spot in the column in the spot's part
-- otherYSpot :: CardSpot -> CardSpot
-- otherYSpot TopLeft = BottomLeft
-- otherYSpot Top = Bottom
-- otherYSpot TopRight = BottomRight
-- otherYSpot BottomLeft = TopLeft
-- otherYSpot Bottom = Top
-- otherYSpot BottomRight = TopRight

-- | All enemy spots of a spot
allEnemySpots :: CardSpot -> [CardSpot]
allEnemySpots = enemySpots True [Ranged]

-- | Spots that can be attacked from a spot
-- | Spot as argument is in one player part while spots returned
-- | are in the other player part.
-- | The order in the result matters, the first element is the first spot
-- | attacked, then the second element is attacked if the first spot is empty
enemySpots :: Bool -> [Skill] -> CardSpot -> [CardSpot]
enemySpots canAttack skills cSpot =
  map bottomSpotOfTopVisual base'
  where
    spotsInSight =
      case cSpot of
        TopLeft -> [TopLeft, BottomLeft]
        Top -> [Top, Bottom]
        TopRight -> [TopRight, BottomRight]
        BottomLeft -> [TopLeft, BottomLeft]
        Bottom -> [Top, Bottom]
        BottomRight -> [TopRight, BottomRight]
    base =
      if
          | Ranged `elem` skills -> spotsInSight
          | inTheBack cSpot -> if LongReach `elem` skills then take 1 spotsInSight else []
          | otherwise -> take 1 spotsInSight
    base' = if canAttack then base else []

-- | The order in which cards attack
attackOrder :: PlayerSpot -> [CardSpot]
attackOrder PlayerTop =
  [BottomRight, Bottom, BottomLeft, TopRight, Top, TopLeft]
attackOrder PlayerBot =
  map bottomSpotOfTopVisual $ reverse $ attackOrder PlayerTop

nextAttackSpot :: Board Core -> PlayerSpot -> Maybe CardSpot -> Maybe CardSpot
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
    spots :: [CardSpot] = attackOrder pSpot
    hasCreature c = isJust $ boardToInPlaceCreature board pSpot c

-- | The reason for drawing a card
data DrawSource
  = -- | Drawing one of the [nbDrawCards] cards allowed
    Native
  | -- | Drawing a card because of a creature with the [DrawCard] skill at the
    -- given position
    CardDrawer PlayerSpot CardSpot
  deriving (Eq, Ord, Show)

-- | The cards to draw, the Boolean indicates whether to bound by the
-- stack's length or not
cardsToDraw :: Board Core -> PlayerSpot -> Bool -> [DrawSource]
cardsToDraw board pSpot considerStack =
  map (const Native) [0 .. natives - 1] ++ map (CardDrawer pSpot) cardsDrawer
  where
    stackLen = length $ boardToStack board pSpot
    natives =
      let base = Constants.nbCardsToDraw
       in min base (if considerStack then stackLen else base)
    cardsDrawer =
      map (\cSpot -> (cSpot, boardToInPlaceCreature board pSpot cSpot)) (attackOrder pSpot)
        & liftOpt
        & map (\(cSpot, c) -> nbAvailDrawCardSkill c & flip replicate cSpot)
        & concat
    liftOpt [] = []
    liftOpt ((x, Nothing) : rest) = liftOpt rest
    liftOpt ((x, Just y) : rest) = (x, y) : liftOpt rest
    nbAvailDrawCardSkill Creature {skills} =
      filter (\case DrawCard' b -> b; _ -> False) skills & length
