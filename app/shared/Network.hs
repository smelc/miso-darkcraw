{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Module providing the API using the data from 'Roads'
-- Also contains some hardcoded data, which - contrary to 'Roads' -
-- is not generated.
module Network
  ( chooseTeamSpots,
    deckForEncounter,
    Encounter (..),
    fightSpots,
    journeys,
    lootNextToFight,
    lootSpots,
    Network (..),
    mkTopology,
    rewards,
    Rewards (..),
    Topology,
  )
where

import Card (Team (..))
import qualified Card
import qualified Data.Bifunctor as Bifunctor
import Data.Function ((&))
import Data.List (intersperse)
import qualified Data.Map.Strict as Map
import Data.Maybe
import qualified Direction
import GHC.Generics
import Nat
import qualified Shared
import qualified Theme

-- | Possible encounters on the world map
data Encounter
  = -- | Fighting
    Fight Card.Team Theme.Kind
  | -- | Picking up rewards. The Nat indicates the number of rewards.
    Reward Nat
  | -- | Choosing your team at the start of the game
    Select Card.Team
  deriving (Eq, Generic, Show)

-- | The high-level API to query data from 'Roads'
class Network b where
  neighbors :: b -> Direction.Coord -> [Direction.Coord]

newtype Topology = Topology (Map.Map Direction.Coord [Direction.Coord])

instance Network Topology where
  neighbors (Topology m) k = Map.findWithDefault [] k m

-- | How to obtain the API from data in 'Roads'
mkTopology :: [(Nat, Nat)] -> Topology
mkTopology points = Topology $ Map.fromList l
  where
    coords = map Direction.Coord points
    l =
      [ (k, kNeighbors)
        | k <- coords,
          let kNeighbors = filter (Direction.isAdjacent k) coords
      ]

-- | Map from coordinates to the number of loots at this position
lootSpots :: Map.Map Direction.Coord Nat
lootSpots =
  Map.fromList $
    map (Bifunctor.first Direction.Coord) $
      [ ((24, 34), 1),
        ((30, 37), 1),
        ((19, 28), 2),
        ((18, 23), 2),
        ((18, 23), 2),
        ((30, 24), 1)
      ]

lootNextToFight :: Direction.Coord -> Maybe Direction.Coord
lootNextToFight fight =
  [y | y <- Map.keys lootSpots, y `Direction.isAdjacent` fight]
    & listToMaybe

-- | Rewards after a game. First 'Nat' is the number of rewards to pick.
-- Second list is all available rewards.
data Rewards = Rewards Nat [Card.ID]
  deriving (Eq, Generic, Show)

rewards :: Map.Map Card.Team [Rewards]
rewards =
  Map.fromList $
    [ ( Evil,
        [Rewards 1 [Card.IDI Card.AxeOfRage]]
      ),
      ( Human,
        [ Rewards 1 [mkIDC Human Card.Knight, Card.IDI Card.Crown, Card.IDN Card.Life],
          Rewards 1 [mkIDC Human Card.Ogre]
        ]
      ),
      ( Sylvan,
        [ Rewards 1 [mkSIDC Card.Ranger, mkSIDC Card.Guardian, Card.IDI Card.BowOfGaia],
          Rewards 1 [mkSIDC Card.Ranger, Card.IDN Card.HuntingHorn, Card.IDI Card.BowOfGaia, mkSIDC Card.Guardian],
          Rewards 1 [mkSIDC Card.Worm, mkSIDC Card.Bear]
        ]
      ),
      ( Undead,
        [ Rewards 1 [mkIDC Undead Card.Necromancer, Card.IDI Card.SkBanner],
          Rewards 1 [mkIDC Undead Card.Specter, Card.IDI Card.SwordOfBlood]
        ]
      )
    ]
  where
    mkIDC team kind = Card.IDC (Card.CreatureID kind team) []
    mkSIDC = mkIDC Sylvan

deckForEncounter :: Shared.Model -> Team -> Nat -> [Card.ID]
deckForEncounter shared team nb =
  if nb == 0
    then Card.teamDeck (Shared.getCards shared) team & map Card.toIdentifier
    else
      let indexes = [nb .. 1]
          mkc x = Card.IDC (Card.CreatureID x Human) []
          additions = concat [addition mkc team nb | nb <- indexes]
       in deckForEncounter shared team 0 ++ additions
  where
    addition mkc team (_nb :: Nat) =
      case team of
        Beastmen -> []
        Evil -> []
        Human -> [mkc Card.Knight]
        Sylvan -> [mkc Card.Ranger]
        Undead -> [mkc Card.Necromancer]
        ZKnights -> []

-- | Journeys of playable teams, for testing. This data could be generated
-- automatically, this is tracked in https://github.com/smelc/miso-darkcraw/issues/13
journeys :: Map.Map Card.Team [[Network.Encounter]]
journeys =
  Map.fromList
    [ (Evil, [leftInit ++ leftPath, leftInit ++ rightPath]),
      (Human, [leftInit ++ leftPath, leftInit ++ rightPath]),
      (Sylvan, [leftInit ++ leftPath, leftInit ++ rightPath]),
      (Undead, [rightInit ++ leftPath, leftInit ++ rightPath])
    ]
    & Map.map (map f)
  where
    addRewards = intersperse (Network.Reward 1)
    f y = map (\x -> x Theme.Forest) y & addRewards
    leftInit =
      [Network.Fight Undead]
    leftPath =
      [ Network.Fight Beastmen,
        Network.Fight Evil
      ]
    rightInit = [Network.Fight Sylvan]
    rightPath = [Network.Fight ZKnights]

-- | The position where to choose the team
chooseTeamSpots :: Map.Map Card.Team Direction.Coord
chooseTeamSpots =
  [ (Human, (22, 43)),
    (Sylvan, (24, 43)),
    (Evil, (26, 43)),
    (Undead, (28, 43))
  ]
    & map (Bifunctor.second Direction.Coord)
    & Map.fromList

-- | The spot where the game ends. It must be reachable from all values
-- of 'chooseTeamSpots'
_endSpot :: (Nat, Nat)
_endSpot = (24, 18)

-- | The position of fights, hardcoded yes
fightSpots :: Map.Map Card.Team [(Direction.Coord, Theme.Kind)]
fightSpots =
  Map.map (map (Bifunctor.first Direction.Coord)) $
    Map.fromList $
      [ (Beastmen, [((20, 28), Theme.DarkForest)]),
        (Evil, [((18, 24), Theme.Forest)]),
        (Sylvan, [((30, 38), Theme.Forest)]),
        (Undead, [((24, 35), Theme.DarkForest)]),
        (ZKnights, [((30, 25), Theme.Forest)])
      ]
