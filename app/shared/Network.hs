{-# LANGUAGE DeriveGeneric #-}

-- | Module providing the API using the data from 'Roads'
-- Also contains some hardcoded data, which - contrary to 'Roads' -
-- is not generated.
module Network
  ( chooseTeamSpots,
    Encounter (..),
    fightSpots,
    journeys,
    lootSpots,
    Network (..),
    mkTopology,
    rewards,
    Topology,
  )
where

import Card (Team (..))
import qualified Card
import qualified Data.Bifunctor as Bifunctor
import Data.Function ((&))
import Data.List (intersperse)
import qualified Data.Map.Strict as Map
import qualified Direction
import GHC.Generics
import Nat
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

rewards :: Map.Map Card.Team [Card.ID]
rewards =
  Map.fromList $
    [ ( Evil,
        [Card.IDI Card.AxeOfRage]
      ),
      ( Human,
        [ mkIDC Human Card.Knight,
          Card.IDI Card.Crown,
          Card.IDN Card.Life,
          mkIDC Human Card.Ogre
        ]
      ),
      ( Sylvan,
        [ Card.IDI Card.BowOfGaia,
          Card.IDN Card.HuntingHorn,
          Card.IDI Card.BowOfGaia,
          mkIDC Sylvan Card.Worm
        ]
      ),
      ( Undead,
        [ mkIDC Undead Card.Necromancer,
          Card.IDI Card.SkBanner,
          mkIDC Undead Card.Specter
        ]
      )
    ]
  where
    mkIDC team kind = Card.IDC (Card.CreatureID kind team) []

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
