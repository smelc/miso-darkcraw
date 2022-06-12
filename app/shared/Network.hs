-- | Module providing the API using the data from 'Roads'
-- Also contains some hardcoded data, which - contrary to 'Roads' -
-- is not generated.
module Network
  ( chooseTeamSpots,
    Encounter (..),
    fightSpots,
    Network (..),
    mkTopology,
    Topology,
  )
where

import Card (Item, Team (..))
import qualified Data.Bifunctor as Bifunctor
import Data.Function ((&))
import qualified Data.Map.Strict as Map
import qualified Direction
import Nat

-- | Possible encounters on the world map
data Encounter
  = -- | Fighting
    Fight Team
  | -- | Picking up an item
    Pickup Item
  | -- | Choosing your team at the start of the game
    Select Team

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

-- | The position where to choose the team
chooseTeamSpots :: Map.Map Team Direction.Coord
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
fightSpots :: Map.Map Team [Direction.Coord]
fightSpots =
  Map.map (map Direction.Coord) $
    Map.fromList $
      [ (Beastmen, [(20, 28)]),
        (Evil, [(18, 24)]),
        (Sylvan, [(30, 38)]),
        (Undead, [(24, 35)]),
        (ZKnights, [(30, 25)])
      ]
