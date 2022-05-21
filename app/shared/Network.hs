-- | Module providing the API using the data from 'Roads'
module Network
  ( Network (..),
    mkTopology,
    Topology,
  )
where

import qualified Data.Map.Strict as Map
import qualified Direction
import Nat

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
