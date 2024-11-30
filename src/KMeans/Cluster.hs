module KMeans.Cluster (Cluster(..), clusterLength, sortCluster, getCluster, displayClusters) where

import KMeans.Point

import Data.List
import Data.Char

-- | Wrapper type for a list of any type constrained by 'Point'. Variables of this 
-- wrapper type are produced by the clustering algorithm.
newtype (Point a) => Cluster a = Cluster [a]


-- Utils

-- | @'clusterLength' x@ returns the number of points in the cluster @x@.
--
-- > clusterLength (Cluster points) == Prelude.length points
--
-- ==== __Examples__
--
-- >>> let points = [(1, 5), (3, 2), (6, 4), (9, 2)]
-- >>> clusterLength (Cluster points)
-- 4
clusterLength :: Point a => Cluster a -> Int
clusterLength (Cluster points) = Prelude.length points

-- | @'sortCluster' x@ sorts the points in cluster @x@. Type @a@ must be constrained 
-- by 'Ord'. A new 'Cluster' of the sorted 'Point's is returned.
--
-- > sortCluster (Cluster points) == Cluster (sort points)
sortCluster :: Point a => Ord a => Cluster a -> Cluster a
sortCluster (Cluster points) =
    Cluster $ sort points

-- | @'getCluster' cs p@ returns the 'Cluster' of point @p@, provided it is in the
-- list @cs@. If, for some reason, the @p@ is found in multiple clusters, the cluster
-- that appears first in @cs@ is returned. If @p@ appears in no clusters, an exception
-- is thrown.
--
-- > getCluster cs p == head $ filter (\(Cluster c) -> p `elem` c) cs
getCluster :: Point a => [Cluster a] -> a -> Cluster a
getCluster clusters point =
    head $ filter (\(Cluster c) -> point `elem` c) clusters


-- Display

-- | @'displayClusters' cs@ creates a multiline string, displaying the clustered
-- points. Each cluster is assigned a letter, A through Z, with smaller clusters being
-- assigned first.
displayClusters :: Point a => Show a => [Cluster a] -> String
displayClusters clusters =
    unlines [ displayCluster (chr (ord 'A' + i)) (clusters !! i)
            | i <- [0..length clusters - 1]
            ]

displayCluster :: Point a => Show a => Char -> Cluster a -> String
displayCluster letter (Cluster c) =
    unlines $
      ("Cluster " ++ [letter] ++ ":")
      : map displayPoint c

displayPoint :: Point a => Show a => a -> String
displayPoint p =
    "    " ++ show p