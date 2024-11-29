module KMeans.Cluster where

import KMeans.Point

import Data.List
import Data.Char


newtype (Point a) => Cluster a = Cluster [a]


clusterLength :: Point a => Cluster a -> Int
clusterLength (Cluster points) = Prelude.length points

sortCluster :: Point a => Ord a => Cluster a -> Cluster a
sortCluster (Cluster cluster) =
    Cluster $ sort cluster

getCluster :: Point a => [Cluster a] -> a -> Cluster a
getCluster clusters point =
    head $ filter (\(Cluster c) -> point `elem` c) clusters


-- Display

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