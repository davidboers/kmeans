module KMeans.OptimizeK.ElbowMethod where

import KMeans.Point
import KMeans.Cluster
import KMeans.Centroid
import KMeans.Algorithm

{- Returns a map, where [(k, wcss)], for subjective inspection -}
elbowMethod :: Point a => [a] -> [(Int, Int)]
elbowMethod points =
  map (inertia points) [1..length points]

inertia :: Point a => [a] -> Int -> (Int, Int)
inertia points k =
    ( k
    , sum $ concatMap sqedDistances $ kMeans k points
    )

sqedDistances :: Point a => Cluster a -> [Int]
sqedDistances (Cluster points) =
    let centroid = newCentroid (Cluster points) in
    map (round . sq . (`distance2Centeroid` centroid)) points
  where
    sq a = a ^ (2 :: Integer)