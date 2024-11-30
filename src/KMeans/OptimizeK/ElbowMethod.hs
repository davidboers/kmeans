module KMeans.OptimizeK.ElbowMethod (elbowMethod, inertia) where

import KMeans.Point
import KMeans.Cluster
import KMeans.Centroid
import KMeans.Algorithm

-- | @'elbowMethod' ps@ returns a map of keys, where [(k, wcss)], for subjective 
-- inspection.
-- 
-- ==== __Examples__
-- 
-- >>> let points = [(46.67, 1.28), (36.21, 41.47), (91.73, 35.01), (93.26, 37.41), (64.51, 78.7), (44.34, 84.58), (19.4, 1.53), (21.17, 16.91)]
-- >>> elbowMethod points
-- [(1,12854),(2,5372),(3,4778),(4,4778),(5,1218),(6,1215),(7,354),(8,354)]
elbowMethod :: Point a => [a] -> [(Int, Int)]
elbowMethod points =
    map (\k -> (k, inertia points k)) [1..length points]

-- | @'inertia' ps k@ returns the inertia (sum of the squared distances) for a 
-- specific set of @k@ clusters produced by the @'kMeansStatic' 100@ function. 
inertia :: Point a => [a] -> Int -> Int
inertia points k =
    sum $ concatMap sqedDistances $ kMeansStatic 100 k points

sqedDistances :: Point a => Cluster a -> [Int]
sqedDistances (Cluster points) =
    let centroid = newCentroid (Cluster points) in
    map (sq . (`distance2Centroid` centroid)) points
  where
    sq a = round $ a ^ (2 :: Integer)
