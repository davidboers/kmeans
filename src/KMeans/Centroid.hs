module KMeans.Centroid where

import KMeans.Point
import KMeans.Cluster

newtype (Point a) => Centroid a = Centroid a
  deriving (Eq)

distance2Centeroid :: Point a => a -> Centroid a -> Double
distance2Centeroid a (Centroid b) =
    distance a b

newCentroid :: Point a => Cluster a -> Centroid a
newCentroid (Cluster points) =
    Centroid $ center points