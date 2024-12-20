module KMeans.Centroid (Centroid (..), distance2Centroid, newCentroid) where

import KMeans.Cluster
import KMeans.Point

{- | Wrapper type for a type constrained by 'Point'. Variables of this wrapper type
represent centroids.
-}
newtype Point a => Centroid a = Centroid a
    deriving (Eq)

{- | @'distance2Centroid' a c@ returns the distance between a 'Point' @a@ and
'Centroid' @c@.

> distance2Centroid a (Centroid c) = distance a c
-}
distance2Centroid :: Point a => a -> Centroid a -> Double
distance2Centroid a (Centroid b) =
    distance a b

{- | @'newCentroid' c@ returns the centroid of 'Cluster' @c@

> newCentroid c = Centroid $ center $ toList c
-}
newCentroid :: Point a => Cluster a -> Centroid a
newCentroid c =
    Centroid $ center $ toList c
