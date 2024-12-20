{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeFamilies #-}

{- |

The `Point` class is a type constraint intended to simplify the syntax, specifically to prevent too many '[[a]]'.
-}
module KMeans.Point (Point, distance, center, closestFriend, Named (..), namePoints) where

import Data.Char

import qualified Data.Text.Lazy as T
import Data.Text.Metrics (levenshtein)

import Data.List
import Data.List.Extras.Argmax

import Data.Maybe

import Data.Hashable

import qualified Data.HashMap.Strict as HM

import KMeans.Utils

-- | Any observation to be clustered should be of a type with an instance of Point.
class Eq a => Point a where
    -- | @'center' points@ returns the center of a group of Points of the same type.
    -- Most likely, this will be an average of the given @points@. The function is called
    -- to create the centroid of a 'KMeans.Cluster'.
    center :: [a] -> a

    -- | @'distance' x y@ returns the distance between a Point @x@ and @y@ in 'Double' form.
    distance :: a -> a -> Double

-- Coordinates

instance Point (Double, Double) where
    -- \| Point halfway between the coordinates.
    center =
        mapBoth avgDouble . unzip

    -- \| Length of the hypotenuse of the triangle produced between the two coordinates as determined by the Pythagorean Theorem.
    distance (x1, y1) (x2, y2) =
        sqrt $ ((x2 - x1) ** 2) + ((y2 - y1) ** 2)

-- Integrals

instance Point Int where
    -- \| Average @point@
    center = round . avgIntegral

    distance x y = fromIntegral $ abs $ x - y

instance Point Char where -- Not technically an integral but whatever
    center chars = argmax (`count` chars) chars

    distance x y = distance (ord x) (ord y)

-- Text

instance Point T.Text where
    center strings =
        T.pack $ center (map T.unpack strings)

    distance x y = fromIntegral $ levenshtein (T.toStrict x) (T.toStrict y)

-- Lists of anything with an instance of `Point`.

instance Point a => Point [a] where
    center points
        | all null points = []
        | otherwise = map center $ transpose points

    -- \| Euclidean distance between the two lists.
    distance xs ys =
        sqrt $ sum $ map (** 2) $ zipWith distance xs ys

-- HashMaps

instance (Hashable k, Eq k, Point v) => Point (HM.HashMap k v) where
    center hms =
        HM.fromList $ zip keys $ map (\k -> center $ mapMaybe (HM.!? k) hms) keys
      where
        keys = nub $ concatMap HM.keys hms

    distance x y =
        weight * sum distances
      where
        distances = HM.elems $ HM.intersectionWith distance x y
        weight = fromIntegral (max (length x) (length y)) / fromIntegral (length distances)

-- Named Points

-- | This data type exists to facilitate the clustering of labeled data formats.
data Point a => Named n a
    = -- | An actual data point to cluster.
      Named
        { name :: n
        , point :: a
        -- ^ Constrained by 'Point', handed to clustering algorithm.
        }
    | -- | An imaginary point that lacks a name. Centroids are initialized using this constructor.
      Virtual a
    deriving (Eq)

-- | Point instance
instance (Eq n, Point a) => Point (Named n a) where
    center named = Virtual $ center (map point named)

    distance (Named{point = x}) (Named{point = y}) = distance x y
    distance (Named{point = x}) (Virtual y) = distance x y
    distance (Virtual x) (Named{point = y}) = distance x y
    distance (Virtual x) (Virtual y) = distance x y

instance (Show n, Point a) => Show (Named n a) where
    show (Named{name}) = show name
    show (Virtual _) = show "Virtual point"

{- | @'namePoints' names points@ casts @points@ to a list of named points. Each
point is assigned the name provided at the same index in the @names@ parameter.

> namePoints names points == zipWith Named names points
-}
namePoints :: Point a => [n] -> [a] -> [Named n a]
namePoints =
    zipWith Named

-- Utils

{- | @'closestFriend' ps a@ returns the point that is the closest (least
'distance') to @a@ among points @ps@.

> closestFriend ps a == argmin (distance a) $ filter (/= a) ps
-}
closestFriend :: Point a => [a] -> a -> a
closestFriend ps a =
    argmin (distance a) $ filter (/= a) ps
