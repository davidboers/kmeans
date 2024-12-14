{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-|

The `Point` class is a type constraint intended to simplify the syntax, specifically to prevent too many '[[a]]'.

-}
module KMeans.Point (Point, distance, center, closestFriend) where

import Data.List as L hiding (head, tail)
import qualified Data.Text.Lazy as T
import Data.Text.Metrics (levenshtein)
import Data.Char

import Data.List.Extras.Argmax


-- | Any observation to be clustered should be of a type with an instance of Point.
--  
class Eq a => Point a where
    -- | @'center' points@ returns the center of a group of Points of the same type.
    -- Most likely, this will be an average of the given @points@. The function is called
    -- to create the centroid of a 'Cluster'.
    center :: [a] -> a

    -- | @'distance' x y@ returns the distance between a Point @x@ and @y@ in 'Double' form. 
    distance :: a -> a -> Double


-- Coordinates

instance Point (Double, Double) where
    -- | Point halfway between the coordinates.
    center points =
        (avgDouble $ map fst points, avgDouble $ map snd points)

    -- | Length of the hypotenuse of the triangle produced between the two coordinates as determined by the Pythagorean Theorem.
    distance (x1, y1) (x2, y2) =
        sqrt $ (dx * dx) + (dy * dy)
      where
        dx = x2 - x1
        dy = y2 - y1


-- Integrals

instance Point Int where
    -- | Average @point@
    center = round . avgIntegral

    distance x y = fromIntegral $ abs $ x - y

instance Point Char where -- Not technically an integral but whatever
    center chars = argmax (\c -> length $ filter (c ==) chars) chars

    distance x y = distance (ord x) (ord y)


-- Text

instance Point T.Text where
    center strings =
        T.pack $ center (map T.unpack strings)

    distance x y = distance (T.unpack x) (T.unpack y) --fromIntegral $ levenshtein (T.toStrict x) (T.toStrict y)


-- Lists of anything that can be constrained by `Point`

instance Point a => Point [a] where
    center points
        | all null points = []
        | otherwise       = map center $ transpose points

    -- | Euclidean distance between the two lists.
    distance xs ys =
        sqrt $ sum $ map (\d -> d * d) $ zipWith distance xs ys


-- Utils

-- | Returns the point that is the closest (least 'distance') to
-- @a@ among points @ps@.
--
-- > closestFriend ps a == argmin (distance a) $ filter (/= a) ps
closestFriend :: Point a => [a] -> a -> a
closestFriend ps a =
    argmin (distance a) $ filter (/= a) ps


-- Helpers

avgIntegral :: Integral a => [a] -> Double
avgIntegral l =
    fromIntegral (sum l) / fromIntegral (L.length l)

avgDouble :: Floating a => [a] -> a
avgDouble l =
    sum l / fromIntegral (L.length l)