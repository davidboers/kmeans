{-# LANGUAGE FlexibleInstances #-}
{-|

The `Point` class is a type constraint intended to simplify the syntax, specifically to prevent too many '[[a]]'.

-}
module KMeans.Point (Point, distance, center) where

import Data.List

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

instance (Eq a, RealFloat a) => Point (a, a) where
    -- | Point halfway between the coordinates.
    center points =
        ( avg $ map fst points, avg $ map snd points )
      where
        avg :: RealFloat a => [a] -> a
        avg l =
            sum l / fromIntegral (length l)

    -- | Length of the hypotenouse of the triangle produced between the two coordinates as determined by the Pythagorean Theorem.
    distance (x1, y1) (x2, y2) =
        hypotenouse
            (fromIntegral (round x2) - fromIntegral (round x1))
            (fromIntegral (round y2) - fromIntegral (round y1))


-- Lists of integrals

instance Integral a => Point [a] where
    -- | Average @point@
    center points
        | all null points = []
        | otherwise       = map (round . avg) $ transpose points
      where
        avg :: Integral a => [a] -> Double
        avg l =
            sum (map fromIntegral l) / fromIntegral (length l)

    -- | Euclidean distance between the two lists.
    distance xs ys =
        sqrt . sum $ zipWith (\x y -> squared (fromIntegral x - fromIntegral y)) xs ys


-- Helpers

hypotenouse :: RealFloat a => a -> a -> a
hypotenouse a b =
    sqrt $ squared a + squared b

squared :: RealFloat a => a -> a
squared a =
    a ^ (2 :: Integer)