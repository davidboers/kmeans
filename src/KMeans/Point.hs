{-# LANGUAGE FlexibleInstances #-}
module KMeans.Point (Point, distance, center) where

import Data.List


class Eq a => Point a where
    center :: [a] -> a

    distance :: a -> a -> Double


-- Coordinates

instance (Eq a, RealFloat a) => Point (a, a) where
    center points =
        ( avg $ map fst points, avg $ map snd points )
      where
        avg :: RealFloat a => [a] -> a
        avg l =
            sum l / fromIntegral (length l)

    distance (x1, y1) (x2, y2) =
        hypotenouse
            (fromIntegral (round x2) - fromIntegral (round x1))
            (fromIntegral (round y2) - fromIntegral (round y1))


-- Lists of integrals

instance Integral a => Point [a] where
    center points
        | all null points = []
        | otherwise       = map (round . avg) $ transpose points
      where
        avg :: Integral a => [a] -> Double
        avg l =
            sum (map fromIntegral l) / fromIntegral (length l)

    distance xs ys =
        sqrt . sum $ zipWith (\x y -> squared (fromIntegral x - fromIntegral y)) xs ys


-- Helpers

hypotenouse :: RealFloat a => a -> a -> a
hypotenouse a b =
    sqrt $ squared a + squared b

squared :: RealFloat a => a -> a
squared a =
    a ^ (2 :: Integer)