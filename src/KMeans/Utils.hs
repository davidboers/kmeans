module KMeans.Utils where 

avgIntegral :: Integral a => [a] -> Double
avgIntegral l =
    fromIntegral (sum l) / fromIntegral (length l)

avgDouble :: Floating a => [a] -> a
avgDouble l =
    sum l / fromIntegral (length l)

mapBoth :: (a -> b) -> (a, a) -> (b, b)
mapBoth func (x, y) =
    (func x, func y)

mapIndex :: (Int -> a -> b) -> [a] -> [b]
mapIndex f xs =
    zipWith f (indices xs) xs

indices :: [a] -> [Int]
indices l =
    [0..(length l - 1)]