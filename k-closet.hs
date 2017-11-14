module KClosest where

import Data.List

points :: [(Int, Int)]
points = [(-2, 3), (-1, 0), (1, 2), (4, 3), (2, 3)]

closest :: Int -> [(Int, Int)] -> [(Int, Int)]
closest 0 _ = []
closest _ [] = []
closest n xs = take n $ ordered xs

ordered :: [(Int, Int)] -> [(Int, Int)]
ordered [] = []
ordered xs = sortBy ord xs

ord :: (Int, Int) -> (Int, Int) -> Ordering
ord x y
  | dist x < dist y = LT
  | dist x > dist y = GT
  | otherwise = EQ

dist :: (Int, Int) -> Int
dist tup = intSqrt $ (abs (fst tup) ^ 2) + (abs (snd tup) ^ 2)

intSqrt :: Double -> Double
intSqrt n = sqrt fromIntegral n
