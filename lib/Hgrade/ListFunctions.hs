module Hgrade.ListFunctions where

import Data.List


colsToRows :: [[a]] -> [[a]]
--colsToRows [[]] = []
--colsToRows [[], xs] = []
--colsToRows xs = (map head xs) : colsToRows (map tail xs)
colsToRows = transpose

median :: [Int] -> Double
median xs
        | length xs `mod` 2 == 0 = fromIntegral (xs!!m + xs!!(m-1)) / 2
        | otherwise = fromIntegral (xs!!m)
        where m = (length xs) `div` 2

calculateMedians :: [[Int]] -> [Double]
calculateMedians [] = []
calculateMedians (x:xs) = (median (sort x)) : calculateMedians xs

-- calculateMedians [[0,2,0,2],[2,1,1,1],[2,1,0,0],[2,1,2,1],[2,1,1,1]]

doubleToStringList :: [Double] -> [String]
doubleToStringList xs = map (\d -> show d) xs