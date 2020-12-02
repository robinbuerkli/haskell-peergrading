{-# OPTIONS_GHC -Wall #-}
module Hgrade.ListFunctions where

import Data.List

colsToRows :: [[a]] -> [[a]]
colsToRows [] = []
colsToRows ([]:_) = []
colsToRows xs = (map head xs) : colsToRows (map tail xs)

median :: [Int] -> Double
median [] = 0
median xs
        | length sortedList `mod` 2 == 0 = fromIntegral (sortedList!!m + sortedList!!(m-1)) / 2
        | otherwise = fromIntegral (sortedList!!m)
        where m = (length xs) `div` 2
              sortedList = sort xs

calculateMedians :: [[Int]] -> [Double]
calculateMedians [] = []
calculateMedians (x:xs) = (median x) : calculateMedians xs

doubleToStringList :: [Double] -> [String]
doubleToStringList xs = map (\d -> show d) xs

histogram :: [Int] -> (Int, Int, Int)
histogram []  = (0,0,0)
histogram col = (countPerPoints 0 col, countPerPoints 1 col, countPerPoints 2 col)

countPerPoints :: Int -> [Int] -> Int
countPerPoints _ [] = 0
countPerPoints e grading = length (filter (\x -> e == x) grading)
