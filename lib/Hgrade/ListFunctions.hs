{-|
Module      : List functions
Description : This modules contains some custom list functions needed in the project.

Contains the colsToRows, median and histogram functions demanded as well as some other auxiliary functions.
-}
module Hgrade.ListFunctions where

import Data.List

-- | transforms a list so that the entires of a list get mapped to a set of the same associated values. or just transforms columns to rows...
colsToRows :: [[a]] -> [[a]]
colsToRows [] = []
colsToRows ([]:_) = []
colsToRows xs = (map head xs) : colsToRows (map tail xs)

-- | returns the median of a list
median :: [Int] -> Double
median [] = 0
median xs
        | length sortedList `mod` 2 == 0 = fromIntegral (sortedList!!m + sortedList!!(m-1)) / 2
        | otherwise = fromIntegral (sortedList!!m)
        where m = (length xs) `div` 2
              sortedList = sort xs

-- | calculates the medians of all lists within the given list
calculateMedians :: [[Int]] -> [Double]
calculateMedians [] = []
calculateMedians (x:xs) = (median x) : calculateMedians xs

-- | generates a histogram for the given column list (gradings for a specific criteria)
histogram :: [Int] -> (Int, Int, Int)
histogram []  = (0,0,0)
histogram col = (countPerPoints 0 col, countPerPoints 1 col, countPerPoints 2 col)

-- | counts the number of points an author got for a given criteria
countPerPoints :: Int -> [Int] -> Int
countPerPoints _ [] = 0
countPerPoints e grading = length (filter (\x -> e == x) grading)
