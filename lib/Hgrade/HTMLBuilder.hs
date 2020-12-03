{-|
Module      : HTML Builder
Description : This module contains functions to create html output

This module contains a set of functions to create html output more easily.
-}
module Hgrade.HTMLBuilder where

import Hgrade.ListFunctions

-- | wraps the passed string into a standard html, so that every page metadata is the same
renderPage :: String -> String
renderPage content = unlines [
  "<!doctype html>",
  "<html lang='en'>",
  "  <head>",
  "      <title>fprog - Peergrading in Haskell</title>",
  "      <link rel=\"stylesheet\" type=\"text/css\" href=\"/static/styles.css\">",
  " </head>",
  "  <body>" ++ content ++ "</body>",
  "</html>"]

-- | wraps the string list into list items
ul :: [String] -> String
ul [] = ""
ul [s] = "<li>" ++ s ++ "</li>"
ul (s:ss) = "<li>" ++ s ++ "</li>" ++ ul ss

-- | creates an anchor element with the given parameters
a :: String -> String -> String
a href title = "<a href=\"" ++ href ++ "\">" ++ title ++ "</a>"

-- | wraps the string list into table head elements
th :: [String] -> String
th [] = ""
th [s] = "<th>" ++ s ++ "</th>"
th (s:ss) = "<th>" ++ s ++ "</th>" ++ th ss

-- | transforms the string list into table rows
tr :: [String] -> String
tr [] = ""
tr [s] = "<tr>" ++ s ++ "</tr>"
tr (s:ss) = "<tr>" ++ s ++ "</tr>" ++ tr ss

-- | transforms a list of showable values into table columns
td :: Show a => [a] -> String
td[] = ""
td [s] = "<td>" ++ (show s) ++ "</td>"
td (s:ss) = "<td>" ++ (show s) ++ "</td>" ++ td ss

-- | creates an input field with a corresponding label
labeledInput :: String -> String
labeledInput name = "<label for=\"" ++ name ++"\">" ++ name ++ ": </label><input type=\"text\" name=\"" ++ name ++ "\" />"

-- | build rows with the grader's data
buildGraderRows :: [String] -> [[Int]] -> String
buildGraderRows [] _ = ""
buildGraderRows (_:_) [] = ""
buildGraderRows (x:xs) (g: gs) = tr [("<td>" ++ x ++ "</td>" ++ td g)] ++ buildGraderRows xs gs

-- | build the table row for medians
buildMedianRow :: [Double] -> String
buildMedianRow xs = tr ["<td>Median</td>" ++ concatMap(\d -> td ([d])) xs]

-- | build the table row for histograms
buildHistogramRow :: Int -> [[Int]]  -> String
buildHistogramRow n xs = tr ["<td>Histograms</td>" ++ concatMap(\d -> "<td>" ++ (buildHistogramTable n d) ++ "</td>") xs]

-- | build a single histogram
buildHistogramTable :: Int -> [Int] -> String
buildHistogramTable _ [] = ""
buildHistogramTable n xs = "<table class='histo'>" ++ (buildHistogramRows n (histogram xs)) ++ "</table>"

-- | build a single row within the histogram table
buildHistogramRows :: Int -> (Int, Int, Int) -> String
buildHistogramRows 0 _ = ""
buildHistogramRows n (first, second, third) = tr[(tdColor n first ++ tdColor n second ++ tdColor n third)] ++ (buildHistogramRows (n - 1) (first, second, third))

-- | print a white or black table column by comparing the two input values
tdColor :: Int -> Int -> String
tdColor x y
        | x > y = "<td class='white'></td>"
        | otherwise = "<td class='black'></td>"