{-# OPTIONS_GHC -Wall #-}
module Hgrade.HTMLBuilder where

import Data.Text (pack, Text)
import Hgrade.ListFunctions


createPage :: String -> String
createPage content = unlines [
  "<!doctype html>",
  "<html lang='en'>",
  "  <head>",
  "      <title>fprog - Peergrading in Haskell</title>",
  "      <link rel=\"stylesheet\" type=\"text/css\" href=\"/static/styles.css\">",
  " </head>",
  "  <body>" ++ content ++ "</body>",
  "</html>"]


ul :: [String] -> String
ul [] = ""
ul [s] = "<li>" ++ s ++ "</li>"
ul (s:ss) = "<li>" ++ s ++ "</li>" ++ ul ss

a :: String -> String -> String
a href title = "<a href=\"" ++ href ++ "\">" ++ title ++ "</a>"

th :: [String] -> String
th [] = ""
th [s] = "<th>" ++ s ++ "</th>"
th (s:ss) = "<th>" ++ s ++ "</th>" ++ th ss

tr :: [String] -> String
tr [] = ""
tr [s] = "<tr>" ++ s ++ "</tr>"
tr (s:ss) = "<tr>" ++ s ++ "</tr>" ++ tr ss

td :: Show a => [a] -> String
td[] = ""
td [s] = "<td>" ++ (show s) ++ "</td>"
td (s:ss) = "<td>" ++ (show s) ++ "</td>" ++ td ss

labeledInput :: String -> String
labeledInput name = "<label for=\"" ++ name ++"\">" ++ name ++ ": </label><input type=\"text\" name=\"" ++ name ++ "\" />"

criteriaInput :: String -> String
criteriaInput name = "<label for=\"" ++ name ++"\">" ++ name ++ ": </label><input type=\"text\" name=\"criteria[" ++ name ++ "]\" />"

buildGraderRows :: [String] -> [[Int]] -> String
buildGraderRows [] _ = ""
buildGraderRows (_:_) [] = ""
buildGraderRows (x:xs) (g: gs) = tr [("<td>" ++ x ++ "</td>" ++ td g)] ++ buildGraderRows xs gs

buildMedianRow :: [Double] -> String
buildMedianRow xs = tr ["<td>Median</td>" ++ concatMap(\d -> td ([d])) xs]

buildHistogramRow :: Int -> [[Int]]  -> String
buildHistogramRow n xs = tr ["<td>Histograms</td>" ++ concatMap(\d -> "<td>" ++ (buildHistogramTable n d) ++ "</td>") xs]

buildHistogramTable :: Int -> [Int] -> String
buildHistogramTable _ [] = ""
buildHistogramTable n xs = "<table class='histo'>" ++ (buildHistogramRows n (histogram xs)) ++ "</table>"

buildHistogramRows :: Int -> (Int, Int, Int) -> String
buildHistogramRows 0 _ = ""
buildHistogramRows n (fst, snd, trd) = tr[(tdColor n fst ++ tdColor n snd ++ tdColor n trd)] ++ (buildHistogramRows (n - 1) (fst, snd, trd))

tdColor :: Int -> Int -> String
tdColor x y
        | x > y = "<td class='white'></td>"
        | otherwise = "<td class='black'></td>"