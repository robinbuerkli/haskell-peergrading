module Hgrade.HTMLBuilder where

import Control.Monad (mapM, replicateM, forM_, filterM)
import Data.Text (pack, Text)
import Hgrade.ListFunctions
import           Control.Monad.IO.Class (liftIO)


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
