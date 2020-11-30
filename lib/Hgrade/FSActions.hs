{-# OPTIONS_GHC -Wall #-}
module Hgrade.FSActions where

import System.Directory
import System.IO
import Control.Monad (mapM, replicateM, forM_, filterM)
import Data.Text (pack, Text)
import           Control.Monad.IO.Class (liftIO)

listAuthors :: IO [FilePath]
listAuthors = do
              curDir <- getCurrentDirectory
              authors <- listDirectory (curDir ++ "/data")
              return authors

listGraders :: String -> IO [FilePath]
listGraders author =  do
                      curDir <- getCurrentDirectory
                      graders <- listDirectory (curDir ++ "/data/" ++ author)
                      return graders


getFileName :: String -> String
getFileName []  = ""
getFileName (x:xs)
            | x == '.' = ""
            | otherwise = [x] ++ (getFileName xs)

--getGrading :: FilePath -> FilePath -> IO [Int]
getGrading :: FilePath -> FilePath -> IO [Int]
getGrading author grader =  do
                            curDir <- getCurrentDirectory
                            contents <- readFile (curDir ++ "/data/" ++ author ++ "/" ++ grader)
                            let grading = read (contents)
                            return grading

getGradingsForAuthor :: String -> IO [[Int]]
getGradingsForAuthor author =  do
                              graders <- listGraders author
                              gradings <- (mapM (\g -> (getGrading author) g) graders)
                              let intGrades = gradings
                              return intGrades


colsToRows :: [[a]] -> [[a]]
colsToRows [[]] = []
colsToRows [[], xs] = []
colsToRows xs = (map head xs) : colsToRows (map tail xs)