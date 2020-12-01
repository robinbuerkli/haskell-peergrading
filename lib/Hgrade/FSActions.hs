{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE ViewPatterns #-}

module Hgrade.FSActions where

import System.Directory
import System.IO
import Web.Scotty
import Control.Monad (mapM, replicateM, forM_, filterM)
import qualified Data.Text.Lazy as T
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


storeGrading :: String -> String -> [Int] -> IO ()
storeGrading author grader gradings = do
                                      curDir <- getCurrentDirectory
                                      createDirectoryIfMissing True (curDir ++ "/data/" ++ author)
                                      let file = (curDir ++ "/data/" ++ author ++ "/" ++ grader ++  ".txt")
                                      writeFile file (show gradings)
                                      return ()