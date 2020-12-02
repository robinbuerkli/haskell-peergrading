{-# LANGUAGE ViewPatterns #-}

{-|
Module      : Filesystem actions
Description : This modules contains functions which interact with the filesystem

This module contains a set of functions with interact with the filesystem. Mainly it handles the reading and storing of
authors, graders and gradings.
-}
module Hgrade.FSActions where

import System.Directory

-- | get a list of the existing authors
listAuthors :: IO [FilePath]
listAuthors = do
              curDir <- getCurrentDirectory
              authors <- listDirectory (curDir ++ "/data")
              return authors

-- | get a list of the graders for a given author
listGraders :: String -> IO [FilePath]
listGraders author =  do
                      curDir <- getCurrentDirectory
                      graders <- listDirectory (curDir ++ "/data/" ++ author)
                      return graders

-- | get the filename without extension
getFileName :: String -> String
getFileName []  = ""
getFileName (x:xs)
            | x == '.' = ""
            | otherwise = [x] ++ (getFileName xs)

-- | read the gradings of an author grader
getGrading :: FilePath -> FilePath -> IO [Int]
getGrading author grader =  do
                            curDir <- getCurrentDirectory
                            contents <- readFile (curDir ++ "/data/" ++ author ++ "/" ++ grader)
                            let grading = read (contents)
                            return grading

-- | get all gradings for a specific author
getGradingsForAuthor :: String -> IO [[Int]]
getGradingsForAuthor author = do
                              graders <- listGraders author
                              gradings <- (mapM (\g -> (getGrading author) g) graders)
                              let intGrades = gradings
                              return intGrades

-- | store the grading data for a given author and grader
storeGrading :: String -> String -> [Int] -> IO ()
storeGrading author grader gradings = do
                                      curDir <- getCurrentDirectory
                                      createAuthor author
                                      let file = (curDir ++ "/data/" ++ author ++ "/" ++ grader ++  ".txt")
                                      writeFile file (show gradings)
                                      return ()

-- | create the directory of an author
createAuthor :: String -> IO ()
createAuthor author = do
                      curDir <- getCurrentDirectory
                      createDirectoryIfMissing True (curDir ++ "/data/" ++ author)
                      return ()