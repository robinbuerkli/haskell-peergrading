{-# OPTIONS_GHC -Wall #-}

module Hgrade.Demo where

import Hgrade.FSActions
import System.Random
import Control.Monad (mapM, replicateM, forM_)

predefAuthors = ["author1", "author2", "author3", "author4", "author5", "author6"]
predefGraders = ["grader1", "grader2", "grader3"]

generateDemoData :: [String] -> IO ()
generateDemoData criteria = do
                            mapM (\a -> createAuthor a) predefAuthors
                            mapM (\a -> generateGradings a criteria) predefAuthors
                            return ()

generateGradings :: String -> [String] -> IO ()
generateGradings author criteria =  do
                                    mapM (\g -> (storeGraderData author g criteria)) predefGraders
                                    return ()

storeGraderData :: String -> String -> [String] -> IO ()
storeGraderData author grader criteria =  do
                                          rndGrades <- rndGradingsForCriteria criteria
                                          storeGrading author grader rndGrades


randomGradings :: [String] -> [IO Int]
randomGradings criteria = do
                          gradings <- replicateM (length criteria) randomRIO(0,2)
                          return gradings

rndGradingsForCriteria :: [String] -> IO [Int]
rndGradingsForCriteria criteria = sequence(randomGradings criteria)


