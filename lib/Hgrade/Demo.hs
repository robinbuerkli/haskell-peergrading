{-|
Module      : Demo generation
Description : This modules generates the demo data

This module contains a set of auxiliary functions to generate random gradings for a set of predefined authors and graders.
-}
module Hgrade.Demo where

import Hgrade.FSActions
import System.Random
import Control.Monad (replicateM)

-- | list of predefined authors
predefAuthors :: [String]
predefAuthors = ["author1", "author2", "author3", "author4", "author5", "author6"]

-- | list of predefined graders
predefGraders :: [String]
predefGraders = ["grader1", "grader2", "grader3", "grader4"]

-- | creates the authors and assigns them a set of graders with their respective gradings
-- takes the list of criteria, so that the matching number of grading entries will be generated
generateDemoData :: [String] -> IO ()
generateDemoData criteria = do
                            _ <- mapM (\a -> createAuthor a) predefAuthors
                            _ <- mapM (\a -> generateGradings a criteria) predefAuthors
                            return ()

-- | generates gradings for a specific author. also the list of criteria is passed, to match the list entries
generateGradings :: String -> [String] -> IO ()
generateGradings author criteria =  do
                                    _ <- mapM (\g -> (storeGraderData author g criteria)) predefGraders
                                    return ()

-- | store grading data for a specific author and grader. takes criteria as well
storeGraderData :: String -> String -> [String] -> IO ()
storeGraderData author grader criteria =  do
                                          rndGrades <- rndGradingsForCriteria criteria
                                          storeGrading author grader rndGrades

-- | transforms the list we're getting from randomGradings from [IO Int] to IO [Int], so it can be further processed
rndGradingsForCriteria :: [String] -> IO [Int]
rndGradingsForCriteria criteria = sequence(randomGradings criteria)

-- | generates a list of random numbers for which the length is matched with the one's being passed
randomGradings :: [String] -> [IO Int]
randomGradings criteria = do
                          gradings <- replicateM (length criteria) randomRIO(0,2)
                          return gradings