{-|
Module      : Peergrading in Haskell
Description : A graded project in fprog about grading projects of peers

This is just the main entry point of the whole app. It passes the responsibility to the Hgrade main module.
-}
module Main where

import qualified Hgrade

-- | Calls the Hgrade main function
main :: IO ()
main = Hgrade.main