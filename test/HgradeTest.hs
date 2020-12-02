module Main (main) where

import Hgrade.ListFunctions
import Data.List
import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Unit Tests" [medianTests, histoTests, c2rTests]

medianTests :: TestTree
medianTests = testGroup "Median tests"
  [ testCase "median empty list" $
          median [] @?= 0,

    testCase "median even list" $
          median [1,0,6,1] @?= 1,

    testCase "median odd list" $
          median [1,0,3] @?= 1.0,

    testCase "median list of zeros" $
          median [0,0] @?= 0
  ]

histoTests :: TestTree
histoTests = testGroup "Histogram tests"
  [ testCase "histogram empty list" $
          histogram [] @?= (0,0,0),

    testCase "histogram four elements" $
          histogram [1,0,2,1] @?= (1,2,1),

    testCase "histogram 6 elements" $
          histogram [1,0,2,1,1,0] @?= (2,3,1)
  ]

c2rTests :: TestTree
c2rTests = testGroup "ColsToRows tests"
  [ testCase "ColsToRows empty list" $
          null (colsToRows []) @?= True,

    testCase "colsToRows basic gradigs" $
          colsToRows [[0,2,2,2,2],[0,2,2,2,1],[2,2,2,2,1],[2,0,0,1,2]] @?= [[0,0,2,2],[2,2,2,0],[2,2,2,0],[2,2,2,1],[2,1,1,2]],

    testCase "colsToRows compare with transpose result" $
          colsToRows [[0,2,2,2,2],[0,2,2,2,1],[2,2,2,2,1],[2,0,0,1,2]] @?= transpose [[0,2,2,2,2],[0,2,2,2,1],[2,2,2,2,1],[2,0,0,1,2]]
  ]