{-# LANGUAGE FlexibleInstances #-}

{-|
Module      : HTML Builder
Description : This module contains the html dsl.

This module contains a set of functions to create html output more easily.
-}
module Hgrade.HTMLBuilder where

import Hgrade.ListFunctions

-- | class for creating some kind of html dsl, supports [String] and String
class Html a where
  -- | creates a simple html element with the passed tag and content
  createElement :: String -> a -> String
  -- | creates an html element with the passed parameters, tag and content
  createElementWithParams :: String -> [String] -> a -> String

-- | instance for a simple element with a String passed on
instance Html String where
  -- | default implementation for all createElement functions
  createElementWithParams tag params content = concat ["<", tag, concatMap (\s -> concat [" ", s]) params , ">", content, "</", tag, ">"]
  -- | create a simple tag (pass an empty list)
  createElement tag content = createElementWithParams tag [] content

-- | instance for a [String] array, for elements that contain more elements in them
instance Html [String] where
  -- | create an html tag with a string list of parameters
  createElementWithParams tag params content = createElementWithParams tag params (concat content)
  -- | create a simple tag (pass an empty list)
  createElement tag content = createElementWithParams tag [] content

-- | wraps the passed string into a standard html, so that every page metadata is the same
page :: [String] -> String
page content = htmlRoot [
                         mHead [
                            pageTitle "fprog - Peergrading in Haskell",
                            link ["rel='stylesheet'", "type='text/css'", "href='/static/styles.css'"],
                            link ["rel='stylesheet'", "type='text/css'", "href='/static/bootstrap.min.css'"]
                         ],
                         body [
                            divEl ["class='container'"] (
                              concat content
                            )
                         ]
                       ]

-- | creates the html tag
htmlRoot :: [String] -> String
htmlRoot content = createElement "html" content

-- | creates the body tag
body :: [String] -> String
body content = createElement "body" content

-- | creates the head tag
mHead :: [String] -> String
mHead content = createElement "head" content

-- | creates the link tag (for stylesheets)
link :: [String] -> String
link params = createElementWithParams "link" params ""

-- | creates the title element
pageTitle :: String -> String
pageTitle t = createElement "title" t

-- | creates a header 1 element
h1 :: String -> String
h1 content = createElement "h1" content

-- | creates a header 2 element
h2 :: String -> String
h2 content = createElement "h2" content

-- | creates a paragraph
p :: String -> String
p content = createElement "p" content

-- | creates an unordered list
ul :: String -> String
ul content = createElement "ul" content

-- | creates a list element
li :: String -> String
li content = createElement "li" content

-- | creates a div with the given parameters
divEl :: [String] -> String -> String
divEl params content = createElementWithParams "div" params content

-- | creates a horizontal line
hr :: String
hr = createElement "hr" ""

-- | creates an anchor element
a :: String -> String -> String
a href t = createElementWithParams "a" [concat ["href='", href, "'"]] t

-- | creates an anchor element with the given parameters
paramA :: [String] -> String -> String -> String
paramA params href t = createElementWithParams "a" ["href='", href, "'", concat params] t

-- | creates a table with the given parameters
table :: [String] -> String -> String
table params content = createElementWithParams "table" params content

-- | creates a table row
tr :: String -> String
tr content = createElement "tr" content

-- | creates a table row with the given parameters
paramTr :: [String] -> String -> String
paramTr params content = createElementWithParams "tr" params content

-- | creates a table header column
th :: String -> String
th content = createElement "th" content

-- | creates a table column
td :: String -> String
td content = createElement "td" content

-- | creates a table column with the given parameters (used for the histogram)
paramTd :: [String] -> String -> String
paramTd params content = createElementWithParams "td" params content

-- | creates a form element with the given parameters
form :: [String] -> String -> String
form params content = createElementWithParams "form" params content

-- | creates a text input field
textInput :: String -> String
textInput name = createElementWithParams "input" [concat ["name='", name, "'"], concat ["id='", name, "'"], "type='text'", "class='form-control'", "required='required'"] ""

-- | creates a number input field
numberInput :: String -> String
numberInput name = createElementWithParams "input" [concat ["name='", name, "'"], concat ["id='", name, "'"], "type='number'", "min='0'", "max='2'", "class='form-control'", "required='required'"] ""

-- | creates a label
label :: String -> String
label name = createElementWithParams "label" [concat ["for='", name, "'"], "class='col-sm-2 col-form-label'"] name

-- | creates an input field with a corresponding label
labeledInput :: String -> String
labeledInput name = concat [label name, divEl ["class='col-sm-10'"] (textInput name)]

-- | creates a submit button labeled with the given name
button :: String -> String
button name = createElementWithParams "button" ["type='submit'", "class='btn btn-dark'"] name

-- | build rows with the grader's data
buildGraderRows :: [String] -> [[Int]] -> String
buildGraderRows [] _ = ""
buildGraderRows (_:_) [] = ""
buildGraderRows (x:xs) (g: gs) = tr (concat [(th x), (concatMap (\grading -> td (show grading)) g)]) ++ buildGraderRows xs gs

-- | build the table row for medians
buildMedianRow :: [Double] -> String
buildMedianRow xs = paramTr ["class='median-row'"] (concat [(th "Median"), (concatMap(\d -> td (show d)) xs)])

-- | build the table row for histograms
buildHistogramRow :: Int -> [[Int]]  -> String
buildHistogramRow n xs = tr (concat [(th "Histograms"), (concatMap(\d -> td (buildHistogramTable n d)) xs)])

-- | build a single histogram
buildHistogramTable :: Int -> [Int] -> String
buildHistogramTable _ [] = ""
buildHistogramTable n xs = table ["class='histo']"] (buildHistogramRows n (histogram xs))

-- | build a single row within the histogram table
buildHistogramRows :: Int -> (Int, Int, Int) -> String
buildHistogramRows 0 _ = ""
buildHistogramRows n (first, second, third) = tr (concat [(tdColor n first), (tdColor n second), (tdColor n third)]) ++ (buildHistogramRows (n - 1) (first, second, third))

-- | print a white or black table column by comparing the two input values
tdColor :: Int -> Int -> String
tdColor x y
        | x > y = paramTd ["class='bg-light'"] ""
        | otherwise = paramTd ["class='bg-dark'"] ""