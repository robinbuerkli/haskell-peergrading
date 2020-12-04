{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Peergrading in Haskell
Description : A graded project in fprog about grading projects of peers

This ist the main module of the whole project. It contains all the scotty pages, which call functions in other modules
in the project.
-}
module Hgrade where

import            Web.Scotty
import            Control.Monad.IO.Class (liftIO)
import            Network.Wai.Middleware.RequestLogger ( logStdoutDev )
import qualified  Data.Text.Lazy as T
import            Hgrade.FSActions
import            Hgrade.ListFunctions
import            Hgrade.HTMLBuilder
import            Hgrade.Demo as DEMO
import            System.Environment

-- | list of the criteria, for which the authors get graded by
criteria :: [String]
criteria = ["N1", "N2", "F1", "F2", "F3"]

-- | list of the total form inputs on the grading page
formInputs :: [String]
formInputs = concat [["Author", "Grader"], criteria]

-- | main IO action, which checks if the demo argument is passed and starts the scotty webserver
main :: IO ()
main = do
  args <- getArgs
  do if (length args) > 0 then if ("-demo" `elem` args) then generateDemo else return () else return () -- check if demo data is needed

  scotty 4000 $ do
    middleware logStdoutDev

    -- List of pages
    get "/" indexHtml
    get "/authors" authorOverviewHtml
    get "/authors/:author" authorHtml
    get "/grade" gradeFormHtml
    post "/grade" gradeFormHandling

    -- static resources
    get "/static/styles.css" $ file "static/styles.css"

-- | homepage, which you'll get with a get request to the root
indexHtml :: ActionM ()
indexHtml = html (T.pack (
                        page [
                          h1 "Hgrade",
                          h2 "Peergrading in Haskell",
                          divEl ["id='links']"] (
                            ul (concat [
                              li (a "/authors" "Grading Overview"),
                              li (a "/grade" "Submit Grading")
                            ])
                          )
                        ]))

-- | overview of authors
authorOverviewHtml :: ActionM ()

authorOverviewHtml = do
              authors <- liftIO listAuthors
              html (T.pack (
                        page [
                          h1 "Authors",
                           (ul (concatMap(\author -> li author) (map (\author -> a ("authors/" ++ author) author) (reverse authors))))
                         ]))

-- | detail page of a single author
authorHtml :: ActionM ()
authorHtml =  do
              author <- param "author"
              graders <- liftIO (listGraders author)
              gradings <- liftIO (getGradingsForAuthor author)
              html (T.pack (
                          page [
                            h1 (concat ["Author: ", author]),
                             table [] (concat [(tr (concatMap (\c -> th c) ("":criteria))),
                              (buildGraderRows (map getFileName graders) gradings),
                              (buildMedianRow (calculateMedians (colsToRows gradings))),
                              (buildHistogramRow (length graders) (colsToRows gradings))])
                          ]))

-- | grading page that displays the form
gradeFormHtml :: ActionM ()
gradeFormHtml =   do
                  html (T.pack (
                              page [
                                h1 "Grade",
                                (form ["method='post'"]
                                  (concat [(concatMap (\i -> divEl ["class='formInput']"] (labeledInput i)) formInputs),
                                  (button "Send")]))
                              ]))

-- | handles the post request sent by the grading form
gradeFormHandling :: ActionM()
gradeFormHandling = do
                    -- map input fields into a list of string, which is more suitable for our needs
                    inputs <- mapM (\i -> param (i :: T.Text) :: ActionM T.Text) (map (\i -> T.pack i) formInputs)
                    let inputList = map(\s -> read $ show $ T.unpack s) inputs
                    -- check if there are values
                    if any null inputList then html (T.pack (page [p "Incomplete input.", a "/" "Beam me up, Scotty!"])) else do
                      -- get fields that are always the same (Author & Grader)
                      let author = inputList !! 0
                      let grader = inputList !! 1
                      -- get remain fields, which are the grading ints
                      let gradings = map (\g -> read g) (drop 2 inputList)
                      liftIO (storeGrading author grader gradings)
                      -- redirect the user to the overview
                      redirect "/authors"

-- | calls the generation for demo data
generateDemo :: IO ()
generateDemo = do
               putStrLn "generating demo data..."
               generateDemoData criteria