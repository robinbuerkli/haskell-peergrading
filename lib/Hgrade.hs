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
import            Data.List
import            Hgrade.FSActions
import            Hgrade.ListFunctions
import            Hgrade.HTMLBuilder
import            Hgrade.Demo as DEMO
import            System.Environment

-- | list of the criteria, for which the authors get graded by
criteria :: [String]
criteria = ["N1", "N2", "F1", "F2", "F3"]

-- | list of the grading metadata
formMeta :: [String]
formMeta = ["Author", "Grader"]

-- | list of the total form inputs on the grading page
formInputs :: [String]
formInputs = concat [formMeta, criteria]

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
    get "/static/bootstrap.min.css" $ file "static/bootstrap.min.css"

-- | homepage, which you'll get with a get request to the root
indexHtml :: ActionM ()
indexHtml = html (T.pack (
                        page [
                          h1 "Hgrade",
                          h2 "Peergrading in Haskell",
                          hr,
                          divEl ["class='list-group'"] (concat [
                              paramA ["class='list-group-item list-group-item-action'"] "/authors" "Grading Overview",
                              paramA ["class='list-group-item list-group-item-action'"] "/grade" "Submit Grading"
                            ])
                        ]))

-- | overview of authors
authorOverviewHtml :: ActionM ()
authorOverviewHtml = do
              authors <- liftIO listAuthors
              html (T.pack (
                        page [
                          h1 "Authors",
                          hr,
                          divEl ["class='list-group'"]
                            (concatMap (\author -> paramA ["class='list-group-item list-group-item-action'"] ("authors/" ++ author) author) (sort authors)),
                          hr,
                          a "/" "?? Return to homepage"
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
                              hr,
                              divEl ["class='table-responsive'"] (
                                table ["class='table table-bordered table-striped'"] (concat [(tr (concatMap (\c -> th c) ("":criteria))),
                                  (buildGraderRows (map getFileName graders) gradings),
                                  (buildMedianRow (calculateMedians (colsToRows gradings))),
                                  (buildHistogramRow (length graders) (colsToRows gradings))])
                                ),
                              hr,
                              a "/authors" "?? Return to author overview"
                          ]))

-- | grading page that displays the form
gradeFormHtml :: ActionM ()
gradeFormHtml =   do
                  html (T.pack (
                              page [
                                h1 "Grade",
                                hr,
                                (form ["method='post'"]
                                  (concat [(concatMap (\i -> divEl ["class='form-group row'"] (labeledInput i)) formMeta),
                                  (concatMap (\i -> divEl ["class='form-group row'"] (concat [(label i), divEl ["class='col-sm-10'"] (numberInput i)])) criteria),
                                  hr,
                                  a "/" "?? Return to homepage",
                                  divEl ["class='float-right'"] (button "Send")])
                                )
                              ]))

-- | handles the post request sent by the grading form
gradeFormHandling :: ActionM()
gradeFormHandling = do
                    -- map input fields into a list of string, which is more suitable for our needs
                    inputs <- mapM (\i -> param (i :: T.Text) :: ActionM T.Text) (map (\i -> T.pack i) formInputs)
                    let inputList = map(\s -> read $ show $ T.unpack s) inputs
                    -- check if there are values
                    if any null inputList then html (T.pack (page [h1 "Oh no!", hr, p "Incomplete input.", a "/" "Beam me up, Scotty!"])) else do
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