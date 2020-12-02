{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}

module Hgrade where

import           Web.Scotty
import           Control.Monad.IO.Class (liftIO)
import           Network.Wai.Middleware.RequestLogger ( logStdoutDev )
import qualified Data.Text.Lazy as T
import           Hgrade.FSActions
import           Hgrade.ListFunctions
import           Hgrade.HTMLBuilder as HTML
import           Hgrade.Demo as DEMO
import           System.Environment


criteria :: [String]
criteria = ["N1", "N2", "F1", "F2", "F3"]

formInputs :: [String]
formInputs = concat [["Author", "Grader"], criteria]

main :: IO ()
main = do
  args <- getArgs
  do if (length args) > 0 then if ("-demo" `elem` args) then generateDemo else return () else return ()

  scotty 4000 $ do
    middleware logStdoutDev
    
    get "/" indexHtml
    get "/authors" authorOverviewHtml
    get "/authors/:author" authorHtml
    get "/grade" gradeFormHtml
    post "/grade" gradeFormHandling

    get "/static/styles.css" $ file "static/styles.css"


indexHtml :: ActionM () 
indexHtml = html (T.pack (HTML.createPage "<h1>Hgrade</h1><h2>Peergrading in Haskell</h2><div id=\"links\"><ul><li><a href=\"./authors\">Grading Overview</a></li><li><a href=\"grade\">Submit Grading</a></li></ul></div>"))

authorOverviewHtml :: ActionM ()
authorOverviewHtml = do
              authors <- liftIO listAuthors
              html (T.pack (HTML.createPage "<h1>Authors</h1><ul>" ++ (HTML.ul (reverse (map (\author -> HTML.a ("authors/" ++ author) author) authors))) ++ "</ul>"))

authorHtml :: ActionM ()
authorHtml =  do
              author <- param "author"
              graders <- liftIO (listGraders author)
              gradings <- liftIO (getGradingsForAuthor author)
              html (T.pack (HTML.createPage ("<h1>Author: " ++ author ++ "</h1>" ++ "<table><tr>" ++ HTML.th ([]:criteria)  ++ buildGraderRows (map getFileName graders) gradings ++ buildMedianRow (calculateMedians (colsToRows gradings))  ++ buildHistogramRow (length graders) (colsToRows gradings) ++ "</table>" )))


gradeFormHtml :: ActionM ()
gradeFormHtml =   do
                  html (T.pack (HTML.createPage ("<h1>Grade</h1>" ++ "<form method=\"post\">" ++ (concatMap (\i -> HTML.labeledInput i ++ "<br />") formInputs) ++ "<button type=\"submit\">Send</button></form>")))


gradeFormHandling :: ActionM()
gradeFormHandling = do
                    inputs <- mapM (\p -> param (p :: T.Text) :: ActionM T.Text) (map (\i -> T.pack i) formInputs)
                    let inputList = map(\s -> read $ show $ T.unpack s) inputs
                    let author = inputList !! 0
                    let grader = inputList !! 1
                    let gradings = map (\g -> read g) (drop 2 inputList)
                    liftIO (storeGrading author grader gradings)
                    redirect "/authors"


generateDemo :: IO ()
generateDemo = do
               putStrLn "generating demo data..."
               generateDemoData criteria


