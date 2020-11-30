{-# LANGUAGE OverloadedStrings #-}

module Hgrade where

import           Web.Scotty
import           Control.Monad.IO.Class (liftIO)
import           Network.Wai.Middleware.RequestLogger ( logStdoutDev )
import qualified Data.Text.Lazy as T
import           Hgrade.FSActions
import           Hgrade.HTMLBuilder as HTML

main :: IO ()
main = do
  putStrLn "Good Luck!"
  scotty 4000 $ do
    middleware logStdoutDev
    
    get "/" indexHtml
    get "/authors" authorOverviewHtml
    get "/authors/:author" authorHtml
    get "/grade" gradeFormHtml
    post "grade" gradeFormHandling

    get "/static/styles.css" $ file "static/styles.css"


indexHtml :: ActionM () 
indexHtml = html (T.pack (HTML.createPage "<h1>Hgrade</h1><h2>Peergrading in Haskell</h2><div id=\"links\"><ul><li><a href=\"./authors\">Grading Overview</a></li><li><a href=\"grade\">Submit Grading</a></li></ul></div>"))

authorOverviewHtml :: ActionM ()
authorOverviewHtml = do
              authors <- liftIO listAuthors
              html (T.pack (HTML.createPage "<h1>Authors</h1><ul>" ++ (HTML.ul (reverse (map (\author -> HTML.a ("authors/" ++ author) author) authors))) ++ "</ul>"))


criteria = ["N1", "N2", "F1", "F2", "F3"]

formInputs = concat [["Author", "Grader"], criteria]

authorHtml :: ActionM ()
authorHtml =  do
              author <- param "author"
              graders <- liftIO (listGraders author)
              gradings <- liftIO (getGradingsForAuthor author)
              html (T.pack (HTML.createPage ("<h1>Author: " ++ author ++ "</h1>" ++ "<table><tr>" ++ HTML.th ([]:criteria) ++ "<tr>" ++ buildGraderRows (map getFileName graders) gradings  ++  "</table>" )))

gradeFormHtml :: ActionM ()
gradeFormHtml =   do
                  html (T.pack (HTML.createPage ("<h1>Grade</h1>" ++ "<form method=\"post\">" ++ ((concatMap (\i -> HTML.labeledInput i ++ "<br />") formInputs)) ++ "<button type=\"submit\">Send</button></form>")))


gradeFormHandling :: ActionM()
gradeFormHandling = do
                    html (T.pack ("imlement this"))