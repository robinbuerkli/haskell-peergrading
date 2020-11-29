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
    get "/authors" authorsHtml

    get "/static/styles.css" $ file "static/styles.css"


indexHtml :: ActionM () 
indexHtml = html (T.pack "<h1>Hgrade</h1><h2>Peergrading in Haskell</h2><div id=\"links\"><ul><li><a href=\"./authors\">Grading Overview</a></li><li><a href=\"grade\">Submit Grading</a></li></ul></div>")

authorsHtml :: ActionM ()
authorsHtml = do
              authors <- liftIO listAuthors
              html (T.pack ("<h1>Authors</h1><ul>" ++ (HTML.ul (reverse (map (\author -> HTML.a ("authors/" ++ author) author) authors))) ++ "</ul>"))