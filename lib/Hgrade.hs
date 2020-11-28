{-# LANGUAGE OverloadedStrings #-}

module Hgrade where

import           Web.Scotty
import           Control.Monad.IO.Class (liftIO)
import           Network.Wai.Middleware.RequestLogger ( logStdoutDev )
import qualified Data.Text.Lazy as T

main :: IO ()
main = do
  putStrLn "Good Luck!"
  scotty 4000 $ do
    middleware logStdoutDev
    
    get "/" indexHtml

    get "/static/styles.css" $ file "static/styles.css"


indexHtml :: ActionM () 
indexHtml = html (T.pack "<h1>Hgrade</h1>")