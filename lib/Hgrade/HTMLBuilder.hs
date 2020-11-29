module Hgrade.HTMLBuilder where

import Control.Monad (mapM, replicateM, forM_, filterM)
import Data.Text (pack, Text)
import           Control.Monad.IO.Class (liftIO)

ul :: [String] -> String
ul [] = ""
ul [s] = "<li>" ++ s ++ "</li>"
ul (s:ss) = "<li>" ++ s ++ "</li>" ++ ul ss

a :: String -> String -> String
a href title = "<a href=\"" ++ href ++ "\">" ++ title ++ "</a>"