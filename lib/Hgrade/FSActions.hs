module Hgrade.FSActions where

import System.Directory
import Control.Monad (mapM, replicateM, forM_, filterM)
import Data.Text (pack, Text)
import           Control.Monad.IO.Class (liftIO)

listAuthors :: IO [FilePath]
listAuthors = do
              curDir <- getCurrentDirectory
              authors <- listDirectory (curDir ++ "/data")
              return authors

