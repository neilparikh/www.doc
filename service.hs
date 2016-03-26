{-# LANGUAGE OverloadedStrings #-}

-- Packages:
-- * pandoc
-- * scotty
-- * split

import qualified Data.ByteString.Lazy as BS
import Text.Pandoc.Definition
import Text.Pandoc.Readers.Docx
import Data.Default
import Data.String (fromString)
import System.Random (randomRIO)
import Web.Scotty
import Network.Wai.Parse
import Control.Monad.IO.Class

import Lib

main = scotty 3000 $ do
  get "/" $ file "index.html"

  post "/upload" $ do
    fs <- files
    liftIO (putStrLn . show . length $ fs)
    let rawDocx = head [fileContent fi | (_, fi) <- fs]
    -- new_port <- liftIO (randomRIO (3001, 4000))
    new_port <- param "port"
    case (readDocx def rawDocx) of
      Left err -> liftIO $ (putStrLn . show) err
      Right (Pandoc _ doc, _) -> (liftIO (scotty (read new_port) (sequence_ . map (paraToRoute "") $ doc))) 

randStr :: IO String
randStr = sequence . replicate 5 . randomRIO $ ('a', 'z')
