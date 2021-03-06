module Main where

{-# LANGUAGE OverloadedStrings #-}

-- Packages:
-- * pandoc
-- * scotty
-- * split

import qualified Data.ByteString.Lazy as BS
import Text.Pandoc.Definition
import Text.Pandoc.Readers.Docx
import Data.Default
import Web.Scotty
import System.Environment (getArgs)

import Lib

main :: IO ()
main = do
  args <- getArgs
  case args of
    [filename] -> do
      rawDocx <- BS.readFile filename
      case (readDocx def rawDocx) of
        Left err -> putStrLn . show $ err
        Right (Pandoc _ doc, _) -> scotty 3000 $ do
          (sequence_ . map (paraToRoute "") $ doc)
    _ -> putStrLn "Usage: ./www.dot filename.docx"
