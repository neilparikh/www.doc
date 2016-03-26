{-# LANGUAGE OverloadedStrings #-}

-- Packages:
-- * pandoc
-- * scotty
-- * split

import Data.String (fromString)
import qualified Data.ByteString.Lazy as BS
import Data.List.Split (splitOn)
import Data.Default
import Text.Pandoc.Definition
import Text.Pandoc.Readers.Docx
import Web.Scotty

main = do
  docxText <- BS.readFile "web.docx"
  case (readDocx def docxText) of
    Left err -> putStrLn . show $ err
    Right (Pandoc _ doc, media) -> scotty 3000 (sequence_ . map paraToRoute $ doc)

paraToRoute :: Block -> ScottyM ()
paraToRoute (Para els) = let [x, y] = splitOn [Space, Str "->", Space] els
                         in get (validatePath x) ((sequence . simplifyBody) y >>= html . mconcat . map fromString)

validatePath :: [Inline] -> RoutePattern
validatePath [Emph [Str route]] = fromString route
validatePath _ = error "error parsing the route"

simplifyBody :: [Inline] -> [ActionM String]
simplifyBody = map (\x -> case x of
                            Space -> return " "
                            Str string -> return string
                            Strong [Str var] -> param . fromString $ var
                            _ -> error "invalid formatting")
