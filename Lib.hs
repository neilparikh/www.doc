module Lib where

import Control.Arrow
import Data.String (fromString)
import Data.List.Split (splitOn)
import Text.Pandoc.Definition
import Web.Scotty

(|>) :: a -> (a -> b) -> b
(|>) = flip ($)


paraToRoute :: String -> Block -> ScottyM ()
paraToRoute uniq_id (Para els) = let [x, y] = splitOn [Space, Str "->", Space] els
                                 in get (fromString $ uniq_id ++ validatePath x)
                                        (simplifyBody y |> sequence >>= (map fromString >>> mconcat >>> html))
paraToRoute _ _ = error "invalid formatting at top level"

validatePath :: [Inline] -> [Char]
validatePath [Emph [Str route]] = route
validatePath _ = error "error parsing the route"

simplifyBody :: [Inline] -> [ActionM String]
simplifyBody = map (\x -> case x of
                            Space -> return " "
                            Str string -> return string
                            Strong [Str var] -> param . fromString $ var
                            Superscript _ -> return ""
                            Strikeout [Str url] -> redirect . fromString $ url
                            _ -> error "invalid formatting in body")
