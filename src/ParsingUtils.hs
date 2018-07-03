module ParsingUtils where

import           Models
import           Text.CSS3.Selectors.Parser
import           Text.CSS3.Selectors.Syntax
import           Text.HTML.TagSoup
import           Text.HTML.TagSoup.Tree
import           Text.HTML.TagSoup.Tree.Selection
import           Text.HTML.TagSoup.Tree.Zipper
import           Text.Read

extractPrice :: String -> TagTreePos String -> Maybe String
extractPrice selector tree =
    if not (null priceTree) then
        Just $ innerText $ flattenTree [content $ head priceTree]
    else
        Nothing
    where
        priceTree :: [TagTreePos String]
        priceTree = select (sel selector) (content tree)
