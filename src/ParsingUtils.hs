module ParsingUtils where

import Text.CSS3.Selectors.Parser
import Text.HTML.TagSoup.Tree.Zipper
import Text.CSS3.Selectors.Syntax
import Text.HTML.TagSoup
import Text.HTML.TagSoup.Tree
import Text.HTML.TagSoup.Tree.Selection
import Text.Read
import Models

detailToInt :: Maybe String -> Int
detailToInt maybeDetail =
    maybeToInt maybeIntDetail
    where
        detailToMaybeInt :: String -> Maybe Int
        detailToMaybeInt = readMaybe

        maybeIntDetail = maybeDetail >>= detailToMaybeInt

        maybeToInt :: Maybe Int -> Int
        maybeToInt (Just detail) = detail
        maybeToInt Nothing = 0

extractPrice :: String -> TagTreePos String -> Maybe String
extractPrice selector tree =
    if not (null priceTree) then
        Just $ innerText $ flattenTree [content $ head priceTree]
    else
        Nothing
    where
        priceTree :: [TagTreePos String]
        priceTree = select (sel selector) (content tree)
