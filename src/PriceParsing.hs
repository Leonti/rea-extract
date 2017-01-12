module PriceParsing where

import Text.Regex.TDFA
import qualified Data.Text as T

parsePrice :: String -> Maybe Integer
parsePrice textPrice = Just 12

findRawRegularPriceMatch :: String -> Maybe String
findRawRegularPriceMatch textPrice =
    fmap stripPrice maybeRawRegularPrice
    where
        regex = "\\$([0-9][0-9][0-9],[0-9][0-9][0-9])"

        matches :: [String]
        matches = getAllTextMatches (textPrice =~ regex :: AllTextMatches [] String)

        maybeRawRegularPrice :: Maybe String
        maybeRawRegularPrice = if null matches then Nothing else Just $ head matches

stripPrice :: String -> String
stripPrice price = removeChar "K" $ removeChar "$" $ removeChar "," price

removeChar :: String -> String -> String
removeChar char text = T.unpack $ T.replace (T.pack char) (T.pack "") (T.pack text)
