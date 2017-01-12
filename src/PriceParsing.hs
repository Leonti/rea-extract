module PriceParsing where

import Text.Regex.TDFA
import qualified Data.Text as T
import Data.Maybe

parsePrice :: String -> Maybe Int
parsePrice textPrice =
    if null prices then Nothing else Just $ last prices
    where
        maybeRawRegularPrice :: Maybe Int
        maybeRawRegularPrice = (\p -> read p :: Int) <$> extractPrice "\\$([0-9][0-9][0-9],[0-9][0-9][0-9])" textPrice

        maybeRawPriceWithK :: Maybe Int
        maybeRawPriceWithK = (\p -> read p :: Int) . (++ "000") <$> extractPrice "\\$([0-9][0-9][0-9][Kk])" textPrice

        prices :: [Int]
        prices = catMaybes [maybeRawRegularPrice, maybeRawPriceWithK]

extractPrice :: String -> String -> Maybe String
extractPrice regex textPrice =
    fmap stripPrice maybeStringPrice
    where
        matches :: [String]
        matches = getAllTextMatches (textPrice =~ regex :: AllTextMatches [] String)

        maybeStringPrice :: Maybe String
        maybeStringPrice = if null matches then Nothing else Just $ last matches

stripPrice :: String -> String
stripPrice price = removeChar "k" $ removeChar "K" $ removeChar "$" $ removeChar "," price

removeChar :: String -> String -> String
removeChar char text = T.unpack $ T.replace (T.pack char) (T.pack "") (T.pack text)
