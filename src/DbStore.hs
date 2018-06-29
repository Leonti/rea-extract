{-# LANGUAGE OverloadedStrings #-}

module DbStore where

import           Data.Maybe
import           Data.Time
import           Models

import           Database.MongoDB (Action, Document, Document, Selector, Value,
                                   access, close, connect, delete, exclude,
                                   find, host, insertMany, master, project,
                                   rest, select, sort, (=:))

existingPropertySelector :: String -> ParsedProperty -> Selector
existingPropertySelector date p =
    [ "link" =: link p
    , "extractedDate" =: date
    ]

toPropertyDocument :: String -> ParsedProperty -> Document
toPropertyDocument date p =
    [ "link" =: link p
    , "extractedDate" =: date
    , "bedrooms" =: bedrooms (details p)
    , "bathrooms" =: bathrooms (details p)
    , "cars" =: cars (details p)
    , "location" =: location p
    , "price" =: fromJust (price p)
    ]

formatDate :: LocalTime -> String
formatDate = formatTime defaultTimeLocale "%Y-%m-%d"

existingSoldPropertySelector :: SoldParsedProperty -> Selector
existingSoldPropertySelector soldP =
    [ "link" =: link (soldProperty soldP)
    , "soldAt" =: formatDate (soldAt soldP)
    ]

toSoldPropertyDocument :: SoldParsedProperty -> Document
toSoldPropertyDocument soldP =
    [ "link" =: link p
    , "soldAt" =: formatDate (soldAt soldP)
    , "bedrooms" =: bedrooms (details p)
    , "bathrooms" =: bathrooms (details p)
    , "cars" =: cars (details p)
    , "location" =: location p
    , "price" =: fromJust (price p)
    ]
    where
        p = soldProperty soldP
