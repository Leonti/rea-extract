{-# LANGUAGE OverloadedStrings #-}

module DbStore where

import ResultsParsing
import Geocoding
import Data.Maybe
import Database.SQLite.Simple
import Database.SQLite.Simple.FromRow
import Database.SQLite.Simple.ToRow

test = "hello"

-- "CREATE TABLE properties (link TEXT, date TEXT, bedrooms INTEGER, bathrooms INTEGER, cars INTEGER, location TEXT, price INTEGER, lat REAL, lng REAL);"


data PropertyRow = PropertyRow String String Int Int Int String Int Double Double deriving (Show)

instance FromRow PropertyRow where
    fromRow = PropertyRow <$> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field

instance ToRow PropertyRow where
    toRow (PropertyRow link date bedrooms bathrooms cars location price lat lng) = toRow (link, date, bedrooms, bathrooms, cars, location, price, lat, lng)

propertyRowInsertQuery :: Query
propertyRowInsertQuery = "INSERT INTO properties (link, date, bedrooms, bathrooms, cars, location, price, lat, lng) VALUES (?,?,?,?,?,?,?,?,?)"

propertiesForDate :: Query
propertiesForDate = "SELECT * from properties where date=?"

toPropertyRow :: String -> (ParsedProperty, LatLng) -> PropertyRow
toPropertyRow date (p, latLng) =
    PropertyRow (link p) date pBedrooms pBathrooms pCars (location p) propertyPrice (lat latLng) (lng latLng)
    where
        propertyPrice = fromJust (price p)
        pBedrooms = bedrooms (details p)
        pBathrooms = bathrooms (details p)
        pCars = cars (details p)
