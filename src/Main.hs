module Main where

import Data.Char
import Network.HTTP
import Text.HTML.TagSoup
import Text.Regex.TDFA
import Data.Maybe
import Control.Concurrent

import System.IO
import Control.Monad

import Data.Time.Clock
import Data.Time.Calendar

import Data.List.Split

import System.Directory

import ResultsParsing
import Geocoding
import DbStore

import Database.SQLite.Simple

listFiles :: String -> IO [FilePath]
listFiles folder = do
    homeDirectory <- getHomeDirectory
    let resultsFolder = homeDirectory ++ "/reaResults/" ++ folder
    files <- listDirectory resultsFolder
    return $ fmap (\file -> resultsFolder ++ "/" ++ file) files

fileToProperties :: FilePath -> IO [ParsedProperty]
fileToProperties path = do
    handle <- openFile path ReadMode
    contents <- hGetContents handle
    let parsedProperties = parsePage contents
    _ <- print $ Prelude.length parsedProperties -- force parsing before closing the file handle
    hClose handle
    return parsedProperties

propertiesWithGeocoding :: [ParsedProperty] -> IO [(ParsedProperty, Maybe LatLng)]
propertiesWithGeocoding properties = do
    let addresses = propertiesToAddresses properties
    let batchAddresses = chunksOf 100 addresses
    batchGeocodedLocations <- mapM geocodeAddresses batchAddresses
    let geocodedLocations = fromJust $ concat <$> sequence batchGeocodedLocations
    return (zip properties geocodedLocations)

propertiesToAddresses :: [ParsedProperty] -> [String]
propertiesToAddresses = fmap location

main :: IO ()
main = do
    allFiles <- listFiles "2017-1-10"
    allProperties <- mapM fileToProperties allFiles
    let flattenedPropertiesWithPrice = filter hasPrice $ concat allProperties
    geocodedProperties <- propertiesWithGeocoding flattenedPropertiesWithPrice
    let validProperties = filter hasGeocoding geocodedProperties
    _ <- print validProperties
    print $ Prelude.length validProperties

hasPrice :: ParsedProperty -> Bool
hasPrice property = isJust (price property)

hasGeocoding :: (ParsedProperty, Maybe LatLng) -> Bool
hasGeocoding (property, geocodedLocation) = isJust geocodedLocation

singlePage :: IO ()
singlePage = do
    properties <- fileToProperties "/Users/leonti.bielski/reaResults/2017-1-10/list-1"
    geocodedProperties <- propertiesWithGeocoding properties
    print geocodedProperties


location1 = "280 Albert Street, East Melbourne, Vic 3002"
location2 = "G21/8 Garfield Street, Richmond, Vic 3121"

openURL :: String -> IO String
openURL x = getResponseBody =<< simpleHTTP (getRequest x)

geocodeAddresses :: [String] -> IO (Maybe [Maybe LatLng])
geocodeAddresses addresses = do
    geocodeResponse <- openURL $ mapQuestUrl "g8ovkvRh4q4HHX8BQ6oeqQJlMblgMfq9" addresses
    _ <- print "doing mapQuestRequest"
    return $ geocodeResponseToResults geocodeResponse

readGeocoding :: IO ()
readGeocoding = do
    geocoding <- openURL $ mapQuestUrl "g8ovkvRh4q4HHX8BQ6oeqQJlMblgMfq9" [location1, location2, "sadsniuhuygxw"]
--    geocoding <- readFile "/Users/leonti.bielski/geocoding_response"
    let parsedGeocoding = geocodeResponseToResults geocoding
    print parsedGeocoding

dbTest :: IO ()
dbTest = do
    conn <- open "properties.db"
    execute conn propertyRowInsertQuery (PropertyRow "url" "2017-1-10" 1 1 1 "Street number" 399000 (-37.45) 144.2)
    r <- query_ conn propertyRowSelectAllQuery :: IO [PropertyRow]
    mapM_ print r
    Database.SQLite.Simple.close conn
