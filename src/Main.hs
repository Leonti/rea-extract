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

--import Data.List.Split

import System.Environment
import System.Directory

import ResultsParsing
import Geocoding
import DbStore

import Database.SQLite.Simple

import Streaming
import qualified Streaming.Prelude as S

listFiles :: String -> IO [FilePath]
listFiles folder = do
    homeDirectory <- getHomeDirectory
    let resultsFolder = homeDirectory ++ "/reaResults/" ++ folder
    files <- listDirectory resultsFolder
    return $ fmap (\file -> resultsFolder ++ "/" ++ file) files

fileToProperties :: FilePath -> IO [ParsedProperty]
fileToProperties path = do
    contents <- readFile path
    return $ parsePage contents

--propertiesWithGeocoding :: [ParsedProperty] -> IO [(ParsedProperty, Maybe LatLng)]
--propertiesWithGeocoding properties = do
--    let batchProperties = chunksOf 100 properties
--    batchGeocodedLocations <- mapM geocodeAddresses batchProperties
--    let geocodedLocations = fromJust $ concat <$> sequence batchGeocodedLocations
--    return geocodedLocations

propertiesWithGeocoding
  :: Stream (Of ParsedProperty) IO r
     -> Stream (Of (ParsedProperty, Maybe LatLng)) IO r
propertiesWithGeocoding properties = do
    let batchProperties = S.mapped S.toList $ chunksOf 100 properties
    S.concat $ S.concat $ S.mapM geocodeAddresses batchProperties
    -- concat here flattens a stream of lists of as into a stream of as
    -- and a stream of maybe as into a stream of as

main :: IO ()
main = do
    homeDirectory <- getHomeDirectory
    let reaResultsFolder = homeDirectory ++ "/reaResults"
    dates <- listDirectory reaResultsFolder
    sequence_ $ fmap processDate dates

processDate :: String -> IO ()
processDate date = do
    allFiles <- listFiles date
    let allProperties = S.mapM fileToProperties $ S.each allFiles
    let flattenedPropertiesWithPrice = S.filter hasPrice $ S.concat allProperties
    S.print $  propertiesWithGeocoding flattenedPropertiesWithPrice

--processDate :: String -> IO ()
--processDate date = do
--    allFiles <- listFiles date
--    allProperties <- mapM fileToProperties allFiles
--    let flattenedPropertiesWithPrice = filter hasPrice $ concat allProperties
--    conn <- open "properties.db"
--    existingProperties <- query_ conn propertyRowSelectAllQuery :: IO [PropertyRow]
--    let newFlattenedPropertiesWithPrice = filter (notYetInserted date existingProperties) flattenedPropertiesWithPrice
--    print (fmap location newFlattenedPropertiesWithPrice)
--    geocodedProperties <- propertiesWithGeocoding newFlattenedPropertiesWithPrice
--    let validProperties = filter hasGeocoding geocodedProperties
--    print geocodedProperties

--    mapM_ (execute conn propertyRowInsertQuery . createPropertyRow date) validProperties
--    propertyRows <- query_ conn propertyRowSelectAllQuery :: IO [PropertyRow]
--    mapM_ print propertyRows
--    _ <- print (Prelude.length propertyRows)
--    Database.SQLite.Simple.close conn
--    print $ date ++ ": " ++ show (Prelude.length validProperties)

notYetInserted :: String -> [PropertyRow] -> ParsedProperty -> Bool
notYetInserted date existingProperties parsedProperty =
    not $ any isInserted existingProperties
    where
        isInserted :: PropertyRow -> Bool
        isInserted (PropertyRow existingLink existingDate _ _ _ _ _ _ _) =
            existingLink == link parsedProperty && existingDate == date

createPropertyRow :: String -> (ParsedProperty, Maybe LatLng) -> PropertyRow
createPropertyRow date (parsedProperty, maybeLatLng) =
    toPropertyRow date (parsedProperty, fromJust maybeLatLng)

hasPrice :: ParsedProperty -> Bool
hasPrice property = isJust (price property)

hasGeocoding :: (ParsedProperty, Maybe LatLng) -> Bool
hasGeocoding (property, geocodedLocation) = isJust geocodedLocation

openURL :: String -> IO String
openURL x = getResponseBody =<< simpleHTTP (getRequest x)

geocodeAddresses :: [ParsedProperty] -> IO (Maybe [(ParsedProperty, Maybe LatLng)])
geocodeAddresses properties = do
    let addresses = fmap location properties
    mapQuestKey <- getEnv "MAP_QUEST_KEY"
    geocodeResponse <- openURL $ mapQuestUrl mapQuestKey addresses
    return $ fmap (zip properties) (geocodeResponseToResults geocodeResponse)
