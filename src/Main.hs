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

import System.Environment
import System.Directory

import Models
import ResultsParsing
import SoldResultsParsing
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

propertiesWithGeocoding
  :: Stream (Of ParsedProperty) IO r
     -> Stream (Of (ParsedProperty, Maybe LatLng)) IO r
propertiesWithGeocoding properties = do
    let batchProperties = S.mapped S.toList $ chunksOf 100 properties
    S.concat $ S.concat $ S.mapM geocodeAddresses batchProperties
    -- concat here flattens a stream of lists of as into a stream of as
    -- and a stream of maybe as into a stream of as

parseSingleSoldPage :: IO ()
parseSingleSoldPage = do
    homeDirectory <- getHomeDirectory
    contents <- readFile $ homeDirectory ++ "/reaSoldResults/2017-4-20/list-1"
    let soldResults = parseSoldPage contents
    print $ show soldResults

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
    conn <- open "properties.db"
    existingProperties <- query conn propertiesForDate (Only date) :: IO [PropertyRow]
    let newFlattenedPropertiesWithPrice = S.filter (notYetInserted existingProperties) flattenedPropertiesWithPrice
    let geocodedProperties = S.filter hasGeocoding (propertiesWithGeocoding newFlattenedPropertiesWithPrice)
    S.mapM_ (execute conn propertyRowInsertQuery . createPropertyRow date) geocodedProperties
    insertedProperties <- query conn propertiesForDate (Only date) :: IO [PropertyRow]
    Database.SQLite.Simple.close conn
    print $ date ++ ": " ++ show (Prelude.length insertedProperties)

notYetInserted :: [PropertyRow] -> ParsedProperty -> Bool
notYetInserted existingProperties parsedProperty =
    not $ any isInserted existingProperties
    where
        isInserted :: PropertyRow -> Bool
        isInserted (PropertyRow existingLink _ _ _ _ _ _ _ _) =
            existingLink == link parsedProperty

createPropertyRow :: String -> (ParsedProperty, Maybe LatLng) -> PropertyRow
createPropertyRow date (parsedProperty, maybeLatLng) =
    toPropertyRow date (parsedProperty, fromJust maybeLatLng)

hasPrice :: ParsedProperty -> Bool
hasPrice property = isJust (price property)

--144.95143536,-37.8555269395,144.990094688,-37.799716676

hasGeocoding :: (ParsedProperty, Maybe LatLng) -> Bool
hasGeocoding (_, Just coordinates) = isInMelbourne coordinates
hasGeocoding (_, Nothing) = False

isInMelbourne :: LatLng -> Bool
isInMelbourne coordinates =
    (pLat > -37.8555269395) && (pLat < -37.799716676) && (pLng > 144.95143536) && (pLng < 144.990094688)
    where
        pLat = lat coordinates
        pLng = lng coordinates

openURL :: String -> IO String
openURL x = getResponseBody =<< simpleHTTP (getRequest x)

geocodeAddressesFake :: [ParsedProperty] -> IO (Maybe [(ParsedProperty, Maybe LatLng)])
geocodeAddressesFake properties = do
    _ <- print "Fake geocoding"
    let fakeGeocoding = fmap (\p -> Just LatLng {lat = 1, lng =2}) properties
    return $ Just (zip properties fakeGeocoding)

geocodeAddresses :: [ParsedProperty] -> IO (Maybe [(ParsedProperty, Maybe LatLng)])
geocodeAddresses properties = do
    let addresses = fmap ((++ ", Australia") . location) properties
    mapQuestKey <- getEnv "MAP_QUEST_KEY"
    geocodeResponse <- openURL $ mapQuestUrl mapQuestKey addresses
    let geocodeResults = geocodeResponseToResults geocodeResponse
    _ <- printGeocoded geocodeResponse geocodeResults
    return $ fmap (zip properties) geocodeResults

printGeocoded :: String -> Maybe [Maybe LatLng] -> IO()
printGeocoded response (Just results) = print $ "Got " ++ show (length results) ++ " geocoded results"
printGeocoded response Nothing = print $ "Geocoding error " ++ response
