{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Char
import Network.HTTP
import Text.HTML.TagSoup
import Text.Regex.TDFA
import Data.Maybe
import Control.Concurrent
import Data.Text

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
import qualified Database.MongoDB as Mongo
import Control.Concurrent.Spawn(pool, parMapIO)

listFiles :: String -> IO [FilePath]
listFiles folder = do
    homeDirectory <- getHomeDirectory
    let resultsFolder = homeDirectory ++ folder
    files <- listDirectory resultsFolder
    return $ fmap (\file -> resultsFolder ++ "/" ++ file) files

fileToProperties :: FilePath -> IO [ParsedProperty]
fileToProperties path = do
    contents <- readFile path
    return $ parsePage contents

soldFileToProperties :: FilePath -> IO [SoldParsedProperty]
soldFileToProperties path = do
    contents <- readFile path
    return $ parseSoldPage contents

parseSingleSoldPage :: IO ()
parseSingleSoldPage = do
    homeDirectory <- getHomeDirectory
    contents <- readFile $ homeDirectory ++ "/reaSoldResults/2017-4-20/list-1"
    let soldResults = parseSoldPage contents
    print $ show soldResults

main :: IO ()
main = do
    wrap <- pool 100
    homeDirectory <- getHomeDirectory
    dates <- listDirectory $ homeDirectory ++ "/reaResults"
    soldDates <- listDirectory $ homeDirectory ++ "/reaSoldResults"
    pipe <- getAuthenticatedMongoPipe
    _ <- parMapIO (wrap . (processDate pipe)) dates
    _ <- parMapIO (wrap . (processSoldPropertiesDate pipe)) soldDates
    Mongo.close pipe
    print "Done"

processDate :: Mongo.Pipe -> String -> IO ()
processDate pipe date = do
    allFiles <- listFiles $ "/reaResults/" ++ date
    let allProperties = S.mapM fileToProperties $ S.each allFiles
    let flattenedPropertiesWithPrice = S.filter hasPrice $ S.concat allProperties
    let insertActions = S.map (insertSingleAction date) flattenedPropertiesWithPrice
    S.mapM_ (runMongoAction pipe) insertActions
    print $ "Finished date " ++ date

getAuthenticatedMongoPipe :: IO Mongo.Pipe
getAuthenticatedMongoPipe = do
    mongoHostPort <- getEnv "MONGO_HOST_PORT"
    mongoDb <- getEnv "MONGO_DB"
    mongoUsername <- getEnv "MONGO_USERNAME"
    mongoPassword <- getEnv "MONGO_PASSWORD"
    pipe <- Mongo.connect (Mongo.readHostPort mongoHostPort)
    _ <- Mongo.access pipe Mongo.UnconfirmedWrites (pack mongoDb) $ Mongo.auth (pack mongoUsername) (pack mongoPassword)
    return pipe

runMongoAction :: Mongo.Pipe -> Mongo.Action IO () -> IO ()
runMongoAction pipe action = do
    mongoDb <- getEnv "MONGO_DB"
    e <- Mongo.access pipe Mongo.UnconfirmedWrites (pack mongoDb) action
    return ()

insertSingleAction :: String -> ParsedProperty -> Mongo.Action IO ()
insertSingleAction date property = Mongo.upsert (Mongo.select (existingPropertySelector date property) "properties") $ toPropertyDocument date property

processSoldPropertiesDate :: Mongo.Pipe -> String -> IO ()
processSoldPropertiesDate pipe date = do
    allFiles <- listFiles $ "/reaSoldResults/" ++ date
    let allSoldProperties = S.mapM soldFileToProperties $ S.each allFiles
    let flattenedSoldPropertiesWithPrice = S.filter soldPropertyHasPrice $ S.concat allSoldProperties
    let insertActions = S.map insertSingleSoldAction flattenedSoldPropertiesWithPrice
    S.mapM_ (runMongoAction pipe) insertActions
    print $ "Finished sold date " ++ date

insertSingleSoldAction :: SoldParsedProperty -> Mongo.Action IO ()
insertSingleSoldAction property = Mongo.upsert (Mongo.select (existingSoldPropertySelector property) "soldProperties") $ toSoldPropertyDocument property

hasPrice :: ParsedProperty -> Bool
hasPrice property = isJust (price property)

soldPropertyHasPrice :: SoldParsedProperty -> Bool
soldPropertyHasPrice property = isJust (price (soldProperty property))

openURL :: String -> IO String
openURL x = getResponseBody =<< simpleHTTP (getRequest x)
