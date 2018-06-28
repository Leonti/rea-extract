{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Concurrent
import           Data.Char
import           Data.Maybe
import           Data.Text          hiding (filter)
import           Network.HTTP
import           Text.HTML.TagSoup
import           Text.Regex.TDFA

import           Control.Monad
import           System.IO

import           Data.Time.Calendar
import           Data.Time.Clock

import           System.Directory
import           System.Environment
import           System.FilePath    ((</>))

import           DbStore
import           Geocoding
import           Models
import           ResultsParsing
import           SoldResultsParsing

import qualified Database.MongoDB   as Mongo
import           Streaming
import qualified Streaming.Prelude  as S

listFiles :: String -> IO [FilePath]
listFiles folder = do
    homeDirectory <- getHomeDirectory
    let resultsFolder = homeDirectory ++ folder
    files <- listDirectory resultsFolder
    filesOnly <- filterM (\d -> doesFileExist (resultsFolder </> d)) (filter (/= ".DS_Store") files)
    return $ fmap (resultsFolder </>) filesOnly

fileToProperties :: FilePath -> IO [ParsedProperty]
fileToProperties path =
    parsePage <$> readFile path

soldFileToProperties :: FilePath -> IO [SoldParsedProperty]
soldFileToProperties path =
    parseSoldPage <$> readFile path

parseSingleSoldPage :: IO ()
parseSingleSoldPage = do
    homeDirectory <- getHomeDirectory
    contents <- readFile $ homeDirectory ++ "/reaSoldResults/2017-4-20/list-1"
    let soldResults = parseSoldPage contents
    print $ show soldResults

main :: IO ()
main = do
    homeDirectory <- getHomeDirectory
    dates <- listDirectory (homeDirectory </> "reaResults")
    folderDates <- filterM (\d -> doesDirectoryExist (homeDirectory </> "reaResults" </> d)) dates
    soldDates <- listDirectory (homeDirectory </> "reaSoldResults")
    folderSoldDates <- filterM (\d -> doesDirectoryExist (homeDirectory </> "reaSoldResults" </> d)) soldDates
    _ <- sequence_ $ fmap processDate folderDates
    _ <- sequence_ $ fmap processSoldPropertiesDate folderSoldDates
    print "Done"

processDate :: String -> IO ()
processDate date = do
    allFiles <- listFiles $ "/reaResults/" ++ date
    let allProperties = S.mapM fileToProperties $ S.each allFiles
    let flattenedPropertiesWithPrice = S.filter hasPrice $ S.concat allProperties
    pipe <- getAuthenticatedMongoPipe
    let insertActions = S.map (insertSingleAction date) flattenedPropertiesWithPrice
    S.mapM_ (runMongoAction pipe) insertActions
    Mongo.close pipe
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

processSoldPropertiesDate :: String -> IO ()
processSoldPropertiesDate date = do
    allFiles <- listFiles $ "/reaSoldResults/" ++ date
    let allSoldProperties = S.mapM soldFileToProperties $ S.each allFiles
    let flattenedSoldPropertiesWithPrice = S.filter soldPropertyHasPrice $ S.concat allSoldProperties
    pipe <- getAuthenticatedMongoPipe
    let insertActions = S.map insertSingleSoldAction flattenedSoldPropertiesWithPrice
    S.mapM_ (runMongoAction pipe) insertActions
    Mongo.close pipe
    print $ "Finished sold date " ++ date

insertSingleSoldAction :: SoldParsedProperty -> Mongo.Action IO ()
insertSingleSoldAction property = Mongo.upsert (Mongo.select (existingSoldPropertySelector property) "soldProperties") $ toSoldPropertyDocument property

hasPrice :: ParsedProperty -> Bool
hasPrice property = isJust (price property)

soldPropertyHasPrice :: SoldParsedProperty -> Bool
soldPropertyHasPrice property = isJust (price (soldProperty property))

openURL :: String -> IO String
openURL x = getResponseBody =<< simpleHTTP (getRequest x)
