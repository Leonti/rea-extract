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

listFiles :: String -> IO [FilePath]
listFiles folder = do
    homeDirectory <- getHomeDirectory
    let resultsFolder = homeDirectory ++ "/reaResults/" ++ folder
    files <- listDirectory resultsFolder
    return $ fmap (\file -> resultsFolder ++ "/" ++ file) files

fileToProperties :: FilePath -> IO [Property]
fileToProperties path = do
    handle <- openFile path ReadMode
    contents <- hGetContents handle
    let parsedProperties = parsePage contents
    _ <- print $ length parsedProperties -- force parsing before closing the file handle
    hClose handle
    return parsedProperties

main :: IO ()
main = do
    allFiles <- listFiles "2017-1-10"
    allProperties <- mapM fileToProperties allFiles
    let flattenedProperties = concat allProperties
    _ <- print $ filter hasPrice flattenedProperties
    print $ length $ filter hasPrice flattenedProperties

hasPrice :: Property -> Bool
hasPrice property = isJust (price property)

singlePage :: IO ()
singlePage = do
    properties <- fileToProperties "/Users/leonti.bielski/reaResults/2017-1-10/list-1"
    print properties
