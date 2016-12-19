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

fileToProperties :: FilePath -> IO (Maybe [Property])
fileToProperties path = do
    handle <- openFile path ReadMode
    contents <- hGetContents handle
    let parsedProperties = parsePage contents
    _ <- print $ length parsedProperties
    hClose handle
    return parsedProperties

main :: IO ()
main = do
    allFiles <- listFiles "2016-11-29"
    allProperties <- mapM fileToProperties allFiles
    print allProperties
