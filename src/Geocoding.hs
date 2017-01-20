{-# LANGUAGE DeriveGeneric, DeriveAnyClass, OverloadedStrings #-}

module Geocoding where

import GHC.Generics
import Data.Aeson
import Data.ByteString.Lazy.UTF8

import Network.HTTP

data LatLng = LatLng
    { lat :: Double
    , lng :: Double
    } deriving (Show, Generic, ToJSON, FromJSON)

data Location = Location
    { latLng :: LatLng
    , geocodeQuality :: String
    } deriving (Show, Generic, ToJSON, FromJSON)

data GeocodingResult = GeocodingResult {
    locations :: [Location]
} deriving (Show, Generic, ToJSON, FromJSON)

data GeocodingResponse = GeocodingResponse {
    results :: [GeocodingResult]
} deriving (Show, Generic, ToJSON, FromJSON)

data Person = Person {
  name :: String,
  age  :: Int }
  deriving (Show, Generic, ToJSON, FromJSON)

decodePerson = Data.Aeson.decode "{\"name\":\"Joe\",\"age\":12}" :: Maybe Person

decodeGeocodingResponse :: String -> Maybe GeocodingResponse
decodeGeocodingResponse inputResponse = Data.Aeson.decode (fromString inputResponse) :: Maybe GeocodingResponse

geocodeResponseToResults :: String -> Maybe [Maybe LatLng]
geocodeResponseToResults inputResponse =
    latLangs
    where
        decodedResponse :: Maybe GeocodingResponse
        decodedResponse = decodeGeocodingResponse inputResponse

        latLangs = fmap (fmap geocodingResultToLatLng . results) decodedResponse

geocodingResultToLatLng :: GeocodingResult -> Maybe LatLng
geocodingResultToLatLng geocodingResult =
    headLatLng
    where
        qualityFilter :: Location -> Bool
        qualityFilter location = geocodeQuality location == "STREET"

        locs = filter qualityFilter (locations geocodingResult)

        headLatLng :: Maybe LatLng
        headLatLng = if null locs then Nothing else Just (latLng (head locs))


mapQuestUrl :: String -> [String] -> String
mapQuestUrl key addresses =
    mapQuestBatch ++ concat locationPairs
    where
        mapQuestBatch = "http://www.mapquestapi.com/geocoding/v1/batch?key="
            ++ key

        toLocationPair :: String -> String
        toLocationPair address = "&location=" ++ urlEncode (address ++ ", Australia")

        locationPairs :: [String]
        locationPairs = fmap toLocationPair addresses
