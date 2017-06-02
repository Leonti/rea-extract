module SoldResultsParsing where

import Text.HTML.TagSoup
import Text.HTML.TagSoup.Tree
import Text.HTML.TagSoup.Tree.Selection
import Data.List.Split
import Data.List
import Text.CSS3.Selectors.Parser
import Text.HTML.TagSoup.Tree.Zipper
import Text.CSS3.Selectors.Syntax
import Data.Maybe
import Text.Read
import Models
import Data.String.Utils
import Data.Time.Parse
import Data.Time.LocalTime
import ParsingUtils
import PriceParsing (parsePrice)

parseSoldPage :: String -> [SoldParsedProperty]
parseSoldPage content =
    fromMaybe [] maybeProperties
    where
        tagTree = parseTree content
        maybeBodyTree = find hasProperies tagTree
        maybeListingTrees = fmap soldListings maybeBodyTree
        maybeProperties = fmap (fmap parseSoldListing) maybeListingTrees


parseSoldListing :: TagTreePos String -> SoldParsedProperty
parseSoldListing listingTree =
    SoldParsedProperty
        { soldProperty = ParsedProperty
            { details = propertyDetails
            , location = address
            , link = link
            , price = maybePrice
            , priceAsText = Nothing
            }
        , soldAt = fromJust maybeLocalTime
        }
    where
        address :: String
        address = addressFromListingTree listingTree

        propertyDetails :: PropertyDetails
        propertyDetails = extractPropertyDetails listingTree

        maybePrice :: Maybe Int
        maybePrice = extractPrice ".property-price" listingTree >>= parsePrice

        maybeLocalTime :: Maybe LocalTime
        maybeLocalTime = extractDate listingTree

        link :: String
        link = extractPropertyLink listingTree

extractPropertyLink :: TagTreePos String -> String
extractPropertyLink tree =
    fromAttrib "href" tagOpenA
    where
        linkTree :: [TagTreePos String]
        linkTree = select (sel "a.property-card__link") (content tree)

        tagOpenA :: Tag String
        tagOpenA = head $ flattenTree [content $ head linkTree]

extractDate :: TagTreePos String -> Maybe LocalTime
extractDate propertyTree = toDate dateTag
    where
        dateTag = head $ select (sel ".property-card__with-comma") (content propertyTree)

extractPropertyDetails :: TagTreePos String -> PropertyDetails
extractPropertyDetails propertyTree =
    PropertyDetails
        { bedroomsAsString = bedroomsAsString
        , bathroomsAsString = bathroomsAsString
        , carsAsString = carsAsString
        , bedrooms = detailToInt bedroomsAsString
        , bathrooms = detailToInt bathroomsAsString
        , cars = detailToInt carsAsString
        }
    where
        bedroomsAsString = iconToValue $ select (sel ".general-features__beds") (content propertyTree)
        bathroomsAsString = iconToValue $ select (sel ".general-features__baths") (content propertyTree)
        carsAsString = iconToValue $ select (sel ".general-features__cars") (content propertyTree)

iconToValue :: [TagTreePos String] -> Maybe String
iconToValue [] = Nothing
iconToValue (x:xs) =
    Just $ innerText $ flattenTree [content x]

addressFromListingTree :: TagTreePos String -> String
addressFromListingTree listingTree =
    head streetAddresses
    where
        addressTrees :: [TagTreePos String]
        addressTrees = select (sel "a.property-card__info-text") (content listingTree)

        streetAddresses :: [String]
        streetAddresses = fmap streetFromAddressTree addressTrees

streetFromAddressTree :: TagTreePos String -> String
streetFromAddressTree addressTree =
    innerText tagList
    where
        tagList :: [Tag String]
        tagList = flattenTree [content addressTree]

toDate :: TagTreePos String -> Maybe LocalTime
toDate tree = fmap fst maybeParsedDate
    where
        flattenedTree = flattenTree [content tree]
        unparsedFullDate = innerText flattenedTree
        dateAsString = replace "Sold on " "" unparsedFullDate
        maybeParsedDate = strptime "%d %b %Y" dateAsString

soldListings :: TagTree String -> [TagTreePos String]
soldListings = select (sel "article.property-card")

hasProperies :: TagTree String -> Bool
hasProperies tree = not (null (select (sel ".property-card__with-comma") tree))
