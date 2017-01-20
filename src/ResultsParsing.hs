module ResultsParsing where

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
import PriceParsing (parsePrice)

data PropertyDetails = PropertyDetails  { bedroomsAsString :: Maybe String
                                        , bathroomsAsString :: Maybe String
                                        , carsAsString :: Maybe String
                                        , bedrooms :: Int
                                        , bathrooms :: Int
                                        , cars :: Int
                                        } deriving (Show)

data ParsedProperty = ParsedProperty
                                { details :: PropertyDetails
                                , location :: String
                                , link :: String
                                , price :: Maybe Int
                                , priceAsText :: Maybe String
                                } deriving (Show)

parsePage :: String -> [ParsedProperty]
parsePage content =
    fromMaybe [] maybeProperties
    where
        tagTree = parseTree content
        maybeBodyTree = find hasListings tagTree
        maybeListingTrees = fmap listings maybeBodyTree
        maybeProperties = fmap (concatMap parseListing) maybeListingTrees


parseListing :: TagTreePos String -> [ParsedProperty]
parseListing listingTree =
    fmap (`combineData` address) details
    where
        address :: String
        address = addressFromListingTree listingTree

        details :: [(PropertyDetails, Maybe String, String)]
        details = detailsFromListingTree listingTree

        combineData :: (PropertyDetails, Maybe String, String) -> String -> ParsedProperty
        combineData (propertyDetails, maybePriceAsText, link) address =
            ParsedProperty
                { details = propertyDetails
                , priceAsText = maybePriceAsText
                , price = maybePrice
                , link = link
                , location = address
            }
            where
                maybePrice :: Maybe Int
                maybePrice = maybePriceAsText >>= parsePrice

addressFromListingTree :: TagTreePos String -> String
addressFromListingTree listingTree =
    head streetAddresses
    where
        addressTrees :: [TagTreePos String]
        addressTrees = select (sel "a[rel=listingName]") (content listingTree)

        streetAddresses :: [String]
        streetAddresses = fmap streetFromAddressTree addressTrees

detailsFromListingTree :: TagTreePos String -> [(PropertyDetails, Maybe String, String)]
detailsFromListingTree listingTree =
    extractedDetails
    where
        projectChildTrees :: [TagTreePos String]
        projectChildTrees = select (sel "div.project-child-listings") (content listingTree)

        extractedDetails :: [(PropertyDetails, Maybe String, String)]
        extractedDetails = if not (null projectChildTrees) then
                processProjectChildren $ head projectChildTrees
            else
                [processSingleProperty listingTree]

processSingleProperty :: TagTreePos String -> (PropertyDetails, Maybe String, String)
processSingleProperty propertyTree =
    (propertyDetails, propertyPrice, propertyLink)
    where
        propertyDetails :: PropertyDetails
        propertyDetails = extractPropertyDetails propertyTree

        propertyPrice :: Maybe String
        propertyPrice = extractSinglePropertyPrice propertyTree

        propertyLink :: String
        propertyLink = extractSinglePropertyLink propertyTree

processProjectChildren :: TagTreePos String -> [(PropertyDetails, Maybe String, String)]
processProjectChildren projectChildTree =
    processedProjectChildren
    where
        childrenTrees :: [TagTreePos String]
        childrenTrees = select (sel "div.child") (content projectChildTree)

        processedProjectChildren :: [(PropertyDetails, Maybe String, String)]
        processedProjectChildren = fmap processProjectChild childrenTrees

processProjectChild :: TagTreePos String -> (PropertyDetails, Maybe String, String)
processProjectChild projectChild =
    (propertyDetails, propertyPrice, propertyLink)
    where
        propertyDetails :: PropertyDetails
        propertyDetails = extractPropertyDetails projectChild

        propertyPrice :: Maybe String
        propertyPrice = extractProjectChildPrice projectChild

        propertyLink :: String
        propertyLink = extractProjectChildLink projectChild

extractPrice :: String -> TagTreePos String -> Maybe String
extractPrice selector tree =
    if not (null priceTree) then
        Just $ innerText $ flattenTree [content $ head priceTree]
    else
        Nothing
    where
        priceTree :: [TagTreePos String]
        priceTree = select (sel selector) (content tree)

extractProjectChildPrice :: TagTreePos String -> Maybe String
extractProjectChildPrice = extractPrice ".price"

extractSinglePropertyPrice :: TagTreePos String -> Maybe String
extractSinglePropertyPrice = extractPrice ".priceText"

extractProjectChildLink :: TagTreePos String -> String
extractProjectChildLink projectChildTree =
    fromAttrib "href" tagOpenA
    where
        parentTree :: TagTreePos String
        parentTree = fromJust $ parent projectChildTree

        tagOpenA :: Tag String
        tagOpenA = head $ flattenTree [content parentTree]

extractSinglePropertyLink :: TagTreePos String -> String
extractSinglePropertyLink tree =
    fromAttrib "href" tagOpenA
    where
        linkTree :: [TagTreePos String]
        linkTree = select (sel ".photoviewer a") (content tree)

        tagOpenA :: Tag String
        tagOpenA = head $ flattenTree [content $ head linkTree]

extractHref :: Tag String -> String
extractHref tagOpen = ""

iconToValue :: [TagTreePos String] -> Maybe String
iconToValue [] = Nothing
iconToValue (x:xs) =
    Just $ innerText $ flattenTree [Text.HTML.TagSoup.Tree.Zipper.after x !! 1]

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
        bedroomsAsString = iconToValue $ select (sel ".rui-icon-bed") (content propertyTree)
        bathroomsAsString = iconToValue $ select (sel ".rui-icon-bath") (content propertyTree)
        carsAsString = iconToValue $ select (sel ".rui-icon-car") (content propertyTree)

detailToInt :: Maybe String -> Int
detailToInt maybeDetail =
    maybeToInt maybeIntDetail
    where
        detailToMaybeInt :: String -> Maybe Int
        detailToMaybeInt = readMaybe

        maybeIntDetail = maybeDetail >>= detailToMaybeInt

        maybeToInt :: Maybe Int -> Int
        maybeToInt (Just detail) = detail
        maybeToInt Nothing = 0

streetFromAddressTree :: TagTreePos String -> String
streetFromAddressTree addressTree =
    innerText tagList
    where
        tagList :: [Tag String]
        tagList = flattenTree [content addressTree]

listings :: TagTree String -> [TagTreePos String]
listings = select (sel "article.resultBody")

propertyFeaturesSelector :: Selector
propertyFeaturesSelector = sel "dl.rui-property-features"

propertyFeaturesTree :: TagTreePos String -> [TagTreePos String]
propertyFeaturesTree listingTree = select propertyFeaturesSelector (content listingTree)

hasListings :: TagTree String -> Bool
hasListings tree = not (null (select (sel "article.resultBody") tree))
