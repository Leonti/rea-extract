module ResultsParsing where

import           Data.List
import           Data.List.Split
import           Data.Maybe
import           Models
import           ParsingUtils
import           PriceParsing                     (parsePrice)
import           Text.CSS3.Selectors.Parser
import           Text.CSS3.Selectors.Syntax
import           Text.HTML.TagSoup
import           Text.HTML.TagSoup.Tree
import           Text.HTML.TagSoup.Tree.Selection
import           Text.HTML.TagSoup.Tree.Zipper
import           Text.Read

parsePage :: String -> Either Error [ParsedProperty]
parsePage content = do
  listingTrees <- Right $ maybe [] listings maybeBodyTree
  nestedProperties <- traverse parseListing listingTrees
  return $ concat nestedProperties
    where
        tagTree = parseTree content
        maybeBodyTree = find hasListings tagTree
        maybeListingTrees = fmap listings maybeBodyTree

parseListing :: TagTreePos String -> Either Error [ParsedProperty]
parseListing listingTree = do
  details <- detailsFromListingTree listingTree
  return $ fmap (`combineData` address) details
    where
        address :: String
        address = addressFromListingTree listingTree

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

maybeToEither :: Error -> Maybe a2 -> Either Error a2
maybeToEither = flip maybe Right . Left

extractPropertyDetails :: TagTreePos String -> Either Error PropertyDetails
extractPropertyDetails propertyTree = do
  bathrooms <- maybeToEither ("Can't parse bathrooms " ++ show (content propertyTree)) $ bathroomsAsString >>= readMaybe
  return PropertyDetails
      { bedroomsAsString = bedroomsAsString
      , bathroomsAsString = bathroomsAsString
      , carsAsString = carsAsString
      , bedrooms = fromMaybe 0 (bedroomsAsString >>= readMaybe)
      , bathrooms = bathrooms
      , cars = fromMaybe 0 (carsAsString >>= readMaybe)
      }
  where
      bedroomsAsString = iconToValue $ select (sel ".rui-icon-bed") (content propertyTree)
      bathroomsAsString = iconToValue $ select (sel ".rui-icon-bath") (content propertyTree)
      carsAsString = iconToValue $ select (sel ".rui-icon-car") (content propertyTree)

iconToValue :: [TagTreePos String] -> Maybe String
iconToValue [] = Nothing
iconToValue (x:xs) =
    Just $ innerText $ flattenTree [Text.HTML.TagSoup.Tree.Zipper.after x !! 1]

detailsFromListingTree :: TagTreePos String -> Either Error [(PropertyDetails, Maybe String, String)]
detailsFromListingTree listingTree =
    extractedDetails
    where
        projectChildTrees :: [TagTreePos String]
        projectChildTrees = select (sel "div.project-child-listings") (content listingTree)

        extractedDetails :: Either Error [(PropertyDetails, Maybe String, String)]
        extractedDetails = if not (null projectChildTrees) then
                processProjectChildren $ head projectChildTrees
            else
                traverse processSingleProperty [listingTree]

processSingleProperty :: TagTreePos String -> Either Error (PropertyDetails, Maybe String, String)
processSingleProperty propertyTree = do
  propertyDetails <- extractPropertyDetails propertyTree
  return (propertyDetails, propertyPrice, propertyLink)
    where
        propertyPrice :: Maybe String
        propertyPrice = extractSinglePropertyPrice propertyTree

        propertyLink :: String
        propertyLink = extractSinglePropertyLink propertyTree

processProjectChildren :: TagTreePos String -> Either Error [(PropertyDetails, Maybe String, String)]
processProjectChildren projectChildTree = traverse processProjectChild childrenTrees
    where
        childrenTrees :: [TagTreePos String]
        childrenTrees = select (sel "div.child") (content projectChildTree)

processProjectChild :: TagTreePos String -> Either Error (PropertyDetails, Maybe String, String)
processProjectChild projectChild = do
  propertyDetails <- extractPropertyDetails projectChild
  return (propertyDetails, propertyPrice, propertyLink)
  where
      propertyPrice :: Maybe String
      propertyPrice = extractProjectChildPrice projectChild

      propertyLink :: String
      propertyLink = extractProjectChildLink projectChild

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
