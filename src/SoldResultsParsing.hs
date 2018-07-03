module SoldResultsParsing where

import           Data.List
import           Data.List.Split
import           Data.Maybe
import           Data.String.Utils
import           Data.Time.LocalTime
import           Data.Time.Parse
import           Data.Validation
import           Models
import           ParsingUtils
import           PriceParsing                     (parsePrice)
import           Safe                             (headMay)
import           Text.CSS3.Selectors.Parser
import           Text.CSS3.Selectors.Syntax
import           Text.HTML.TagSoup
import           Text.HTML.TagSoup.Tree
import           Text.HTML.TagSoup.Tree.Selection
import           Text.HTML.TagSoup.Tree.Zipper
import           Text.Read

parseSoldPage :: String -> Either Error [SoldParsedProperty]
parseSoldPage content = do
  listingTrees <- Right $ maybe [] soldListings maybeBodyTree
  traverse parseSoldListing listingTrees
    where
        tagTree = parseTree content
        maybeBodyTree = find hasProperies tagTree
        maybeListingTrees = fmap soldListings maybeBodyTree

parseSoldListing :: TagTreePos String -> Either Error SoldParsedProperty
parseSoldListing listingTree = do
  propertyDetails <- extractPropertyDetails listingTree
  link <- extractPropertyLink listingTree
  address <- addressFromListingTree listingTree
  return SoldParsedProperty
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
        maybePrice :: Maybe Int
        maybePrice = extractPrice ".property-price" listingTree >>= parsePrice

        maybeLocalTime :: Maybe LocalTime
        maybeLocalTime = extractDate listingTree

maybeToEither :: Error -> Maybe a2 -> Either Error a2
maybeToEither = flip maybe Right . Left

extractPropertyLink :: TagTreePos String -> Either Error String
extractPropertyLink tree =
    fromAttrib "href" <$> tagOpenA
    where
      tagOpenA = maybeToEither "Card link is empty" $ do
        link <- headMay (select (sel "a.residential-card__details-link") (content tree))
        headMay $ flattenTree [content link]

extractDate :: TagTreePos String -> Maybe LocalTime
extractDate propertyTree = do
  dateTag <- headMay $ select (sel ".residential-card__with-comma") (content propertyTree)
  toDate dateTag

extractPropertyDetails :: TagTreePos String -> Either Error PropertyDetails
extractPropertyDetails propertyTree =
  return PropertyDetails
    { bedroomsAsString = bedroomsAsString
    , bathroomsAsString = bathroomsAsString
    , carsAsString = carsAsString
    , bedrooms = fromMaybe 0 $ bedroomsAsString >>= readMaybe
    , bathrooms = fromMaybe 0 $ bathroomsAsString >>= readMaybe
    , cars = fromMaybe 0 $ carsAsString >>= readMaybe
    }
    where
      bedroomsAsString = iconToValue $ select (sel ".general-features__beds") (content propertyTree)
      bathroomsAsString = iconToValue $ select (sel ".general-features__baths") (content propertyTree)
      carsAsString = iconToValue $ select (sel ".general-features__cars") (content propertyTree)

iconToValue :: [TagTreePos String] -> Maybe String
iconToValue [] = Nothing
iconToValue (x:xs) =
    Just $ innerText $ flattenTree [content x]

addressFromListingTree :: TagTreePos String -> Either Error String
addressFromListingTree listingTree =
    maybeToEither "Can't extract address" $ headMay streetAddresses
    where
        addressTrees :: [TagTreePos String]
        addressTrees = select (sel ".residential-card__info-text span") (content listingTree)

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
soldListings = select (sel "article.residential-card")

hasProperies :: TagTree String -> Bool
hasProperies tree = not (null (select (sel ".residential-card__with-comma") tree))
