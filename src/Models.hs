module Models where
import           Data.Time.LocalTime

data PropertyDetails = PropertyDetails  { bedroomsAsString  :: Maybe String
                                        , bathroomsAsString :: Maybe String
                                        , carsAsString      :: Maybe String
                                        , bedrooms          :: Int
                                        , bathrooms         :: Int
                                        , cars              :: Int
                                        } deriving (Show)

data ParsedProperty = ParsedProperty
                                { details     :: PropertyDetails
                                , location    :: String
                                , link        :: String
                                , price       :: Maybe Int
                                , priceAsText :: Maybe String
                                } deriving (Show)

data SoldParsedProperty = SoldParsedProperty
                                { soldProperty :: ParsedProperty
                                , soldAt       :: LocalTime
                                } deriving (Show)

type Error = String
