{-# LANGUAGE DeriveGeneric #-}

-- | Types Module
module Types (
    ParsedResponse(..),
    Movie(..),
    MovieDetails(..),
    MovieDetailsCollection(..),
    TVSeriesDetails(..),
    TVSeriesDetailsCreator(..),
    TVSeriesDetailsEpisode(..),
    TVSeriesDetailsNetwork(..),
    TVSeriesDetailsSeason(..),
    DetailsGenre(..),
    DetailsProductionCompany(..),
    DetailsProductionCountry(..),
    DetailsSpokenLanguage(..),
    MovieResponse(..),
    TVSeries(..),
    TVSeriesResponse(..),
    KnownFor(..),
    Person(..),
    PersonResponse(..),
    ParsedPopular(..),
    PopularModel(..),
    PopularResponse(..),
    PersonDetails(..),
    ParsedDetails(..),
    ParsedImage(..),
    ImageModel(..),
    ImageResponse(..),
    PopularFetchModel(..)
) where

import GHC.Generics
import Data.Aeson
import Database.SQLite.Simple
import Database.SQLite.Simple.FromField (FromField (fromField))

-- | ParsedResponse Data module
data ParsedResponse = ParsedMovie (MovieResponse) |
     ParsedTV (TVSeriesResponse) |
     ParsedPerson (PersonResponse) deriving (Show, Generic)

-- | ParsedDetails Data Module
data ParsedDetails = ParsedMovieDetails (Either String MovieDetails) |
    ParsedTVSeriesDetails (Either String TVSeriesDetails) |
    ParsedPersonDetails (Either String PersonDetails) deriving (Show, Generic)

-- | Movie Data
data Movie = Movie
  { movieAdult                  :: Bool
  , movieBackdropPath           :: Maybe String
  , movieId                     :: Int
  , movieOriginalLanguage       :: String
  , movieOriginalTitle          :: String
  , movieOverview               :: String
  , moviePopularity             :: Double
  , moviePosterPath             :: Maybe String
  , movieReleaseDate            :: String
  , movieTitle                  :: String
  , movieVideo                  :: Bool
  , movieVoteAverage            :: Double
  , movieVoteCount              :: Int
  } deriving (Show, Generic)

data MovieResponse = MovieResponse
  { moviePage                   :: Int
  , movieResults                :: [Movie]
  , movieTotalPages             :: Int
  , movieTotalResults           :: Int
  } deriving (Show, Generic)

data MovieDetails = MovieDetails
  { movieDetailsAdult              :: Bool
  , movieDetailsBackdropPath       :: Maybe String
  , movieDetailsBudget            :: Int
  , movieDetailsHomepage          :: Maybe String
  , movieDetailsID                :: Int
  , movieDetailsIMDBId            :: Maybe String
  , movieDetailsOriginalLanguage  :: String
  , movieDetailsOriginalTitle     :: String
  , movieDetailsOverview          :: Maybe String
  , movieDetailsPopularity        :: Double
  , movieDetailsPosterPath        :: Maybe String
  , movieDetailsReleaseDate       :: String
  , movieDetailsRevenue           :: Int
  , movieDetailsRuntime           :: Maybe Int
  , movieDetailsStatus            :: String
  , movieDetailsTagline           :: Maybe String
  , movieDetailsTitle             :: String
  , movieDetailsVideo             :: Bool
  , movieDetailsVoteAverage       :: Double
  , movieDetailsvoteCount         :: Int
  } deriving (Show, Generic)

data MovieDetailsCollection = MovieDetailsCollection
  { movieDetailsCollectionId     :: Int
  , movieDetailsCollectionName   :: String
  , movieDetailsCollectionPosterPath :: Maybe String
  , movieDetailsCollectionBackdropPath :: Maybe String
  } deriving (Show, Generic)

-- | TVSeries Data
data TVSeries = TVSeries { 
    tvSeriesAdult             :: Bool, 
    tvSeriesBackdropPath      :: Maybe String,
    tvSeriesId                :: Int,
    tvSeriesOriginalLanguage  :: String,
    tvSeriesOriginalName      :: String,
    tvSeriesOverview          :: String,
    tvSeriesPopularity        :: Double,
    tvSeriesPosterPath        :: Maybe String,
    tvSeriesFirstAirDate      :: String,
    tvSeriesName              :: String,
    tvSeriesVoteAverage       :: Double,
    tvSeriesVoteCount         :: Int
} deriving (Show, Generic)

data TVSeriesResponse = TVSeriesResponse { 
    tvSeriesPage         :: Int,
    tvSeriesTotalPages   :: Int,
    tvSeriesTotalResults :: Int,
    tvSeriesResults      :: [TVSeries] 
} deriving (Show, Generic)


data TVSeriesDetails = TVSeriesDetails
  { tvSeriesDetailsAdult               :: Bool
  , tvSeriesDetailsBackdropPath        :: Maybe String
  , tvSeriesDetailsFirstAirDate        :: Maybe String
  , tvSeriesDetailsHomepage            :: Maybe String
  , tvSeriesDetailsId                  :: Int
  , tvSeriesDetailsInProduction        :: Bool
  , tvSeriesDetailsLastAirDate         :: Maybe String
  , tvSeriesDetailsName                :: String
  , tvSeriesDetailsNumberOfEpisodes    :: Int
  , tvSeriesDetailsNumberOfSeasons     :: Int
  , tvSeriesDetailsOriginalLanguage    :: String
  , tvSeriesDetailsOriginalName        :: String
  , tvSeriesDetailsOverview            :: String
  , tvSeriesDetailsPopularity          :: Double
  , tvSeriesDetailsPosterPath          :: Maybe String
  , tvSeriesDetailsStatus              :: String
  , tvSeriesDetailsTagline             :: Maybe String
  , tvSeriesDetailsType                :: String
  , tvSeriesDetailsVoteAverage         :: Double
  , tvSeriesDetailsVoteCount           :: Int
  } deriving (Show, Generic)

data TVSeriesDetailsCreator = TVSeriesDetailsCreator
  { creatorId           :: Int
  , creatorCreditId     :: String
  , creatorName         :: String
  , creatorOriginalName :: String
  , creatorGender       :: Int
  , creatorProfilePath  :: Maybe String
  } deriving (Show, Generic)

data TVSeriesDetailsEpisode = TVSeriesDetailsEpisode
  { episodeId             :: Int
  , episodeName           :: String
  , episodeOverview       :: String
  , episodeVoteAverage    :: Double
  , episodeVoteCount      :: Int
  , episodeAirDate        :: Maybe String
  , episodeNumber         :: Int
  , episodeType           :: String
  , episodeProductionCode :: String
  , episodeRuntime        :: Maybe Int
  , episodeSeasonNumber   :: Int
  , episodeShowId         :: Int
  , episodeStillPath      :: Maybe String
  } deriving (Show, Generic)

data TVSeriesDetailsNetwork = TVSeriesDetailsNetwork
  { networkId             :: Int
  , networkLogoPath       :: Maybe String
  , networkName           :: String
  , networkOriginCountry  :: String
  } deriving (Show, Generic)

data TVSeriesDetailsSeason = TVSeriesDetailsSeason
  { seasonAirDate      :: Maybe String
  , seasonEpisodeCount :: Int
  , seasonId           :: Int
  , seasonName         :: String
  , seasonOverview     :: String
  , seasonPosterPath   :: Maybe String
  , seasonNumber       :: Int
  , seasonVoteAverage  :: Double
  } deriving (Show, Generic)

data DetailsGenre = DetailsGenre
  { detailsGenreId   :: Int
  , detailsGenreName :: String
  } deriving (Show, Generic)

data DetailsProductionCompany = DetailsProductionCompany
  { detailsProductionCompanyId    :: Int
  , detailsProductionCompanyLogoPath :: Maybe String
  , detailsProductionCompanyName  :: String
  , detailsProductionCompanyOriginCountry :: String
  } deriving (Show, Generic)

data DetailsProductionCountry = DetailsProductionCountry
  { detailsProductionCountryIso :: String
  , detailsProductionCountryName      :: String
  } deriving (Show, Generic)

data DetailsSpokenLanguage = DetailsSpokenLanguage
  { detailsSpokenLanguageIso :: String
  , detailsSpokenLanguageName     :: String
  , detailsSpokenLanguageEnglishName :: String
  } deriving (Show, Generic)

-- | Person Data
data KnownFor = KnownFor {
    knownForId :: Int,
    knownForTitle :: Maybe String,
    knownForOriginalTitle :: Maybe String,
    knownForName :: Maybe String,
    knownForOriginalName :: Maybe String,
    knownForOverview :: String,
    knownForBackdropPath :: Maybe String,
    knownForPosterPath :: Maybe String,
    knownForMediaType :: String,
    knownForAdult :: Bool,
    knownForOriginalLanguage :: String,
    knownForGenreIds :: [Int],
    knownForPopularity :: Double,
    knownForReleaseDate :: Maybe String,
    knownForFirstAirDate :: Maybe String,
    knownForVoteAverage :: Double,
    knownForVoteCount :: Int,
    knownForOriginCountry :: Maybe [String]
} deriving (Show, Generic)

data Person = Person {
    personAdult :: Bool,
    personGender :: Int,
    personId :: Int,
    personKnownForDepartment :: String,
    personName :: String,
    personOriginalName :: String,
    personPopularity :: Double,
    personProfilePath :: Maybe String
} deriving (Show, Generic)

data PersonResponse = PersonResponse {
    personPage :: Int,
    personResults :: [Person],
    personTotalPages :: Int,
    personTotalResults :: Int
} deriving (Show, Generic)

-- | Popular Data

data ParsedPopular = Popular (Either String PopularResponse)

data PopularResponse = PopularResponse {
    page :: Int,
    results :: [PopularModel],
    total_pages :: Int,
    total_results :: Int
} deriving (Show, Generic)

data PopularModel = PopularModel {
    adult :: Bool,
    gender :: Maybe Int,
    backdrop_path :: Maybe String,
    popular_id :: Int,
    known_for_department :: Maybe String,
    original_language :: Maybe String,
    original_name :: Maybe String,
    original_title :: Maybe String,
    overview :: Maybe String,
    popularity :: Maybe Double,
    profile_path :: Maybe String,
    poster_path :: Maybe String,
    first_air_date :: Maybe String,
    release_date :: Maybe String,
    name :: Maybe String,
    title :: Maybe String,
    video :: Maybe Bool,
    vote_average :: Maybe Double,
    vote_count :: Maybe Int
} deriving (Show, Generic)

data PopularFetchModel = PopularFetchModel {
    popular_poster_path :: Maybe String,
    popular_original_title :: Maybe String,
    popular_overview :: Maybe String,
    popular_context_id :: Int
} deriving (Show, Generic)

instance FromRow PopularFetchModel where
    fromRow = PopularFetchModel <$> field <*> field <*> field <*> field

-- | Person Data

data PersonDetails = PersonDetails
  { personDetailsAdult              :: Bool
  , personDetailsBiography          :: String
  , personDetailsBirthday           :: Maybe String
  , personDetailsDeathday           :: Maybe String
  , personDetailsGender             :: Int
  , personDetailsHomepage           :: Maybe String
  , personDetailsId                 :: Int
  , personDetailsImdbId             :: Maybe String
  , personDetailsKnownForDepartment :: String
  , personDetailsName               :: String
  , personDetailsPlaceOfBirth       :: Maybe String
  , personDetailsPopularity         :: Double
  , personDetailsProfilePath        :: Maybe String
  } deriving (Show, Generic)

-- | Image Data

data ParsedImage = Image (Either String ImageResponse)

data ImageResponse = ImageResponse {
    image_context_id :: Int,
    backdrops :: [ImageModel],
    logos :: [ImageModel],
    posters :: [ImageModel]
} deriving (Show, Generic)

data ImageModel = ImageModel {
  aspect_ratio :: Maybe Double,
  height :: Maybe Int,
  width :: Maybe Int,
  file_path :: Maybe String,
  image_vote_average :: Maybe Double,
  image_vote_count :: Maybe Int
} deriving (Show, Generic)