{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE InstanceSigs #-}

-- | Parse Module
module Parse (
   parseResponse,
   parsePopular,
   parseImage,
   writeDataToFile
) where

import Database(initialiseDB, getMovies, getPeople, getTVSeries, getPopularData)
import Types
import Data.Aeson
import Data.String (fromString)
import GHC.Generics
import Database.SQLite.Simple
import Data.ByteString.Lazy.Char8 as L8

-- | ErrorHandler
import Data.List (isInfixOf)
import Control.Exception
import Utility.Exception ( throwError,
        AppExceptionType(..))
import GHC.IO.FD (stdout)

-- | Field names for parsing
-- Haskell Movie field names to JSON field names
renameMovieFields :: String -> String
renameMovieFields "movieAdult"              = "adult"
renameMovieFields "movieBackdropPath"       = "backdrop_path"
renameMovieFields "movieGenreIds"           = "genre_ids"
renameMovieFields "movieId"                 = "id"
renameMovieFields "movieOriginalLanguage"   = "original_language"
renameMovieFields "movieOriginalTitle"      = "original_title"
renameMovieFields "movieOverview"           = "overview"
renameMovieFields "moviePopularity"         = "popularity"
renameMovieFields "moviePosterPath"         = "poster_path"
renameMovieFields "movieReleaseDate"        = "release_date"
renameMovieFields "movieTitle"              = "title"
renameMovieFields "movieVideo"              = "video"
renameMovieFields "movieVoteAverage"        = "vote_average"
renameMovieFields "movieVoteCount"          = "vote_count"
renameMovieFields "movieTotalPages"         = "total_pages"
renameMovieFields "movieTotalResults"       = "total_results"
renameMovieFields "moviePage"               = "page"
renameMovieFields "movieResults"            = "results"
renameMovieFields other                     = other

-- Custom Movie field mapping
customMovieOptions :: Options
customMovieOptions = defaultOptions { fieldLabelModifier = renameMovieFields }

-- Haskell Movie Details field names to JSON field names
renameMovieDetailsFields :: String -> String
renameMovieDetailsFields "movieDetailsAdult"                  = "adult"
renameMovieDetailsFields "movieDetailsBackdropPath"           = "backdrop_path"
renameMovieDetailsFields "movieDetailsBelongsToCollection"    = "belongs_to_collection"
renameMovieDetailsFields "movieDetailsBudget"                 = "budget"
renameMovieDetailsFields "movieDetailsGenres"                 = "genres"
renameMovieDetailsFields "movieDetailsHomepage"               = "homepage"
renameMovieDetailsFields "movieDetailsID"                     = "id"
renameMovieDetailsFields "movieDetailsIMDBId"                 = "imdb_id"
renameMovieDetailsFields "movieDetailsOriginCountry"          = "origin_country"
renameMovieDetailsFields "movieDetailsOriginalLanguage"       = "original_language"
renameMovieDetailsFields "movieDetailsOriginalTitle"          = "original_title"
renameMovieDetailsFields "movieDetailsOverview"               = "overview"
renameMovieDetailsFields "movieDetailsPopularity"             = "popularity"
renameMovieDetailsFields "movieDetailsPosterPath"             = "poster_path"
renameMovieDetailsFields "movieDetailsProductionCompanies"    = "production_companies"
-- renameMovieDetailsFields "movieDetailsProductionCompanyId"    = "production_companie_id"
renameMovieDetailsFields "movieDetailsProductionCountries"    = "production_countries"
renameMovieDetailsFields "movieDetailsReleaseDate"            = "release_date"
renameMovieDetailsFields "movieDetailsRevenue"                = "revenue"
renameMovieDetailsFields "movieDetailsRuntime"                = "runtime"
renameMovieDetailsFields "movieDetailsSpokenLanguages"        = "spoken_languages"
renameMovieDetailsFields "movieDetailsStatus"                 = "status"
renameMovieDetailsFields "movieDetailsTagline"                = "tagline"
renameMovieDetailsFields "movieDetailsTitle"                  = "title"
renameMovieDetailsFields "movieDetailsVideo"                  = "video"
renameMovieDetailsFields "movieDetailsVoteAverage"            = "vote_average"
renameMovieDetailsFields "movieDetailsvoteCount"              = "vote_count"
renameMovieDetailsFields "movieDetailsStatus"                 = "status"

renameMovieDetailsFields "movieDetailsCollectionId"           = "id"
renameMovieDetailsFields "movieDetailsCollectionName"         = "name"
renameMovieDetailsFields "movieDetailsCollectionPosterPath"   = "poster_path"
renameMovieDetailsFields "movieDetailsCollectionBackdropPath" = "backdrop_path"
renameMovieDetailsFields other                                = other

-- Custom Movie field mapping
customMovieDetailsOptions :: Options
customMovieDetailsOptions = defaultOptions { fieldLabelModifier = renameMovieDetailsFields }

-- Haskell TV Series field names to JSON field names
renameTVSeriesFields :: String -> String
renameTVSeriesFields "tvSeriesAdult"             = "adult"
renameTVSeriesFields "tvSeriesBackdropPath"      = "backdrop_path"
renameTVSeriesFields "tvSeriesGenreIds"          = "genre_ids"
renameTVSeriesFields "tvSeriesId"                = "id"
renameTVSeriesFields "tvSeriesOriginCountry"     = "origin_country"
renameTVSeriesFields "tvSeriesOriginalLanguage"  = "original_language"
renameTVSeriesFields "tvSeriesOriginalName"      = "original_name"
renameTVSeriesFields "tvSeriesOverview"          = "overview"
renameTVSeriesFields "tvSeriesPopularity"        = "popularity"
renameTVSeriesFields "tvSeriesPosterPath"        = "poster_path"
renameTVSeriesFields "tvSeriesFirstAirDate"      = "first_air_date"
renameTVSeriesFields "tvSeriesName"              = "name"
renameTVSeriesFields "tvSeriesVoteAverage"       = "vote_average"
renameTVSeriesFields "tvSeriesVoteCount"         = "vote_count"
renameTVSeriesFields "tvSeriesPage"              = "page"
renameTVSeriesFields "tvSeriesTotalPages"        = "total_pages"
renameTVSeriesFields "tvSeriesTotalResults"      = "total_results"
renameTVSeriesFields "tvSeriesResults"           = "results"
renameTVSeriesFields other                       = other

-- Custom TV Series field mapping
customTVSeriesOptions :: Options
customTVSeriesOptions = defaultOptions { fieldLabelModifier = renameTVSeriesFields }

-- Haskell TV Series Details field names to JSON field names
renameTVSeriesDetailsFields :: String -> String
renameTVSeriesDetailsFields "tvSeriesDetailsAdult"               = "adult"
renameTVSeriesDetailsFields "tvSeriesDetailsBackdropPath"        = "backdrop_path"
renameTVSeriesDetailsFields "tvSeriesDetailsCreatedBy"           = "created_by"
renameTVSeriesDetailsFields "tvSeriesDetailsEpisodeRunTime"      = "episode_run_time"
renameTVSeriesDetailsFields "tvSeriesDetailsFirstAirDate"        = "first_air_date"
renameTVSeriesDetailsFields "tvSeriesDetailsGenres"              = "genres"
renameTVSeriesDetailsFields "tvSeriesDetailsHomepage"            = "homepage"
renameTVSeriesDetailsFields "tvSeriesDetailsId"                  = "id"
renameTVSeriesDetailsFields "tvSeriesDetailsInProduction"        = "in_production"
renameTVSeriesDetailsFields "tvSeriesDetailsLanguages"           = "languages"
renameTVSeriesDetailsFields "tvSeriesDetailsLastAirDate"         = "last_air_date"
renameTVSeriesDetailsFields "tvSeriesDetailsLastEpisodeToAir"    = "last_episode_to_air"
renameTVSeriesDetailsFields "tvSeriesDetailsName"                = "name"
renameTVSeriesDetailsFields "tvSeriesDetailsNextEpisodeToAir"    = "next_episode_to_air"
renameTVSeriesDetailsFields "tvSeriesDetailsNetworks"            = "networks"
renameTVSeriesDetailsFields "tvSeriesDetailsNumberOfEpisodes"    = "number_of_episodes"
renameTVSeriesDetailsFields "tvSeriesDetailsNumberOfSeasons"     = "number_of_seasons"
renameTVSeriesDetailsFields "tvSeriesDetailsOriginCountry"       = "origin_country"
renameTVSeriesDetailsFields "tvSeriesDetailsOriginalLanguage"    = "original_language"
renameTVSeriesDetailsFields "tvSeriesDetailsOriginalName"        = "original_name"
renameTVSeriesDetailsFields "tvSeriesDetailsOverview"            = "overview"
renameTVSeriesDetailsFields "tvSeriesDetailsPopularity"          = "popularity"
renameTVSeriesDetailsFields "tvSeriesDetailsPosterPath"          = "poster_path"
renameTVSeriesDetailsFields "tvSeriesDetailsProductionCompanies" = "production_companies"
renameTVSeriesDetailsFields "tvSeriesDetailsProductionCountries" = "production_countries"
renameTVSeriesDetailsFields "tvSeriesDetailsSeasons"             = "seasons"
renameTVSeriesDetailsFields "tvSeriesDetailsSpokenLanguages"     = "spoken_languages"
renameTVSeriesDetailsFields "tvSeriesDetailsStatus"              = "status"
renameTVSeriesDetailsFields "tvSeriesDetailsTagline"             = "tagline"
renameTVSeriesDetailsFields "tvSeriesDetailsType"                = "type"
renameTVSeriesDetailsFields "tvSeriesDetailsVoteAverage"         = "vote_average"
renameTVSeriesDetailsFields "tvSeriesDetailsVoteCount"           = "vote_count"

renameTVSeriesDetailsFields "creatorId"                          = "id"
renameTVSeriesDetailsFields "creatorCreditId"                    = "credit_id"
renameTVSeriesDetailsFields "creatorName"                        = "name"
renameTVSeriesDetailsFields "creatorOriginalName"                = "original_name"
renameTVSeriesDetailsFields "creatorGender"                      = "gender"
renameTVSeriesDetailsFields "creatorProfilePath"                 = "profile_path"

renameTVSeriesDetailsFields "episodeId"                          = "id"
renameTVSeriesDetailsFields "episodeName"                        = "name"
renameTVSeriesDetailsFields "episodeOverview"                    = "overview"
renameTVSeriesDetailsFields "episodeVoteAverage"                 = "vote_average"
renameTVSeriesDetailsFields "episodeVoteCount"                   = "vote_count"
renameTVSeriesDetailsFields "episodeAirDate"                     = "air_date"
renameTVSeriesDetailsFields "episodeNumber"                      = "episode_number"
renameTVSeriesDetailsFields "episodeType"                        = "episode_type"
renameTVSeriesDetailsFields "episodeProductionCode"              = "production_code"
renameTVSeriesDetailsFields "episodeRuntime"                     = "runtime"
renameTVSeriesDetailsFields "episodeSeasonNumber"                = "season_number"
renameTVSeriesDetailsFields "episodeShowId"                      = "show_id"
renameTVSeriesDetailsFields "episodeStillPath"                   = "still_path"

renameTVSeriesDetailsFields "networkId"                          = "id"
renameTVSeriesDetailsFields "networkLogoPath"                    = "logo_path"
renameTVSeriesDetailsFields "networkName"                        = "name"
renameTVSeriesDetailsFields "networkOriginCountry"               = "origin_country"

renameTVSeriesDetailsFields "seasonAirDate"                      = "air_date"
renameTVSeriesDetailsFields "seasonEpisodeCount"                 = "episode_count"
renameTVSeriesDetailsFields "seasonId"                           = "id"
renameTVSeriesDetailsFields "seasonName"                         = "name"
renameTVSeriesDetailsFields "seasonOverview"                     = "overview"
renameTVSeriesDetailsFields "seasonPosterPath"                   = "poster_path"
renameTVSeriesDetailsFields "seasonNumber"                       = "season_number"
renameTVSeriesDetailsFields "seasonVoteAverage"                  = "vote_average"

renameTVSeriesDetailsFields other                                = other

-- Custom Details field mapping
customTVSeriesDetailsOptions :: Options
customTVSeriesDetailsOptions = defaultOptions { fieldLabelModifier = renameTVSeriesDetailsFields }

-- Custom Details field mapping
renameDetailsFields "detailsGenreId"                         = "id"
renameDetailsFields "detailsGenreName"                       = "name"
renameDetailsFields "detailsProductionCompanyId"             = "id" -- CHECK LATER
renameDetailsFields "detailsProductionCompanyLogoPath"       = "logo_path"
renameDetailsFields "detailsProductionCompanyName"           = "name"
renameDetailsFields "detailsProductionCompanyOriginCountry"  = "origin_country"

renameDetailsFields "detailsProductionCountryIso"             = "iso_3166_1"
renameDetailsFields "detailsProductionCountryName"            = "name"

renameDetailsFields "detailsSpokenLanguageIso"            = "iso_639_1"
renameDetailsFields "detailsSpokenLanguageName"           = "name"
renameDetailsFields "detailsSpokenLanguageEnglishName"    = "english_name"
renameDetailsFields remaining = remaining

-- Custom Details field mapping
customDetailsOptions :: Options
customDetailsOptions = defaultOptions { fieldLabelModifier = renameDetailsFields }

-- JSON field names to Haskell Person field names
renamePersonFields :: String -> String
renamePersonFields "personAdult"              = "adult"
renamePersonFields "personGender"             = "gender"
renamePersonFields "personId"                 = "id"
renamePersonFields "personKnownForDepartment" = "known_for_department"
renamePersonFields "personName"               = "name"
renamePersonFields "personOriginalName"       = "original_name"
renamePersonFields "personPopularity"         = "popularity"
renamePersonFields "personProfilePath"        = "profile_path"
renamePersonFields "personKnownFor"           = "known_for"
renamePersonFields other                      = other

-- Custom Person field mapping
customPersonOptions :: Options
customPersonOptions = defaultOptions { fieldLabelModifier = renamePersonFields }

-- JSON field names to Haskell KnownFor field names
renameKnownForFields :: String -> String
renameKnownForFields "knownForId"               = "id"
renameKnownForFields "knownForTitle"            = "title"
renameKnownForFields "knownForOriginalTitle"    = "original_title"
renameKnownForFields "knownForName"             = "name"
renameKnownForFields "knownForOriginalName"     = "original_name"
renameKnownForFields "knownForOverview"         = "overview"
renameKnownForFields "knownForBackdropPath"     = "backdrop_path"
renameKnownForFields "knownForPosterPath"       = "poster_path"
renameKnownForFields "knownForMediaType"        = "media_type"
renameKnownForFields "knownForAdult"            = "adult"
renameKnownForFields "knownForOriginalLanguage" = "original_language"
renameKnownForFields "knownForGenreIds"         = "genre_ids"
renameKnownForFields "knownForPopularity"       = "popularity"
renameKnownForFields "knownForReleaseDate"      = "release_date"
renameKnownForFields "knownForFirstAirDate"     = "first_air_date"
renameKnownForFields "knownForVoteAverage"      = "vote_average"
renameKnownForFields "knownForVoteCount"        = "vote_count"
renameKnownForFields "knownForOriginCountry"    = "origin_country"
renameKnownForFields other                      = other

-- Custom KnownFor field mapping
customKnownForOptions :: Options
customKnownForOptions = defaultOptions { fieldLabelModifier = renameKnownForFields }

-- JSON field names to Haskell Person Response field names
renamePersonResponseFields :: String -> String
renamePersonResponseFields "personTotalPages"     = "total_pages"
renamePersonResponseFields "personTotalResults"   = "total_results"
renamePersonResponseFields "personPage"           = "page"
renamePersonResponseFields "personResults"        = "results"
renamePersonResponseFields other                  = other

-- Custom Person Response field mapping
customPersonResponseOptions :: Options
customPersonResponseOptions = defaultOptions { fieldLabelModifier = renamePersonResponseFields }

-- JSON field names to Haskell Person Details field names
customPersonDetailsResponseFields :: String -> String
customPersonDetailsResponseFields "personDetailsAdult"                   = "adult"
customPersonDetailsResponseFields "personDetailsAlsoKnownAs"             = "also_known_as"
customPersonDetailsResponseFields "personDetailsBiography"               = "biography"
customPersonDetailsResponseFields "personDetailsBirthday"                = "birthday"
customPersonDetailsResponseFields "personDetailsDeathday"                = "deathday"
customPersonDetailsResponseFields "personDetailsGender"                  = "gender"
customPersonDetailsResponseFields "personDetailsHomepage"                = "homepage"
customPersonDetailsResponseFields "personDetailsId"                      = "id"
customPersonDetailsResponseFields "personDetailsImdbId"                  = "imdb_id"
customPersonDetailsResponseFields "personDetailsKnownForDepartment"      = "known_for_department"
customPersonDetailsResponseFields "personDetailsName"                    = "name"
customPersonDetailsResponseFields "personDetailsPlaceOfBirth"            = "place_of_birth"
customPersonDetailsResponseFields "personDetailsPopularity"              = "popularity"
customPersonDetailsResponseFields "personDetailsProfilePath"             = "profile_path"
customPersonDetailsResponseFields other                                  = other

-- Custom Person Person Details field mapping
customPersonDetailsResponseOptions :: Options
customPersonDetailsResponseOptions = defaultOptions { fieldLabelModifier = customPersonDetailsResponseFields }

-- | Generic parsing

-- JSON instances using the custom Movie options
instance FromJSON Movie where
    parseJSON = genericParseJSON customMovieOptions
instance ToJSON Movie

instance FromJSON MovieResponse where
    parseJSON = genericParseJSON customMovieOptions
instance ToJSON MovieResponse

-- JSON instances using the custom Movie Details options
instance FromJSON MovieDetails where
    parseJSON = genericParseJSON customMovieDetailsOptions
instance ToJSON MovieDetails

instance FromJSON DetailsGenre where
    parseJSON = genericParseJSON customDetailsOptions
instance ToJSON DetailsGenre

instance FromJSON MovieDetailsCollection where
    parseJSON = genericParseJSON customMovieDetailsOptions
instance ToJSON MovieDetailsCollection

instance FromJSON DetailsProductionCompany where
    parseJSON = genericParseJSON customDetailsOptions
instance ToJSON DetailsProductionCompany

instance FromJSON DetailsProductionCountry where
    parseJSON = genericParseJSON customDetailsOptions
instance ToJSON DetailsProductionCountry

instance FromJSON DetailsSpokenLanguage where
    parseJSON = genericParseJSON customDetailsOptions
instance ToJSON DetailsSpokenLanguage

-- JSON instances using the custom TVSeries Details options
instance FromJSON TVSeriesDetails where
    parseJSON = genericParseJSON customTVSeriesDetailsOptions
instance ToJSON TVSeriesDetails

instance FromJSON TVSeriesDetailsCreator where
    parseJSON = genericParseJSON customTVSeriesDetailsOptions
instance ToJSON TVSeriesDetailsCreator

instance FromJSON TVSeriesDetailsEpisode where
    parseJSON = genericParseJSON customTVSeriesDetailsOptions
instance ToJSON TVSeriesDetailsEpisode

instance FromJSON TVSeriesDetailsNetwork where
    parseJSON = genericParseJSON customTVSeriesDetailsOptions
instance ToJSON TVSeriesDetailsNetwork

instance FromJSON TVSeriesDetailsSeason where
    parseJSON = genericParseJSON customTVSeriesDetailsOptions
instance ToJSON TVSeriesDetailsSeason

-- JSON instances using the custom TV Series options
instance FromJSON TVSeries where
    parseJSON = genericParseJSON customTVSeriesOptions
instance ToJSON TVSeries

instance FromJSON TVSeriesResponse where
    parseJSON = genericParseJSON customTVSeriesOptions
instance ToJSON TVSeriesResponse

-- JSON instances using the custom Person options
instance FromJSON PersonResponse where
    parseJSON = genericParseJSON customPersonResponseOptions
instance ToJSON PersonResponse

-- JSON instances using the custom Person Response options
instance FromJSON Person where
    parseJSON = genericParseJSON customPersonOptions
instance ToJSON Person

instance FromJSON PersonDetails where
    parseJSON = genericParseJSON customPersonDetailsResponseOptions
instance ToJSON PersonDetails

-- JSON instances using the custom Known For options
instance FromJSON KnownFor where
    parseJSON = genericParseJSON customKnownForOptions
instance ToJSON KnownFor

-- | Parsing Function
parseResponse :: [Char] -> L8.ByteString -> ParsedDetails
parseResponse collection
    | "Movie" `isInfixOf` collection = parseMovieResponse
    | "TV" `isInfixOf` collection = parseTVSeriesResponse
    | "People" `isInfixOf` collection = parsePersonResponse
    -- | otherwise = throw (AppException "Error Occurred")

-- | Decoders

parsePopular :: L8.ByteString -> ParsedPopular
parsePopular json = Popular $ eitherDecode json

parseImage :: L8.ByteString -> ParsedImage
parseImage json = Image $ eitherDecode json

parseMovieResponse :: L8.ByteString -> ParsedDetails
parseMovieResponse json = ParsedMovieDetails $ eitherDecode json

parseTVSeriesResponse :: L8.ByteString -> ParsedDetails
parseTVSeriesResponse json = ParsedTVSeriesDetails $ eitherDecode json

parsePersonResponse :: L8.ByteString -> ParsedDetails
parsePersonResponse json = ParsedPersonDetails $ eitherDecode json

-- | Json parsing

instance FromJSON PopularResponse where
    parseJSON = genericParseJSON poularOptions
instance ToJSON PopularResponse

instance FromJSON PopularModel where
    parseJSON = genericParseJSON poularOptions
instance ToJSON PopularModel

instance FromJSON ImageResponse where
    parseJSON = genericParseJSON imageOptions
instance ToJSON ImageResponse

instance FromJSON ImageModel where
    parseJSON = genericParseJSON imageModelOptions
instance ToJSON ImageModel

instance FromJSON PopularFetchModel
instance ToJSON PopularFetchModel

-- | Options

poularOptions :: Options
poularOptions = defaultOptions { fieldLabelModifier = renamePopularId }

imageOptions :: Options
imageOptions = defaultOptions { fieldLabelModifier = renameImageId }

imageModelOptions :: Options
imageModelOptions = defaultOptions { fieldLabelModifier = renameImageVotes }


renamePopularId :: String -> String
renamePopularId "popular_id"             = "id"
renamePopularId remaining = remaining

renameImageId :: String -> String
renameImageId "image_context_id"             = "id"
renameImageId "image_vote_average"             = "vote_average"
renameImageId "image_vote_count"             = "vote_count"
renameImageId remaining = remaining

renameImageVotes :: String -> String
renameImageVotes "image_vote_average"             = "vote_average"
renameImageVotes "image_vote_count"             = "vote_count"
renameImageVotes remaining = remaining

-- | Function to write parsed data to a file
writeDataToFile :: IO ()
writeDataToFile = do
    connection <- initialiseDB
    dbPopular <- getPopularData connection
    let popularJSON = encode $ object [fromString "popular" .= dbPopular]
    L8.writeFile "data.json" popularJSON