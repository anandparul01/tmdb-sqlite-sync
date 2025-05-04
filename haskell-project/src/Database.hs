{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | Database Module
module Database (
    initialiseDB,
    createTables,
    save,
    store,
    insertMovie,
    insertTVSeries,
    insertPerson,
    persist,
    savePopular,
    getIdentifiersFromDB,
    getMovies,
    getTVSeries,
    getPeople,
    getPopulars,
    getPopularByKeyword,
    validatePopularQuery,
    getPopularData
) where


import Utility.Logger (logDebug)
import Database.SQLite.Simple
import Types
import System.IO ()
import Data.List (isInfixOf)
import Data.Char (toLower)

-- | Instances
instance FromRow TVSeriesDetails where
    fromRow = TVSeriesDetails <$> field <*> field <*> field <*> field <*> field <*> field <*> field  <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field
    
    
instance FromRow MovieDetails where 
    fromRow = MovieDetails <$> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field

instance FromRow PersonDetails where 
    fromRow = PersonDetails <$> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field

-- | Initialising DB
initialiseDB :: IO Connection
initialiseDB = open "haskell-project.db"

-- | Creating Tables
createTables :: Connection -> IO ()
createTables conn = do

    execute_ conn "CREATE TABLE IF NOT EXISTS popular (\
        \pid INTEGER AUTO_INCREMENT PRIMARY KEY, \
        \collection TEXT DEFAULT NULL, \
        \adult BOOL DEFAULT NULL, \
        \gender INTEGER DEFAULT NULL, \
        \backdrop_path TEXT DEFAULT NULL, \
        \popular_id INTEGER DEFAULT NULL, \
        \known_for_department TEXT DEFAULT NULL, \
        \original_language TEXT DEFAULT NULL, \
        \original_name TEXT DEFAULT NULL, \
        \original_title TEXT DEFAULT NULL, \
        \overview TEXT DEFAULT NULL, \
        \popularity DOUBLE DEFAULT NULL, \
        \profile_path TEXT DEFAULT NULL, \
        \poster_path TEXT DEFAULT NULL, \
        \first_air_date TEXT DEFAULT NULL, \         
        \release_date TEXT DEFAULT NULL, \
        \name TEXT DEFAULT NULL, \
        \title TEXT DEFAULT NULL, \
        \video BOOL DEFAULT NULL, \
        \vote_average DOUBLE DEFAULT NULL, \
        \vote_count INTEGER DEFAULT NULL\
    \)"

    execute_ conn "CREATE TABLE IF NOT EXISTS movie (\
        \mid INTEGER AUTO_INCREMENT PRIMARY KEY, \
        \movieId INTEGER DEFAULT NULL, \
        \movieAdult BOOL DEFAULT NULL, \
        \movieBackdropPath TEXT DEFAULT NULL, \
        \movieBelongsToCollection TEXT DEFAULT NULL, \
        \movieBudget INT DEFAULT NULL, \
        \movieHomepage TEXT DEFAULT NULL, \
        \movieImdbID TEXT DEFAULT NULL, \
        \movieOriginCountry TEXT DEFAULT NULL, \
        \movieOriginalLanguage TEXT DEFAULT NULL, \
        \movieOriginalTitle TEXT DEFAULT NULL, \
        \movieOverview TEXT DEFAULT NULL, \
        \moviePopularity FLOAT DEFAULT NULL, \
        \moviePosterPath TEXT DEFAULT NULL, \
        \movieProductionCompanies TEXT DEFAULT NULL, \
        \movieProductionCountries TEXT DEFAULT NULL, \         
        \movieReleaseDate TEXT DEFAULT NULL, \
        \movieRevenue INT DEFAULT NULL, \
        \movieRuntime INT DEFAULT NULL, \
        \movieSpokenLanguages TEXT DEFAULT NULL, \
        \movieStatus TEXT DEFAULT NULL, \
        \movieTagline TEXT DEFAULT NULL, \
        \movieTitle TEXT DEFAULT NULL, \
        \movieVideo BOOL DEFAULT NULL, \
        \movieVoteAverage FLOAT DEFAULT NULL, \       
        \movieVoteCount INT DEFAULT NULL\
    \)"

    execute_ conn "CREATE TABLE IF NOT EXISTS tv (\
        \tid INTEGER AUTO_INCREMENT PRIMARY KEY, \
        \tvSeriesId INT DEFAULT NULL,\
        \tvSeriesName STRING DEFAULT NULL,\
        \tvSeriesFirstAirDate,\
        \tvSeriesPosterPath STRING DEFAULT NULL,\
        \tvSeriesOriginalName STRING DEFAULT NULL,\
        \tvSeriesPopularity STRING DEFAULT NULL,\
        \CONSTRAINT tvSeriesId UNIQUE(tvSeriesId) ON CONFLICT IGNORE\
    \)"

    execute_ conn "CREATE TABLE IF NOT EXISTS person (\
        \pid INTEGER AUTO_INCREMENT PRIMARY KEY, \
        \personId INTEGER DEFAULT NULL, \
        \personAdult BOOL DEFAULT NULL, \
        \personGender INT DEFAULT NULL, \
        \personKnownForDepartment TEXT DEFAULT NULL, \
        \personName,\
        \personPlaceOfBirth TEXT DEFAULT NULL\
\)"

    execute_ conn "CREATE TABLE IF NOT EXISTS moviePerson (\
        \personId INT,\
        \movieId INT,\
        \PRIMARY KEY (movieId, personId),\
        \FOREIGN KEY (movieId) REFERENCES movie(movieId),\
        \FOREIGN KEY (personId) REFERENCES person(personId)\
\)"

    execute_ conn "CREATE TABLE IF NOT EXISTS tvSeriesPerson (\
        \tvSeriesId INT,\
        \personId INT,\
        \PRIMARY KEY (tvSeriesId, personId),\
        \FOREIGN KEY (tvSeriesId) REFERENCES tvSeries(tvSeriesId),\
        \FOREIGN KEY (personId) REFERENCES person(personId)\
\)"

    execute_ conn "CREATE TABLE IF NOT EXISTS image_posters (\
        \image_context_id INT,\
        \image_collection TEXT,\
        \aspect_ratio DOUBLE DEFAULT NULL,\
        \height INT DEFAULT NULL,\
        \width INT DEFAULT NULL,\
        \file_path TEXT DEFAULT NULL,\
        \image_vote_average DOUBLE DEFAULT NULL,\
        \image_vote_count INT DEFAULT NULL\
\)"

    execute_ conn "CREATE TABLE IF NOT EXISTS image_backdrops (\
        \image_context_id INT,\
        \image_collection TEXT,\
        \aspect_ratio DOUBLE DEFAULT NULL,\
        \height INT DEFAULT NULL,\
        \width INT DEFAULT NULL,\
        \file_path TEXT DEFAULT NULL,\
        \image_vote_average DOUBLE DEFAULT NULL,\
        \image_vote_count INT DEFAULT NULL\
\)"

    execute_ conn "CREATE TABLE IF NOT EXISTS image_logos (\
        \image_context_id INT,\
        \image_collection TEXT,\
        \aspect_ratio DOUBLE DEFAULT NULL,\
        \height INT DEFAULT NULL,\
        \width INT DEFAULT NULL,\
        \file_path TEXT DEFAULT NULL,\
        \image_vote_average DOUBLE DEFAULT NULL,\
        \image_vote_count INT DEFAULT NULL\
\)"

-- | Functions for getting the data
getPopulars :: Connection -> IO [PopularFetchModel]
getPopulars conn = do
    query_ conn "SELECT poster_path, original_title, overview, popular_id FROM popular WHERE collection LIKE '%Movie%' OR collection LIKE '%TV%'" :: IO [PopularFetchModel]

getPopularData :: Connection -> IO [PopularFetchModel]
getPopularData conn = do
    query_ conn "SELECT poster_path, original_title, overview, popular_id FROM popular" :: IO [PopularFetchModel]


getMovies :: Connection -> IO [MovieDetails]
getMovies conn = do
    query_ conn "SELECT * FROM movie" :: IO [MovieDetails]


getTVSeries :: Connection -> IO [TVSeriesDetails]
getTVSeries conn = do
    query_ conn "SELECT * FROM tv" :: IO [TVSeriesDetails]


getPeople :: Connection -> IO [PersonDetails]
getPeople conn = do
    query_ conn "SELECT * FROM person" :: IO [PersonDetails]

getMovieIds :: IO [PopularFetchModel]
getMovieIds = do
    conn <- initialiseDB
    query_ conn "SELECT poster_path, original_title, overview, popular_id FROM popular WHERE collection LIKE '%Movie%'" :: IO [PopularFetchModel]

getTVIds :: IO [PopularFetchModel]
getTVIds = do
    conn <- initialiseDB
    query_ conn "SELECT poster_path, original_name, overview, popular_id FROM popular WHERE collection LIKE '%TV%'" :: IO [PopularFetchModel]

getPeopleIds :: IO [PopularFetchModel]
getPeopleIds = do
    conn <- initialiseDB
    query_ conn "SELECT profile_path, name, known_for_department, popular_id FROM popular WHERE collection LIKE '%People%'" :: IO [PopularFetchModel]

getPopularByKeyword :: String -> IO [PopularFetchModel]
getPopularByKeyword keyword = do
    conn <- initialiseDB
    let qKeyword = "%" ++ keyword ++ "%"
    let qSQL = "SELECT poster_path, original_title, overview, popular_id FROM popular WHERE original_title LIKE ?"
    query conn qSQL (Only qKeyword) :: IO [PopularFetchModel]

getIdentifiersFromDB :: String -> IO [PopularFetchModel]
getIdentifiersFromDB collection
    | "Movie" `isInfixOf` collection = getMovieIds
    | "TV" `isInfixOf` collection = getTVIds
    | "People" `isInfixOf` collection = getPeopleIds
    | otherwise = return []

-- | Functions for saving the data
validatePopularQuery :: String -> String
validatePopularQuery collection
    | "movie" == map toLower collection = "Popular: Movie"
    | "tv" == map toLower collection = "Popular: TV"
    | "people" == map toLower collection = "Popular: People"
    | otherwise = "Invalid"

save :: ParsedDetails -> IO ()
save (ParsedPersonDetails a) = do
    case a of
        Left failure -> 
            logDebug $ "Error Occurred in Parsing [PersonDetails]: " ++ failure
        Right success -> do
            logDebug "Parse OK"
            saveEachPerson success
            logDebug "Database OK [person]"

save (ParsedTVSeriesDetails a) = do
    case a of
        Left failure -> 
            logDebug $ "Error Occurred in Parsing [TVSeriesDetails]: " ++ failure
        Right success -> do
            logDebug "Parse OK"
            saveEachTV success
            logDebug "Database OK [tv]"

save (ParsedMovieDetails a) = do
    case a of
        Left failure -> 
            logDebug $ "Error Occurred in Parsing [MovieDetails]: " ++ failure
        Right success -> do
            logDebug "Parse OK"
            saveEachMovie success
            logDebug "Database OK [movie]"

saveEachPerson :: PersonDetails -> IO ()
saveEachPerson response = do
    connection <- initialiseDB
    insertPerson connection response

saveEachTV :: TVSeriesDetails -> IO ()
saveEachTV response = do
    connection <- initialiseDB
    insertTVSeries connection response

saveEachMovie :: MovieDetails -> IO ()
saveEachMovie response = do
    connection <- initialiseDB
    insertMovie connection response

insertMovie :: Connection -> MovieDetails -> IO ()
insertMovie conn movie = do
    execute conn
        "INSERT INTO movie (movieId, movieAdult,movieReleaseDate, moviePosterPath,moviePopularity,movieOriginalTitle) VALUES (?,?,?,?,?,?)"
        (movieDetailsID movie, movieDetailsAdult movie,movieDetailsReleaseDate movie, movieDetailsPosterPath movie,movieDetailsPopularity movie,movieDetailsOriginalTitle movie)
insertTVSeries :: Connection -> TVSeriesDetails -> IO ()
insertTVSeries conn tvSeries = do
    execute conn
        "INSERT INTO tv (tvSeriesId, tvSeriesName, tvSeriesFirstAirDate, tvSeriesPosterPath, tvSeriesOriginalName,tvSeriesPopularity) VALUES (?,?,?,?,?,?)"
        (tvSeriesDetailsId tvSeries, tvSeriesDetailsName tvSeries, tvSeriesDetailsFirstAirDate tvSeries, tvSeriesDetailsPosterPath tvSeries, tvSeriesDetailsOriginalName tvSeries,tvSeriesDetailsPopularity tvSeries)
insertPerson :: Connection -> PersonDetails -> IO ()
insertPerson conn person = do
    execute conn
        "INSERT INTO person (personId, personAdult, personGender, personName,  personPlaceOfBirth) VALUES (?,?,?,?,?)"
        (personDetailsId person, personDetailsAdult person, personDetailsGender person , personDetailsName person,  personDetailsPlaceOfBirth person)

persist :: ParsedPopular -> [Char] -> IO ()
persist (Popular a) collection = do
    case a of
        Left failure -> 
            logDebug $ "Error Occurred in Parsing [PopularModel]: " ++ failure
        Right success -> do
            logDebug "Parse OK"
            savePopular success collection

savePopular :: PopularResponse -> [Char] -> IO ()
savePopular response collection = do
    saveEachPopular (results response) collection

saveEachPopular :: [PopularModel] -> [Char] -> IO ()
saveEachPopular [] _ = logDebug "Database OK [popular]"
saveEachPopular (current:remaining) collection = do
    
    connection <- initialiseDB
    
    insertPopular connection current collection

    saveEachPopular remaining collection

insertPopular :: Connection -> PopularModel -> [Char] -> IO ()
insertPopular conn popular collection = do
    execute conn
        "INSERT INTO popular (poster_path, gender, profile_path, popular_id, collection, name, original_name, original_title, overview, known_for_department) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?)"
        (poster_path popular, gender popular, profile_path popular, popular_id popular, collection, name popular, original_name popular, original_title popular, overview popular, known_for_department popular)
        -- "INSERT INTO popular (adult, gender, profile_path, popular_id, known_for_department, name, original_name, original_title, overview, popularity , profile_path, poster_path, first_air_date,release_date, name, title, video, vote_average, vote_count) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)"
        -- (show $ adult popular, show $ gender popular, show $ profile_path popular, popular_id popular, show $ known_for_department popular, show $ name popular, show $ original_name popular, show $ original_title popular, show $ overview popular, show $ popularity popular, show $ profile_path popular, show $ poster_path popular, show $ first_air_date popular, show $ release_date popular, show $ name popular, show $ title popular, show $ video popular, show $ vote_average popular, show $ vote_count popular) 

store :: ParsedImage -> [Char] -> IO ()
store (Image a) image_collection = do
    case a of
        Left failure -> 
            logDebug $ "Error Occurred in Parsing or Image Not Unavailable [ImageModel]: " ++ failure
        Right success -> do
            logDebug "Parse OK"
            saveImage success image_collection

saveImage :: ImageResponse -> [Char] -> IO ()
saveImage response image_collection = do
    saveEachImagePoster (posters response) (image_context_id response) image_collection
    saveEachImageBackdrop (backdrops response) (image_context_id response) image_collection
    saveEachImageLogo (logos response) (image_context_id response) image_collection

saveEachImagePoster :: [ImageModel] -> Int -> [Char] -> IO ()
saveEachImagePoster [] _ _ = logDebug "Database OK [image_posters]"
saveEachImagePoster (current:remaining) image_context_id image_collection = do
    
    connection <- initialiseDB
    
    insertImagePoster connection current image_context_id image_collection

    saveEachImagePoster remaining image_context_id image_collection

saveEachImageBackdrop :: [ImageModel] -> Int -> [Char] -> IO ()
saveEachImageBackdrop [] _ _ = logDebug "Database OK [image_backdrops]"
saveEachImageBackdrop (current:remaining) image_context_id image_collection = do
    
    connection <- initialiseDB
    
    insertImageBackdrop connection current image_context_id image_collection

    saveEachImageBackdrop remaining image_context_id image_collection

saveEachImageLogo :: [ImageModel] -> Int -> [Char] -> IO ()
saveEachImageLogo [] _ _ = logDebug "Database OK [image_logos]"
saveEachImageLogo (current:remaining) image_context_id image_collection = do
    
    connection <- initialiseDB
    
    insertImageLogo connection current image_context_id image_collection

    saveEachImageLogo remaining image_context_id image_collection

insertImagePoster :: Connection -> ImageModel -> Int -> [Char] -> IO ()
insertImagePoster conn image image_context_id image_collection = do
    execute conn
        "INSERT INTO image_posters (image_context_id, image_collection, aspect_ratio, height, width, file_path, image_vote_average, image_vote_count) VALUES (?, ?, ?, ?, ?, ?, ?, ?)"
        (image_context_id, image_collection, aspect_ratio image, height image, width image, file_path image, image_vote_average image, image_vote_count image)

insertImageBackdrop :: Connection -> ImageModel -> Int -> [Char] -> IO ()
insertImageBackdrop conn image image_context_id image_collection = do
    execute conn
        "INSERT INTO image_backdrops (image_context_id, image_collection, aspect_ratio, height, width, file_path, image_vote_average, image_vote_count) VALUES (?, ?, ?, ?, ?, ?, ?, ?)"
        (image_context_id, image_collection, aspect_ratio image, height image, width image, file_path image, image_vote_average image, image_vote_count image)

insertImageLogo :: Connection -> ImageModel -> Int -> [Char] -> IO ()
insertImageLogo conn image image_context_id image_collection = do
    execute conn
        "INSERT INTO image_logos (image_context_id, image_collection, aspect_ratio, height, width, file_path, image_vote_average, image_vote_count) VALUES (?, ?, ?, ?, ?, ?, ?, ?)"
        (image_context_id, image_collection, aspect_ratio image, height image, width image, file_path image, image_vote_average image, image_vote_count image)
