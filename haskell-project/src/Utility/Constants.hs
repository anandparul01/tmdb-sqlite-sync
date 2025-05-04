-- | Utility.Constants Module

module Utility.Constants(
    discover,
    info,
    getEndpoint,
    hAuthorization,
    bearerToken,
    pages,
    qPage,
    getDetailedCollection,
    getImageCollection,
    imagePath,
    imageRepository
) where

import Data.ByteString.Char8 (pack)
import qualified Data.ByteString as Data.ByteString.Internal.Type
import Data.List (isInfixOf)

-- | Needed Variables
pages :: [Int]
pages = [1..2]

qPage :: String
qPage = "page"

discover :: [String]
discover = ["Discover: Movie", "Discover: TV"]
popular :: [String]
popular = ["Popular: Movie", "Popular: TV", "Popular: People"]

details :: [String]
details = ["Details: Movie", "Details: TV", "Details: People"]

images :: [String]
images = ["Images: Movie", "Images: TV", "Images: People"]

info :: [String]
info = popular

getEndpoint :: String -> String
getEndpoint "Discover: Movie" = "https://api.themoviedb.org/3/discover/movie"
getEndpoint "Discover: TV" = "https://api.themoviedb.org/3/discover/tv"

getEndpoint "Popular: Movie" = "https://api.themoviedb.org/3/movie/popular"
getEndpoint "Popular: TV" = "https://api.themoviedb.org/3/tv/popular"
getEndpoint "Popular: People" = "https://api.themoviedb.org/3/person/popular"

getEndpoint "Details: Movie" = "https://api.themoviedb.org/3/movie/" -- {movie_id}
getEndpoint "Details: TV" = "https://api.themoviedb.org/3/tv/" -- {series_id}
getEndpoint "Details: People" = "https://api.themoviedb.org/3/person/" -- {person_id}

getEndpoint "Images: Movie" = "https://api.themoviedb.org/3/movie/" -- {movie_id}
getEndpoint "Images: TV" = "https://api.themoviedb.org/3/tv/" -- {series_id}
getEndpoint "Images: People" = "https://api.themoviedb.org/3/person/" -- {person_id}

getEndpoint _ = "" -- default is never used in app logic

imagePath :: String
imagePath = "/images"

imageRepository :: String
imageRepository = "https://image.tmdb.org/t/p/w500"

getDetailedCollection :: String -> String
getDetailedCollection collection
    | "Movie" `isInfixOf` collection = "Details: Movie"
    | "TV" `isInfixOf` collection = "Details: TV"
    | "People" `isInfixOf` collection = "Details: People"
    | otherwise = ""

getImageCollection :: String -> String
getImageCollection collection
    | "Movie" `isInfixOf` collection = "Images: Movie"
    | "TV" `isInfixOf` collection = "Images: TV"
    | "People" `isInfixOf` collection = "Images: People"
    | otherwise = ""

hAuthorization :: String
hAuthorization = "Authorization"
bearerToken :: Data.ByteString.Internal.Type.ByteString
bearerToken = Data.ByteString.Char8.pack "Bearer eyJhbGciOiJIUzI1NiJ9.eyJhdWQiOiI5YWQzNzJiMjNkMTE2NzY5YjlkMmU2Y2Y5NmE0MWU4MSIsIm5iZiI6MTczMTU5NTQ0OC4xNzMzOTQ3LCJzdWIiOiI2NzM2MGIyNDI5NTRkMjY0NzYyNThiNzciLCJzY29wZXMiOlsiYXBpX3JlYWQiXSwidmVyc2lvbiI6MX0.bjnSKtNdVDWFtOfdQBgr3z-gdPa3ueMJTotAK4GOzjs"