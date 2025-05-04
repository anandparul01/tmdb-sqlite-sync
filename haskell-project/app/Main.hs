{-# OPTIONS_GHC -Wno-unused-do-bind #-}
module Main (main) where

import System.Environment (getArgs)
import Fetch ( fetchPages,
        fetchDetails,
        fetchImages )
import Utility.Constants ( info,
        pages,
        getDetailedCollection,
        getImageCollection )
import Database (
    initialiseDB,
    createTables,
    save,
    store,
    persist,
    getIdentifiersFromDB,
    getPopulars,
    getMovies,
    validatePopularQuery,
    getPopularByKeyword )
import Parse (parseResponse, parsePopular, parseImage, writeDataToFile)
import Utility.Logger ( logInfo, logDebug )
import Utility.Exception ( throwError,
        AppExceptionType(..),
        handleAppError,
        handleAppWarning )
import Control.Exception
import Data.ByteString.Lazy.Internal as LBS
import Database.SQLite.Simple
import UI.Explorer (generatePopular, generatePages)
import Text.Blaze.Html.Renderer.Utf8 (renderHtml)
import Types (PopularModel(poster_path), ImageResponse (posters), PopularFetchModel(..))
import Data.List (isInfixOf)

main :: IO ()
main = do
    args <- getArgs
    case args of
        ["create"] -> do
            catch (create) handleAppError
        ["loaddata"] -> do
            logInfo "Fetching Data & Saving in Database"
            catch (load info) handleAppError
        ["dumpdata"] -> do
            logInfo "Generating data.json"
            catch (dump) handleAppError
        ["ui"] -> do
            ui
            logInfo "Created HTML5 Page: haskell-project.html"
        ["get-popular", collection] -> do -- Get Popular Movies, TV or People
            let validation = validatePopularQuery collection
            if validation == "Invalid"
                then do
                    throwError QueryParameter "Invalid Argument | usage: stack run -- popular <args: movie|tv|people>"
                else do
                    records <- getIdentifiersFromDB validation
                    mapM_ print records
        ["search-movie", keyword] -> do -- Search Popular Movies with Keyword
            records <- getPopularByKeyword keyword
            mapM_ print records
        _ -> cliError

cliError :: IO ()
cliError = throwError CommandLine "Invalid Command | usage: stack run -- <args: create|loaddata|dumpdata>"

create :: IO ()
create = do
    -- Step 1: Initialize Database
    conn <- initialiseDB
    logInfo "Initialized Database"

    -- Step 2: Create Tables

    createTables conn
    logInfo "Created Tables"


load :: [[Char]] -> IO ()
load [] = logInfo "Fetch Complete!"
load (current:remaining) = do
    -- Step 1: Fetch Pages for 'current'
    getPages pages current

    -- Step 2: Fetch Identifiers for 'current' from Database
    identifiers <- getIdentifiersFromDB current
    -- getIdentifier identifiers $ getIdentifiersFromDB current -- add function in db

    -- Step 3: Fetch Details for 'current'
    getDetails identifiers $ getDetailedCollection current

    -- Step 4: Fetch Images for 'current'
    getImages identifiers $ getImageCollection current

    -- Step 5: Repeat same for remaining elements
    load remaining



getPages :: [Int] -> [Char] -> IO ()
getPages [] _ = logInfo "Fetch Pages Complete!"
getPages (page:remaining) collection = do

    logDebug $ "Collection: [" ++ collection ++ "] & Page: [" ++ show page ++ "]"

    -- Step 1: Fetch Page JSON
    json <- try (fetchPages collection page) :: IO (Either SomeException LBS.ByteString)
    case json of
        Left _ ->
            handleAppWarning
        Right success -> do
            -- Step 2: Parse Page JSON & Save Data
            persist (parsePopular success) collection

    -- Step 4: Repeat same for remaining pages
    getPages remaining collection

getDetails :: [PopularFetchModel] -> [Char] -> IO ()
getDetails [] _ = logInfo "Fetch Details Complete!"
getDetails (identifier:remaining) collection = do

    logDebug $ "Collection: [" ++ collection ++ "] & Identifier: [" ++ show (popular_context_id identifier) ++ "]"

    -- Step 1: Fetch Detail JSON
    json <- try (fetchDetails collection (popular_context_id identifier)) :: IO (Either SomeException LBS.ByteString)
    case json of
        Left _ ->
            handleAppWarning
        Right success ->
            -- Step 2: Parse Detail JSON & Save Data
            save $ parseResponse collection success

    -- Step 4: Repeat same for remaining identifiers
    getDetails remaining collection

getImages :: [PopularFetchModel] -> [Char] -> IO ()
getImages [] _ = logInfo "Fetch Images Complete!"
getImages (identifier:remaining) collection = do

    if ("People" `isInfixOf` collection)
        then do
            logDebug "People Images Not Required"
        else do
            logDebug $ "Collection: [" ++ collection ++ "] & Identifier: [" ++ show (popular_context_id identifier) ++ "]"

            -- Step 1: Fetch Image JSON
            json <- try (fetchImages collection (popular_context_id identifier)) :: IO (Either SomeException LBS.ByteString)
            case json of
                Left _ ->
                    handleAppWarning
                Right success ->
                    -- Step 2: Parse Image JSON & Save Data
                    store (parseImage success) collection

            -- Step 4: Repeat same for remaining identifiers
            getImages remaining collection

dump :: IO ()
dump = do
    writeDataToFile

ui :: IO ()
ui = do
    conn <- initialiseDB
    records <- getPopulars conn
    generatePopular records
    generatePages records
