-- | Fetch Module
module Fetch(
    fetchPages,
    fetchDetails,
    fetchImages
) where

import Network.HTTP.Conduit
    ( Request(requestHeaders),
      Response(responseBody),
      Response(responseStatus),
      newManager,
      tlsManagerSettings,
      parseRequest,
      httpLbs,
      setQueryString )
import Network.HTTP.Types.Status (statusCode)
import Utility.Constants
    ( getEndpoint,
      hAuthorization,
      bearerToken,
      qPage,
      imagePath )
import Utility.Exception
    ( handleFetchResponse )

import qualified Data.ByteString.Lazy.Char8 as L8
import Data.String (fromString)
import Data.ByteString (ByteString)

-- | Fetch Pages Function
fetchPages :: String -> Int -> IO L8.ByteString
fetchPages collection page = do

    -- Step 1: Create Connection Manager
    manager <- newManager tlsManagerSettings

    -- Step 2: Get Endpoint
    request <- parseRequest $ getEndpoint collection

    -- Step 3: Add Authorization
    let authorizedRequest = request {
         requestHeaders = [ (fromString hAuthorization, bearerToken) ]
         }
    
    -- Step 4: Add Query Parameters
    let pageQuery :: [ (ByteString, Maybe ByteString) ]
        pageQuery = [ (fromString qPage, Just (fromString $ show page)) ]
    let authorizedRequestWithPageQuery = setQueryString pageQuery authorizedRequest
    
    -- Step 5: Fetch Data
    response <- httpLbs authorizedRequestWithPageQuery manager

    handleFetchResponse 
        (statusCode $ responseStatus response)
        (getEndpoint collection)

    return $ responseBody response

-- | Fetch Details Function
fetchDetails :: String -> Int -> IO L8.ByteString
fetchDetails collection identifier = do

    -- Step 1: Create Connection Manager
    manager <- newManager tlsManagerSettings

    -- Step 2: Get Endpoint & Append Identifier Path
    request <- parseRequest (getEndpoint collection ++ show identifier)

    -- Step 3: Add Authorization
    let authorizedRequest = request {
         requestHeaders = [ (fromString hAuthorization, bearerToken) ]
         }
    
    -- Step 4: Fetch Data
    response <- httpLbs authorizedRequest manager

    handleFetchResponse 
        (statusCode $ responseStatus response)
        (getEndpoint collection ++ show identifier)

    return $ responseBody response

-- | Fetch Images Function
fetchImages :: String -> Int -> IO L8.ByteString
fetchImages collection identifier = do

    -- Step 1: Create Connection Manager
    manager <- newManager tlsManagerSettings

    -- Step 2: Get Endpoint & Append Identifier Path
    request <- parseRequest (getEndpoint collection ++ show identifier ++ imagePath)

    -- Step 3: Add Authorization
    let authorizedRequest = request {
         requestHeaders = [ (fromString hAuthorization, bearerToken) ]
         }
    
    -- Step 4: Fetch Data
    response <- httpLbs authorizedRequest manager

    handleFetchResponse 
        (statusCode $ responseStatus response)
        (getEndpoint collection ++ show identifier)

    return $ responseBody response