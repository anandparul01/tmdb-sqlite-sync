-- | Utility.Exception Module

module Utility.Exception (
    AppException,
    AppExceptionType(..),
    FetchExceptionType(..),
    throwError,
    handleAppError,
    handleAppWarning,
    handleFetchResponse
) where

import Control.Exception
import Utility.Logger ( logError, logWarning, logDebug )

-- | AppExceptionType Data
data AppExceptionType
    = Fetch
    | Parse
    | Database
    | IO
    | Validation
    | Unknown
    | CommandLine
    | QueryParameter
    deriving (Show, Enum)

-- | FetchExceptionType Data
data FetchExceptionType
    = NotFound
    | Forbidden
    | InternalServerError
    | BadGateway
    | Unauthorized
    | Unavailable
    deriving (Show, Enum)

getFetchError :: FetchExceptionType -> String
getFetchError Unauthorized = "Unauthorized [401]: "
getFetchError Forbidden = "Forbidden [403]: "
getFetchError NotFound = "Not Found [404]: "
getFetchError InternalServerError = "Internal Server Error [500]: "
getFetchError BadGateway = "Bad Gateway [502]: "
getFetchError Unavailable = "Server Unavailable [503]: "

data AppException = AppException [Char] deriving (Show)

instance Exception AppException

throwError :: AppExceptionType -> [Char] -> IO ()
throwError exType exMessage = do
    let message = show exType ++ "Exception | " ++ exMessage
    logError message
    throw $ AppException message

handleAppError :: AppException -> IO ()
handleAppError (AppException a) = logError "A Fatal Error Occurred!"

handleAppWarning :: IO ()
handleAppWarning = logWarning "A Non-Essential Transaction Skipped!"

handleFetchResponse :: Int -> [Char] -> IO ()
handleFetchResponse code endpoint = case code of
        200 ->
            logDebug $ "Fetch OK [200]: " ++ endpoint
        401 ->
            throwError Fetch $ getFetchError Unauthorized ++ endpoint
        403 ->
            throwError Fetch $ getFetchError Forbidden ++ endpoint
        404 ->
            throwError Fetch $ getFetchError NotFound ++ endpoint
        500 ->
            throwError Fetch $ getFetchError InternalServerError ++ endpoint
        502 ->
            throwError Fetch $ getFetchError BadGateway ++ endpoint
        503 ->
            throwError Fetch $ getFetchError Unavailable ++ endpoint
        _ ->
            throwError Unknown $ "Unexpected Error [" ++ show code ++ "]: " ++ endpoint