-- | Utility Module.Logger
module Utility.Logger (
    logInfo,
    logWarning,
    logError,
    logDebug
) where

import Data.Time (getCurrentTime)
import qualified Data.ByteString.Lazy.Char8 as L8

-- | Level Data
data Level
    = INFO
    | WARNING
    | ERROR
    | DEBUG
    deriving (Show, Enum)

-- | Logger
logger :: Level -> [Char] -> IO ()
logger level log = do
    time <- getCurrentTime
    let formatted = "[ " ++ show time ++ " ][ " ++ show level ++ " ]: " ++ log
    print formatted
    L8.appendFile "haskell-project.log" (L8.pack $ formatted ++ "\n")

logInfo :: [Char] -> IO ()
logInfo = logger INFO
logWarning :: [Char] -> IO ()
logWarning = logger WARNING
logError :: [Char] -> IO ()
logError = logger ERROR
logDebug :: [Char] -> IO ()
logDebug = logger DEBUG