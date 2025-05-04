{-# LANGUAGE OverloadedStrings #-}

-- | UI.Explorer Module
module UI.Explorer (
    generatePopular,
    generatePages
) where

import Utility.Constants (imageRepository)
import Utility.Logger (logDebug)
import Text.Blaze.Html5 as BlazeH5
import Text.Blaze.Html5.Attributes
import qualified Data.ByteString.Lazy.Char8 as L8
import Types (PopularFetchModel(..))
import qualified Text.Blaze.Html4.FrameSet as A
import Text.Blaze.Html.Renderer.Utf8 (renderHtml)

-- | Variables
columnSize :: Int
columnSize = 3

columnCount :: Int
columnCount = 3

setSize :: Int
setSize = columnSize * columnCount

extract :: [PopularFetchModel] -> Int -> Int -> [PopularFetchModel]
extract paths t d = take t (drop (t * d) paths)

absPath :: [Char] -> [Char]
absPath path = imageRepository ++ path

relPath :: Maybe String -> [Char]
relPath (Just path) = path
relPath Nothing = ""

refPath :: Maybe String -> String
refPath (Just path) = takeWhile (/= '.') path
refPath Nothing = "/haskell-project-page"

hrefPath :: PopularFetchModel -> [Char]
hrefPath model = "ui-resources" ++ refPath (popular_poster_path model) ++ ".html"

justText :: Maybe String -> [Char]
justText (Just name) = name
justText Nothing = ""

image :: String -> PopularFetchModel -> Html
image path poster = BlazeH5.a ! href (BlazeH5.toValue (hrefPath poster)) $ img ! class_ "img" ! src (BlazeH5.toValue path)

column :: [PopularFetchModel] -> Html
column paths = do -- 3 Elements
    BlazeH5.div ! class_ "img-base img-column" $ do
        image (absPath $ relPath $ popular_poster_path $ paths !! 0) (paths !! 0)
        image (absPath $ relPath $ popular_poster_path $ paths !! 1) (paths !! 1)
        image (absPath $ relPath $ popular_poster_path $ paths !! 2) (paths !! 2)

set :: [PopularFetchModel] -> Html
set paths = do -- 9 Elements
    BlazeH5.div ! class_ "img-base img-set" $ do
        column $ extract paths columnSize 0
        column $ extract paths columnSize 1
        column $ extract paths columnSize 2

initialize :: [PopularFetchModel] -> Html
initialize paths = docTypeHtml $ do
    BlazeH5.head $ do
        BlazeH5.title "TMDB | Popular Movies & TV Shows"
        link ! rel "stylesheet" ! href "ui-resources/haskell-project.css"
    body $ do
        A.br
        A.center $ BlazeH5.h1 "Popular Movies & TV Shows"
        A.br

        BlazeH5.div ! class_ "popular" $ do
            set $ extract paths setSize 0
            set $ extract paths setSize 1
            set $ extract paths setSize 2
            set $ extract paths setSize 3

page :: PopularFetchModel -> Html
page model = docTypeHtml $ do
    BlazeH5.head $ do
        BlazeH5.title "TMDB | Info"
        link ! rel "stylesheet" ! href "ui-resources/haskell-project.css"
    body $ do
        A.br
        A.center $ image (absPath $ relPath $ popular_poster_path model) (model)
        A.br
        A.br
        A.center $ BlazeH5.h1 (BlazeH5.toMarkup (justText (popular_original_title model)))
        A.br
        A.center $ BlazeH5.h3 (BlazeH5.toMarkup (justText (popular_overview model)))


-- | Generate function
generatePages :: [PopularFetchModel] -> IO ()
generatePages [] = logDebug "Generated HTML5 Pages!"
generatePages (current:remaining) = do
    generatePage (current) (renderHtml (page (current)))
    generatePages remaining

generatePage :: PopularFetchModel -> L8.ByteString -> IO ()
generatePage model = L8.writeFile (hrefPath model)

generatePopular :: [PopularFetchModel] -> IO ()
generatePopular records = do
    generate $ renderHtml $ initialize records

generate :: L8.ByteString -> IO ()
generate = L8.writeFile "haskell-project.html"