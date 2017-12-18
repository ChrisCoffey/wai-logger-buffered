{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad (mapM_)
import Data.Monoid ((<>))
import Data.List (sortBy)
import Data.Foldable (foldl')
import Data.Ord (comparing)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.Map as M
import Network.Wai
import Network.Wai.Logging.Buffered
import Network.Wai.Handler.Warp
import Network.HTTP.Types.Status (status200)
import Control.Concurrent (forkIO)
-- goal is to write a wai example client

defaultConfig :: Config
defaultConfig = Config {
    maxSize = 10000
    , publishIntervalSeconds = 15
    , pubFunc = stdOutLogger
    }

demoApp :: Application
demoApp req respond =
    respond $ responseLBS status200 [] "Logging Demo"

main :: IO ()
main = do
    forkIO $ runBufferedRequestLogger defaultConfig
    run 8080 $ bufferedRequestLogger defaultConfig demoApp

--
-- A simple standardOut logger
--

stdOutLogger :: Publish
stdOutLogger =
    mapM_ printSection . M.toList . grouped

printSection ::
    (BS.ByteString, [Event])
    -> IO ()
printSection (p, ex) = do
    let count = length ex
        avgDur =  sum (duration <$> ex) / fromIntegral count
        srted = sortBy (comparing duration) ex
        median = duration $ nthPercentile 0.5 srted
        n5 = duration $ nthPercentile 0.95 srted
        n9 = duration $ nthPercentile 0.99 srted
    print $ p <> ": \n" <>
        "events : " <> BSC.pack (show count) <> "\n" <>
        "average duration: " <> BSC.pack (show avgDur) <>  "\n" <>
        "median: " <> BSC.pack (show median) <> "\n" <>
        "95th percentile duration: " <> BSC.pack (show n5) <> "\n" <>
        "99th percentile duration: " <> BSC.pack (show n9) <> "\n"

nthPercentile ::
    Double -- | Must be between 0 and 1
    -> [a]
    -> a
nthPercentile ntile xs = let
    c = length xs
    idx = floor (ntile * fromIntegral c)
    xs' = drop (idx - 1 ) xs
    in case xs' of
        [] -> last xs
        (x:rest) -> x

grouped ::
    [Event]
    -> M.Map BS.ByteString [Event]
grouped =
    foldl' addTo M.empty
    where
        addTo acc e = M.insertWith (<>) (path e) [e] acc

