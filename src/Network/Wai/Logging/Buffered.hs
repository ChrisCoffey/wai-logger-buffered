{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module:      Network.Wai.Logging.Buffered
-- Copyright:   (c) 2017 Chris Coffey
-- License:     MIT
-- Maintainer:  Chris Coffey <chris@foldl.io>
-- Stability:   experimental
-- Portability: portable
--
-- Middleware for buffering log messages instead of immediately writing them to a handle.
--
-- This allows easy integration with tools providing bulk publish apis like Graphite or Elasticsearch.
--
-- @
--    main :: IO ()
--    main = do
--      forkIO $ runBufferedRequestLogger def
--      run 8080 $ bufferedRequestLogger def waiApplication
-- @
--
-- A note about wildcards: the '*' wildcard is used to collapse similar URLs for easier reporting on external platforms.
--
module Network.Wai.Logging.Buffered (
    -- * Types
    Config(..),
    Event(..),
    Publish,
    -- * Functions
    bufferedRequestLogger,
    runBufferedRequestLogger
) where

import Control.Concurrent
import Control.Exception (bracket, catch, Exception, SomeException)
import Control.Monad (forever)
import Data.Default (Default(..))
import Data.Foldable (foldl')
import Data.IORef
import Data.Monoid ((<>))
import Data.Time.Clock (getCurrentTime, diffUTCTime, UTCTime, NominalDiffTime)
import GHC.Exts (toList)
import Network.Wai (Application, Request, Middleware,
                    rawPathInfo, requestMethod)
import Network.Wai.Internal (Response(..))
import System.IO.Unsafe (unsafePerformIO)

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.Sequence as S
import qualified Data.Map as M


-- | A log sink.
type Publish = [Event] -> IO ()

-- | Configuration arguments for draining the buffer
data Config = Config {
    -- | The maximum size allowed for the buffer. After this is hit messages are pushed to stdOut. Defaults to 1000
    maxSize :: Int,
    -- | How frequently to publish events to the sink. Defaults to 10 (seconds)
    publishIntervalSeconds :: Int,
    -- | The sink function. Defaults to `print`
    pubFunc :: Publish,
    -- | Determines whether to publish the original path and a '*' wildcarded version. Defaults to 'True'
    useWildcards :: Bool
}

instance Default Config where
    def = Config {
        maxSize = 1000,
        publishIntervalSeconds = 10,
        pubFunc = print,
        useWildcards = True
        }

-- | Tracks a single 'Request'
data Event = Event {
    -- | The request URL
    path:: !BS.ByteString,
    -- | The time the request occurred
    reportedTime :: !UTCTime,
    -- | The request duration
    duration :: !NominalDiffTime
    }
    deriving (Show, Eq, Ord)

-- | The ordering of events within a buffer is unimportant
newtype Buffer = Buffer (S.Seq Event)
    deriving (Eq, Ord, Monoid)


-- | There is only a single buffer per instance of the milddleware. All calls are logged to the same
-- buffer before publication.
--
-- This can obviously be pulled out and passed via a reader, but I can't think of
-- a good reason to do that yet.
buffer :: IORef Buffer
{-# NOINLINE buffer #-}
buffer = unsafePerformIO . newIORef $ Buffer S.empty

-- | adds an event to the buffer if the buffer is not full. If it is full, the event
-- is dumped to stdOut
logEvent ::
    Config
    -> Request
    -> UTCTime
    -> IO ()
logEvent (Config {..}) req start = do
    finish <- getCurrentTime
    let path = rawPathInfo req
        event = Event (requestMethod req <>":"<>path) finish (finish `diffUTCTime` start)
    -- its possible for other requets to join the buffer in the time it takes
    -- between read & write. Those messages are added to the buffer rather than silently dropped
    (Buffer b) <- readIORef buffer
    if S.length b < maxSize
    then atomicModifyIORef' buffer $ addToBuffer event
    else print $ errorMsg event
    where
        addToBuffer evt (Buffer ls) = (Buffer (evt S.<| ls), ())

-- | Dumps overflow messages to stdOut
--
-- >>>

errorMsg ::
   Event
   -> String
errorMsg Event {..} =
    show reportedTime <> " [Error][Logging] Log Buffer Full. Dropping: \n" <>
    "\tPath: "<>show path<> ", Duration: "<> show duration

-- | attempt to publish the buffer. on failure, the events remain in the buffer
-- This assumes that there will generally be far more events in the publish buffer than
-- have been added during function invocation
publishBuffer ::
    Bool
    -> Publish
    -> IO ()
publishBuffer useWc doPublish = do
    events <- atomicModifyIORef' buffer clearBuffer
    let events' = if useWc
        then concat . M.elems. M.filterWithKey wcPred $ foldl' applyWildCard M.empty events
        else toList events
    catch (doPublish events') (preserveAndLog events')
    where
        wcPred k xs = (length xs > 1 && BSC.any (== '*') k) || BSC.all (/= '*') k
        clearBuffer (Buffer ls) = (Buffer S.empty, ls)
        mergeBufer events  b = (b <> Buffer events, ())
        preserveAndLog :: [Event] -> SomeException -> IO ()
        preserveAndLog events e = do
            atomicModifyIORef' buffer . mergeBufer $ S.fromList events
            print e

-- | Based on the provided 'Config' publishes the logged requests & drains the buffer. The ideal configuration
-- depends on your workload, but know that each request is stored as is. I.e. if you handle 1k req/s, then you should
-- make sure 'maxSize'/'publishIntervalSeconds' > 1000.
runBufferedRequestLogger ::
    Config
    -> IO ()
runBufferedRequestLogger (Config {..}) =
    forever $ do
        threadDelay $ toMicros publishIntervalSeconds
        publishBuffer useWildcards pubFunc
    where
        toMicros = (*) 1000000

-- | Collect timing on all 'Request's and add them to the buffer. Configuration is controlled via the provided
-- 'Config'
bufferedRequestLogger ::
    Config
    -> Middleware
bufferedRequestLogger conf app req sendResponse = do
    t0 <- getCurrentTime
    app req $ \res -> do
        x <- case res of
            ResponseRaw{} -> pure ()
            _ -> pure ()
        logEvent conf req t0
        sendResponse res

applyWildCard ::
    M.Map BS.ByteString [Event]
    -> Event
    -> M.Map BS.ByteString [Event]
applyWildCard known e =
    foldl' accum known $ setPath <$> wildCardPermutations (path e)
    where
        accum m evt = M.insertWith (<>) (path evt) [evt] m
        setPath p = e {path = p}

-- TODO use edit distance on the path segments rather than simple replace logic
wildCardPermutations ::
    BS.ByteString
    -> [BS.ByteString]
wildCardPermutations "" = []
wildCardPermutations path = let
    segments = BSC.split '/' path
    wildcarded = perms segments
    res = BS.intercalate "/" <$> wildcarded
    in res
    where
        replaceAt :: [BS.ByteString] -> Int -> [BS.ByteString]
        replaceAt bs n = case Prelude.splitAt n bs of
            (as, []) -> as
            (as, b:bs) -> as <> ("*":bs)
        perms :: [BS.ByteString] -> [[BS.ByteString]]
        perms xs = replaceAt xs <$> [0.. Prelude.length xs]
