{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}

module Network.Wai.Logging.Buffered (
    Config(..),
    Event(..),
    Publish,
    bufferedRequestLogger,
    runBufferedRequestLogger
) where

import Control.Concurrent
import Control.Monad (forever)
import Data.Monoid ((<>))
import Control.Exception (bracket, catch, Exception, SomeException)
import Network.Wai (Application, Request, Middleware,
                    rawPathInfo)
import qualified Data.ByteString as BS
import Data.Time.Clock (getCurrentTime, diffUTCTime, UTCTime, NominalDiffTime)
import Data.IORef
import System.IO.Unsafe (unsafePerformIO)
import Data.Sequence as S
import GHC.Exts (toList)

data Config = Config {
    maxSize :: Int,
    publishIntervalSeconds :: Int,
    pubFunc :: Publish
}

data Event = Event {
    path:: !BS.ByteString,
    reportedTime :: !UTCTime,
    duration :: !NominalDiffTime
    }
    deriving (Show, Eq, Ord)

-- | The ordering of events within a buffer is unimportant
newtype Buffer = Buffer (S.Seq Event)
    deriving (Eq, Ord, Monoid)

type Publish = [Event] -> IO ()

bufferLen (Buffer ls) = S.length ls

-- | There is only a single 'buffer' per instance of the milddleware. All calls are logged to the same
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
        event = Event path finish (finish `diffUTCTime` start)
    -- its possible for other requets to join the buffer in the time it takes
    -- between read & write. Those messages are added to the buffer rather than silently dropped
    (Buffer b) <- readIORef buffer
    if S.length b < maxSize
    then atomicModifyIORef' buffer $ addToBuffer event
    else print $ errorMsg event
    where
        addToBuffer evt (Buffer ls) = (Buffer (evt S.<| ls), ())

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
    Publish
    -> IO ()
publishBuffer doPublish = do
    events <- atomicModifyIORef' buffer clearBuffer
    catch (doPublish $ toList events) (preserveAndLog $ toList events)
    where
        clearBuffer (Buffer ls) = (Buffer S.empty, ls)
        mergeBufer events  b = (b <> Buffer events, ())
        preserveAndLog :: [Event] -> SomeException -> IO ()
        preserveAndLog events e = do
            atomicModifyIORef' buffer . mergeBufer $ S.fromList events
            print e

runBufferedRequestLogger ::
    Config
    -> IO ()
runBufferedRequestLogger (Config {..}) =
    forever $ do
        threadDelay $ toMicros publishIntervalSeconds
        publishBuffer pubFunc
    where
        toMicros = (*) 1000000

bufferedRequestLogger ::
    Config
    -> Middleware
bufferedRequestLogger conf app req sendResponse =
    app req $ \res ->
        bracket getCurrentTime
                (logEvent conf req)
                (const $ sendResponse res)
