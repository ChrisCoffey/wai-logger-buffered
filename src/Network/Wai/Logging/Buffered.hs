{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Network.Wai.Logging.Buffered (
   bufferedRequestLogger
) where

import Data.Monoid ((<>))
import Control.Exception (bracket, catch)
import Network.Wai (Application, Request, Middleware,
                    rawPathInfo)
import qualified Data.ByteString as BS
import Data.Time.Clock (getCurrentTime, diffUTCTime, UTCTime, NominalDiffTime)
import Data.IORef
import System.IO.Unsafe (unsafePerformIO)

data Config = Config {

}

data Event = Event {
    path:: BS.ByteString,
    reportedTime :: UTCTime,
    duration :: NominalDiffTime
    }
    deriving (Show, Eq, Ord)

-- | The ordering of events within a buffer is unimportant
newtype Buffer = Buffer [Event]
    deriving (Eq, Ord, Monoid)

-- | There is only a single 'buffer' per instance of the milddleware. All calls are logged to the same
-- buffer before publication.
--
-- This can obviously be pulled out and passed via a reader, but I can't think of
-- a good reason to do that yet.
buffer :: IORef Buffer
buffer = unsafePerformIO . newIORef $ Buffer []

-- | adds an event to the buffer if the buffer is not full. If it is full, the event
-- is dumped to stdOut
logEvent ::
    Request
    -> UTCTime
    -> IO ()
logEvent req start = do
    finish <- getCurrentTime
    let path = rawPathInfo req
        event = Event path finish (finish `diffUTCTime` start)
    atomicModifyIORef' buffer $ addToBuffer event
    where
        addToBuffer evt (Buffer ls) = (Buffer (evt:ls), ())



-- | attempt to publish the buffer. on failure, the events remain in the buffer
-- This assumes that there will generally be far more events in the publish buffer than
-- have been added during function invocation
publishBuffer ::
    ([Event] -> IO ())
    -> IO ()
publishBuffer doPublish = do
    events <- atomicModifyIORef' buffer clearBuffer
    catch (doPublish events) $ \e -> do
        atomicModifyIORef' buffer $ mergeBufer events
    where
        clearBuffer (Buffer ls) = (Buffer [], ls)
        mergeBufer events  b = (b <> Buffer events, ())

bufferedRequestLogger :: Middleware
bufferedRequestLogger app req sendResponse =
    app req $ \res ->
        bracket getCurrentTime
                (logEvent req)
                (const $ sendResponse res)


