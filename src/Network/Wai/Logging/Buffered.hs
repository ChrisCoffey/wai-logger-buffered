{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Network.Wai.Logging.Buffered where

import qualified Data.ByteString.Lazy as BS
import Data.Time.Clock (getCurrentTime, UTCTime)
import Data.IORef

data Event = Event {
    path:: BS.ByteString,
    reportedTime :: UTCTime
    }

-- | The ordering of events within a buffer is unimportant
newtype Buffer = Buffer [Event]


