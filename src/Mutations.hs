{-# LANGUAGE DeriveGeneric #-}
module Mutations
  ( Mutation(..)
  , TimeMutation
  , stampMutation
  )
where

import           Control.Monad
import qualified Data.ByteString.Char8         as C
import qualified Data.ByteString.Lazy          as L
import           Data.Binary
import           Data.Word
import           GHC.Generics                   ( Generic )
import           Data.Time.Clock.POSIX          ( getCurrentTime
                                                , utcTimeToPOSIXSeconds
                                                )
import           Data.Int
import           Data.Time                      ( nominalDiffTimeToSeconds
                                                , UTCTime
                                                )


data Mutation =
  Input C.ByteString
  | Output C.ByteString
  | WindowSize Int Int
  deriving Generic
instance Binary Mutation

data TimeMutation = TimeMutation Int64 Mutation
  deriving Generic
instance Binary TimeMutation

stampMutation :: Mutation -> IO TimeMutation
stampMutation mutation = do
  stamp <- getCurrentTime
  return (TimeMutation (millisSinceEpoch stamp) mutation)

intToWord32 :: Int -> Word32
intToWord32 x = fromInteger $ toInteger x

millisSinceEpoch :: UTCTime -> Int64
millisSinceEpoch =
  floor . (1e3 *) . nominalDiffTimeToSeconds . utcTimeToPOSIXSeconds
