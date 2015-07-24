module Playlistach.Mpeg where

import Data.Bits
import Data.ByteString        as BS
import Data.ByteString.Unsafe as BS
import Pipes

data Version = Mpeg10 | Mpeg20 | Mpeg25
  deriving (Eq, Show, Read)

data Layer = LayerI | LayerII | LayerIII
  deriving (Eq, Show, Read)

data ChannelMode = Stereo | JointStereo | DualMono | Mono
  deriving (Eq, Show, Read)

data Header = Header
  { version     :: Version
  , layer       :: Layer
  , bitrate     :: Int
  , frequency   :: Int
  , channelMode :: ChannelMode }
  deriving (Eq, Show, Read)

headerFromBS :: ByteString -> Header
headerFromBS bs = undefined
  where
    version = case (b1 .&. 0x18) of
        0x3 -> Mpeg10
        0x2 -> Mpeg20
        _   -> Mpeg25

    layer = case (b1 .&. 0x6) of
        0x3 -> LayerI
        0x2 -> LayerII
        _   -> LayerIII

    b0 = BS.unsafeIndex bs 0
    b1 = BS.unsafeIndex bs 1
    b2 = BS.unsafeIndex bs 2
    b3 = BS.unsafeIndex bs 3

headerFromPipe :: Monad m => Producer ByteString m r -> m (Header, Producer ByteString m r)
headerFromPipe p = next p >>= either fail proceed
  where
    fail _ = error "Premature producer termination!"
    proceed (bs, p') = return (headerFromBS bs, yield bs >> p')
