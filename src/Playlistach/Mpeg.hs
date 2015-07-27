{-# LANGUAGE BinaryLiterals #-}

module Playlistach.Mpeg (Header(..), findHeader) where

import Data.Bits
import Data.Array.Unboxed     as A
import Data.ByteString        as BS
import Data.ByteString.Unsafe as BS
import Pipes

data Header = Header
  { version   :: Int
  , layer     :: Int
  , bitrate   :: Maybe Int
  , frequency :: Int }
  deriving (Eq, Show, Read)

findHeader :: Monad m => Producer ByteString m r -> m (Header, Producer ByteString m r)
findHeader p = next p >>= either (ppterm "findHeader") proceed
  where
    proceed (bs, p')
        | isID3TagPresent bs = do
            (bs', offset, p'') <- skipBytes (getID3TagSize bs + 10) (yield bs >> p')
            return (headerFromBS bs' offset, p'')
        | otherwise =
            return (headerFromBS bs 0, yield bs >> p')

headerFromBS :: ByteString -> Int -> Header
headerFromBS bs offset = Header version layer bitrate frequency
  where
    bitrate
        | isVBR     = Nothing
        | otherwise = Just $ bitrates A.! ((bitrateIndex - 1) * 5 + versionLayerOffset)
      where
        bitrateIndex = shiftR (b2 .&. 0b11110000) 4
        versionLayerOffset = 3 * (version - 1) + layerOffset - 1
        layerOffset
            | version == 2 = min 2 layer
            | otherwise    = layer

    frequency = frequencies A.! (shiftR (b2 .&. 0b00001100) 2 * 2 + version - 1)

    version
        | shiftR (b1 .&. 0b00011000) 3 == 3 = 1
        | otherwise                         = 2

    layer = 4 - shiftR (b1 .&. 0b00000110) 1

    -- TODO: VBRI, MLLP
    isVBR =
        ix 36 == 0x58 && -- X
        ix 37 == 0x69 && -- i
        ix 38 == 0x6E && -- n
        ix 39 == 0x67    -- g

    b1 = ix 1
    b2 = ix 2

    ix :: Int -> Int
    ix n = fromIntegral $ BS.unsafeIndex bs (offset + n)

getID3TagSize :: ByteString -> Int
getID3TagSize bs =
    shiftL (ix 6) 21 .|.
    shiftL (ix 7) 14 .|.
    shiftL (ix 8)  7 .|.
           (ix 9)
  where
    ix n = fromIntegral $ BS.unsafeIndex bs n

isID3TagPresent :: ByteString -> Bool
isID3TagPresent bs =
    ix 0 == 0x49 && -- I
    ix 1 == 0x44 && -- D
    ix 2 == 0x33    -- 3
  where
    ix :: Int -> Int
    ix n = fromIntegral $ BS.unsafeIndex bs n

skipBytes :: Monad m => Int -> Producer ByteString m r -> m (ByteString, Int, Producer ByteString m r)
skipBytes n p = go (return ()) p n
  where
    go l r n = next r >>= either (ppterm "skipBytes") proceed
      where
        proceed (bs, r')
            | len <= n  = go (l >> yield bs) r' (n - len)
            | otherwise = return (bs, n, l >> yield bs >> r')
          where
            len = BS.length bs

ppterm :: String -> e
ppterm fname = error (fname ++ ": premature producer termination!")

bitrates :: UArray Int Int
bitrates =
    A.listArray (0, 69) $
        [ 32,  32,  32,  32,  8
        , 64,  48,  40,  48,  16
        , 96,  56,  48,  56,  24
        , 128, 64,  56,  64,  32
        , 160, 80,  64,  80,  40
        , 192, 96,  80,  96,  48
        , 224, 112, 96,  112, 56
        , 256, 128, 112, 128, 64
        , 288, 160, 128, 144, 80
        , 320, 192, 160, 160, 96
        , 352, 224, 192, 176, 112
        , 384, 256, 224, 192, 128
        , 416, 320, 256, 224, 144
        , 448, 384, 320, 256, 160 ]

frequencies :: UArray Int Int
frequencies =
    A.listArray (0, 5) $
        [ 44100, 22050
        , 48000, 24000
        , 32000, 16000 ]
