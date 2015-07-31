module Playlistach.Common (ProducerResponse, Track, Origin) where

import Data.ByteString (ByteString)
import Network.HTTP.Client (Response)
import Pipes (Producer)
import Playlistach.Model.Track (Track)
import Playlistach.Model.Origin (Origin)

type ProducerResponse = Response (Producer ByteString IO ())