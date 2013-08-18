module Swift.Helpers
    ( findHeader
    , findHeaderInResponse
    , addHeader
    , addHeaders
    ) where

import Data.Functor ((<$>))
import Data.Foldable as Foldable
import qualified Data.ByteString as StrictByteString

import Network.HTTP.Types (HeaderName, ResponseHeaders, Header)
import Network.HTTP.Conduit (Request, Response, responseHeaders, requestHeaders)

import Swift.Types (StrictByteString)

findHeader :: HeaderName -> ResponseHeaders -> Maybe StrictByteString
findHeader name headers = snd <$> Foldable.find ((name ==) . fst) headers

findHeaderInResponse :: Response body -> HeaderName -> Maybe StrictByteString
findHeaderInResponse resp name = findHeader name headers
  where
    headers = responseHeaders resp

addHeader :: Request m -> Header -> Request m
addHeader r h = addHeaders r [h]

addHeaders :: Request m -> [Header] -> Request m
addHeaders req headers = req { requestHeaders = updatedHeaders }
  where
    updatedHeaders = requestHeaders req ++ headers
