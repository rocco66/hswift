module Swift.Common
    ( LazyByteString
    , StrictByteString
    , setPreferableFormat
    , findHeaderValue
    ) where

import Data.Monoid ((<>))
import qualified Data.ByteString as StrictByteString
import qualified Data.ByteString.Lazy as LazyByteString
import qualified Data.List as List

import Data.Attoparsec.ByteString.Lazy (Parser, eitherResult, parse)
import Network.HTTP.Types.Header (ResponseHeaders, Header, HeaderName)
import Network.HTTP.Conduit (Request(queryString))

type LazyByteString = LazyByteString.ByteString
type StrictByteString = StrictByteString.ByteString

setPreferableFormat :: Request m -> Request m
setPreferableFormat req = req { queryString = "format=json" }

findHeaderValue :: ResponseHeaders
                -> HeaderName
                -> Parser a
                -> Either StrictByteString a
findHeaderValue headers name parser = do
    bsValue <- either (Left $ "Can't find header " <> name)
                           (return . snd)
                           (List.find name headers)
    eitherResult $ parse parser bsValue
