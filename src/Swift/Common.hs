module Swift.Common
    ( setPreferableFormat
    , findHeaderValue
    ) where

import Data.Monoid ((<>))
import qualified Data.Foldable as Foldable

import Data.CaseInsensitive (original)
import Data.Attoparsec.ByteString (Parser, eitherResult, parse)
import Network.HTTP.Types.Header (ResponseHeaders, HeaderName)
import Network.HTTP.Conduit (Request(queryString))
import Data.String.Like (bs)
import qualified Data.HashMap.Strict as HashMap

import Swift.Types (StrictByteString, Metadata)

-- TODO: cool query string builder
setPreferableFormat :: Request m -> Request m
setPreferableFormat req = req { queryString = "format=json" }

findHeaderValue :: ResponseHeaders
                -> HeaderName
                -> Parser a
                -> Either StrictByteString a
findHeaderValue headers name parser = do
    bsValue <- maybe (Left $ "Can't find header " <> (original $ name))
                     (return . snd)
                     (Foldable.find (\(n, _) -> n == name) headers)
    case eitherResult $ parse parser bsValue of
        Left s -> Left $ bs s
        Right r -> return r
