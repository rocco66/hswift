module Swift.Internal
    ( castJsonObjectToBsMetadata
    , castHeadersToMetadata
    , setPreferableFormat
    , findHeaderValue
    , debug
    ) where

import Data.Monoid ((<>))
import Data.Functor ((<$>))
import Control.Monad.IO.Class (liftIO)
import qualified Data.Foldable as Foldable

import Data.String.Like (bs)
import Data.ByteString.Lazy (toStrict)
import Data.Conduit (MonadThrow(monadThrow))
import Data.Attoparsec.ByteString (IResult(Fail, Done), Parser,
                                   eitherResult, parse)
import Network.HTTP.Types.Header (ResponseHeaders, HeaderName)
import Network.HTTP.Conduit (Request(queryString))
import Network.URI (uriPath)
import Data.CaseInsensitive (original)
import qualified Data.Aeson as Aeson
import qualified Data.HashMap.Strict as HashMap
import qualified Data.List as List

import Swift.Types (StrictByteString, Metadata)
import Swift.Monad (Swift, SwiftAuthenticator,
                    SwiftException(NotJsonValueInHeader))

castJsonObjectToBsMetadata :: (SwiftAuthenticator auth info)
                           => Aeson.Object
                           -> Swift auth info Metadata
castJsonObjectToBsMetadata = return . HashMap.fromList .
                             map (\(n, v) -> (bs n, v)) . HashMap.toList

debug :: (SwiftAuthenticator auth info, Show a) => a -> Swift auth info ()
debug = liftIO . putStrLn . show

castHeadersToMetadata :: ResponseHeaders -> Metadata
castHeadersToMetadata = HashMap.fromList . map castPair
  where
    castPair (name, value) = case parse Aeson.json value of
      Done _ jsonValue -> (original name, jsonValue)
      Fail _ _ err -> (original name, Aeson.toJSON value)

-- TODO: cool query string builder
setPreferableFormat :: Request -> Request
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
