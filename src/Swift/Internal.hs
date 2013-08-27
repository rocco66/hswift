module Swift.Internal
    ( castJsonObjectToBsMetadata
    , castHeadersToMetadata
    , debug
    ) where

import Data.Monoid ((<>))
import Data.Functor ((<$>))
import Control.Monad.IO.Class (liftIO)

import Data.String.Like (bs)
import Data.ByteString.Lazy (toStrict)
import Data.Conduit (MonadThrow(monadThrow))
import Data.Attoparsec.ByteString (IResult(Fail, Done), parse)
import Network.HTTP.Types.Header (ResponseHeaders)
import Data.CaseInsensitive (original)
import qualified Data.Aeson as Aeson
import qualified Data.HashMap.Strict as HashMap

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
