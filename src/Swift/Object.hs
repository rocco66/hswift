module Swift.Object
    ( Object(..)
    , ObjectMetadata
    , headObject
    , getObject
    , putObject
    , postObject
    , deleteObject
    , mkObjectsMetadataFromJson
    ) where

import Data.Monoid ((<>))
import Data.Functor ((<$>))
import Control.Monad (forM, void)
import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString as StrictByteString

import Network.HTTP.Types.Header (ResponseHeaders, Header)
import Network.HTTP.Types (methodHead, methodPost, methodPut, methodDelete,
                           statusIsSuccessful)
import Data.Conduit (MonadThrow(monadThrow))
import Data.String.Like (bs)
import Network.HTTP.Conduit (Request(method, path, requestBody),
                             RequestBody(..),
                             httpLbs,
                             responseHeaders, responseBody, responseStatus)
import qualified Data.Vector as Vector
import qualified Data.Aeson as Aeson

import Swift.Types (StrictByteString, LazyByteString, Metadata)
import Swift.Monad (Swift, SwiftAuthenticator,
                    SwiftException(UnknownSwift, SomeSwiftException),
                    getManager, prepareRequestAndParseUrl)
import Swift.Helpers (addHeaders)
import Swift.Internal (castJsonObjectToBsMetadata, castHeadersToMetadata,
                       setPreferableFormat, debug)


newtype ObjectMetadata = ObjectMetadata Metadata
  deriving (Eq, Show)

data Object = Object { objectHeaders :: ObjectMetadata
                     , objectData    :: LazyByteString }
  deriving (Eq, Show)

urlJoin :: [StrictByteString] -> StrictByteString
urlJoin = StrictByteString.intercalate "/"

mkObjectMetadataFromHeaders :: ResponseHeaders -> ObjectMetadata
mkObjectMetadataFromHeaders = ObjectMetadata . castHeadersToMetadata

mkObjectsMetadataFromJson :: (SwiftAuthenticator auth info)
                          => Aeson.Array
                          -> Swift auth info [ObjectMetadata]
mkObjectsMetadataFromJson jsonObjects = do
    objectsMetadata <- forM (Vector.toList jsonObjects) $ \case
        (Aeson.Object obj) -> castJsonObjectToBsMetadata obj
        other -> monadThrow $ UnknownSwift $
            "Object is not a object" <> (bs $ show other)
    return $ ObjectMetadata <$> objectsMetadata

getObject :: (SwiftAuthenticator auth info)
             => StrictByteString
             -> StrictByteString
             -> Swift auth info Object
getObject containerName objectName = do
    request <- setPreferableFormat <$> prepareRequestAndParseUrl
    manager <- getManager
    response <- httpLbs request {path = containerName} manager
    case statusIsSuccessful (responseStatus response) of
        True -> let objectHeaders = mkObjectMetadataFromHeaders $
                                              responseHeaders response
                    objectData    = responseBody response in
                return Object { .. }
        False -> monadThrow $
            SomeSwiftException -- "Can't get object " <> fullPath
  where
    fullPath = urlJoin [containerName, objectName]


headObject :: (SwiftAuthenticator auth info)
           => StrictByteString
           -> StrictByteString
           -> Swift auth info ObjectMetadata
headObject containerName objectName = do
    baseRequest <- prepareRequestAndParseUrl
    manager <- getManager
    let fullPath = urlJoin [containerName, objectName]
        request = baseRequest { method = methodHead
                              , path = fullPath }
    response <- httpLbs request manager
    return $ mkObjectMetadataFromHeaders $ responseHeaders response

putObject :: (SwiftAuthenticator auth info)
           => StrictByteString
           -> StrictByteString
           -> [Header]
           -> LazyByteString
           -> Swift auth info ()
putObject containerName objectName headers content = do
    baseRequest <- prepareRequestAndParseUrl
    manager <- getManager
    -- TODO: get metadate not as Headers
    -- rawBody ???
    let request = addHeaders headers baseRequest { method = methodPut
                                                 , path = fullPath
                                                 , requestBody = body }
    void $ httpLbs request manager
  where
    -- TODO(Mitroshin): replace to RequestBodyStreamChunked
    body = RequestBodyLBS content
    fullPath = urlJoin [containerName, objectName]

postObject :: (SwiftAuthenticator auth info)
           => StrictByteString
           -> StrictByteString
           -> [Header]
           -> Swift auth info ()
postObject containerName objectName headers = do
    baseRequest <- prepareRequestAndParseUrl
    manager <- getManager
    -- TODO: get metadate not as Headers
    let request = addHeaders headers baseRequest { method = methodPost
                                                 , path = fullPath }
    void $ httpLbs request manager
  where
    fullPath = urlJoin [containerName, objectName]

deleteObject :: (SwiftAuthenticator auth info)
             => StrictByteString
             -> StrictByteString
             -> Swift auth info ()
deleteObject containerName objectName = do
    baseRequest <- prepareRequestAndParseUrl
    manager <- getManager
    let request = baseRequest { method = methodDelete
                              , path = fullPath }
    void $ httpLbs request manager
  where
    fullPath = urlJoin [containerName, objectName]
