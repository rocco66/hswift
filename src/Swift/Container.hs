module Swift.Container
    ( Container(..)
    , ContainerMetadata
    , mkContainersMetadataFromJson
    , headContainer
    , getContainer
    , putContainer
    , postContainer
    , deleteContainer
    ) where


import Data.Monoid ((<>))
import Data.Functor ((<$>))
import Control.Monad (forM)

-- TODO: extract all common imports in prelude

import Network.HTTP.Types.Header (ResponseHeaders)
import Data.String.Like (bs)
import Data.Conduit (MonadThrow(monadThrow))
import Network.HTTP.Conduit (Request(method, path), Response,
                             Manager,
                             newManager, def, httpLbs, responseStatus,
                             managerResponseTimeout, parseUrl, HttpException,
                             responseBody, responseHeaders)
import Data.Attoparsec.ByteString (Parser, eitherResult)
import Data.Attoparsec.ByteString.Lazy (Result(Fail, Done), parse)
import Data.Aeson (Value(Array), json)
import Network.HTTP.Types.Header (ResponseHeaders, Header)
import Network.HTTP.Types (methodHead, methodPost, methodPut, methodDelete)
import Control.Monad (void)
import qualified Data.Vector as Vector
import qualified Data.Aeson as Aeson

import Swift.Monad (Swift, SwiftAuthenticator,
                    SwiftException(UnknownSwift, SomeSwiftException),
                    getManager, prepareRequestAndParseUrl)
import Swift.Object (ObjectMetadata, mkObjectsMetadataFromJson)
import Swift.Types (StrictByteString, LazyByteString, Metadata)
import Swift.Internal (castJsonObjectToBsMetadata, castHeadersToMetadata,
                       addUriTokens, setPreferableFormat)
import Swift.Helpers (addHeaders)


newtype ContainerMetadata = ContainerMetadata Metadata
  deriving (Eq, Show)

data Container = Container { containerMetadata :: ContainerMetadata
                           , containerObjects :: [ObjectMetadata] }
  deriving (Eq, Show)

mkContainerMetadataFromHeaders :: ResponseHeaders -> ContainerMetadata
mkContainerMetadataFromHeaders = ContainerMetadata . castHeadersToMetadata

mkContainersMetadataFromJson :: (SwiftAuthenticator auth info)
                             => Aeson.Array
                             -> Swift auth info [ContainerMetadata]
mkContainersMetadataFromJson jsonContainers = do
    containersMetadata <- forM (Vector.toList jsonContainers) $ \case
        (Aeson.Object obj) -> castJsonObjectToBsMetadata obj
        other -> monadThrow $ UnknownSwift $
            "Container is not a object" <> (bs $ show other)
    return $ ContainerMetadata <$> containersMetadata

headContainer :: (SwiftAuthenticator auth info)
              => StrictByteString
              -> Swift auth info ContainerMetadata
headContainer containerName = do
    baseRequest <- setPreferableFormat <$> prepareRequestAndParseUrl
    manager <- getManager
    let request = baseRequest { method = methodHead
                              , path = containerName }
    response <- httpLbs request manager
    return $ mkContainerMetadataFromHeaders $ responseHeaders response

-- TODO extract common code in some function in Internal module

getContainer :: (SwiftAuthenticator auth info)
             => StrictByteString
             -> Swift auth info Container
getContainer containerName = do
    -- ??? join setPreferableFormat and prepareRequestAndParseUrl ???
    request <- setPreferableFormat <$> prepareRequestAndParseUrl
    manager <- getManager
    response <- httpLbs request {path = containerName} manager
    jsonObjects <- case parse json $ responseBody response of
        Fail _ _ _ -> monadThrow SomeSwiftException
        -- TODO: correct exception
        Done _ (Array r) -> return r
        Done _ d -> monadThrow $
            UnknownSwift $ "Objects list is not array: " <> (bs $ show d)
    let containerMetadata = mkContainerMetadataFromHeaders $
            responseHeaders response
    containerObjects <- mkObjectsMetadataFromJson jsonObjects
    return Container { .. }

putContainer :: (SwiftAuthenticator auth info)
             => StrictByteString
             -> Swift auth info ()
putContainer containerName = do
    baseRequest <- setPreferableFormat <$> prepareRequestAndParseUrl
    manager <- getManager
    let request = baseRequest { method = methodPut
                              , path = containerName }
    void $ httpLbs request manager

postContainer :: (SwiftAuthenticator auth info)
              => StrictByteString
              => [Header]
              -> Swift auth info ()
postContainer containerName headers = do
    baseRequest <- setPreferableFormat <$> prepareRequestAndParseUrl
    manager <- getManager
    -- TODO: get metadate not as Headers
    let request = addHeaders headers baseRequest { method = methodPost
                                                 , path = containerName }
    void $ httpLbs request manager

deleteContainer :: (SwiftAuthenticator auth info)
             => StrictByteString
             -> Swift auth info ()
deleteContainer containerName = do
    baseRequest <- setPreferableFormat <$> prepareRequestAndParseUrl
    manager <- getManager
    let request = baseRequest { method = methodDelete
                              , path = containerName }
    void $ httpLbs request manager
