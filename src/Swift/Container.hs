module Swift.Container
    ( Container(..)
    , ContainerMetadata
    , mkContainersMetadataFromJson
    , getContainer
    ) where


import Data.Monoid ((<>))
import Data.Functor ((<$>))
import Control.Monad (forM)

import Network.HTTP.Types.Header (ResponseHeaders)
import Data.String.Like (bs)
import Data.Conduit (MonadThrow(monadThrow))
import Network.HTTP.Conduit (Request(method, secure, port, path), Response,
                             Manager,
                             newManager, def, httpLbs, responseStatus,
                             managerResponseTimeout, parseUrl, HttpException,
                             responseBody, responseHeaders)
import Data.Attoparsec.ByteString (Parser, eitherResult)
import Data.Attoparsec.ByteString.Lazy (Result(Fail, Done), parse)
import Data.Aeson (Value(Array), json)
import qualified Data.Vector as Vector
import qualified Data.Aeson as Aeson

import Swift.Monad (Swift, SwiftAuthenticator,
                    SwiftException(UnknownSwift, SomeSwiftException),
                    getManager, prepareRequestAndParseUrl)
import Swift.Object (ObjectMetadata, mkObjectsMetadataFromJson)
import Swift.Types (StrictByteString, LazyByteString, Metadata)
import Swift.Internal (castJsonObjectToBsMetadata, castHeadersToMetadata,
                       addUriTokens)
import Swift.Common (setPreferableFormat)

newtype ContainerMetadata = ContainerMetadata Metadata
  deriving (Eq, Show)

data Container = Container { containerMetadata :: ContainerMetadata
                           , conatinerObjects :: [ObjectMetadata] }
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

-- headContainer :: (SwiftAuthenticator auth info)
--               => String
--               -> Swift auth info ContainerMetadata
-- headContainer containerName = do
--     defRequest <- setPreferableFormat <$> prepareRequestAndParseUrl
--     manager <- getManager
--     request <- addUriTokens [containerName] defRequest {method = "HEAD"}
--     response <- httpLbs request manager
--     return $ mkContainerMetadataFromHeaders $ responseHeaders response

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


-- getContainer =

-- putContainer =

-- postContainer =

-- deleteConatiner =
