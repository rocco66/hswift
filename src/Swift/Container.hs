module Swift.Container
    ( Container(..)
    , ContainerInfo
    , mkContainersInfoFromJson
    -- , getContainer
    ) where


import Data.Monoid ((<>))
import Data.Functor ((<$>))
import Control.Monad (forM)

import Network.HTTP.Types.Header (ResponseHeaders)
import Data.String.Like (bs)
import Data.Conduit (MonadThrow(monadThrow))
import qualified Data.Vector as Vector
import qualified Data.Aeson as Aeson

import Swift.Monad (Swift, SwiftAuthenticator, SwiftException(UnknownSwift))
import Swift.Object (ObjectInfo)
import Swift.Types (StrictByteString, LazyByteString, Metadata)
import Swift.Internal (castJsonObjectToBsMetadata, castHeadersToMetadata)

-- data ContainerInfo = ContainerInfo { containerInfoObjectCount :: Integer
--                                    , containerInfoBytesUsed   :: Integer
--                                    } deriving (Eq, Show)

newtype ContainerInfo = ContainerInfo Metadata
  deriving (Eq, Show)

data Container = Container { containerHeaders :: ContainerInfo
                           , conatinerObjects :: [ObjectInfo] }
  deriving (Eq, Show)

-- TODO: rename Info to Meta
mkContainerInfoFromHeaders :: ResponseHeaders -> ContainerInfo
mkContainerInfoFromHeaders = ContainerInfo . castHeadersToMetadata

mkContainersInfoFromJson :: (SwiftAuthenticator auth info)
                         => Aeson.Array
                         -> Swift auth info [ContainerInfo]
mkContainersInfoFromJson jsonContainers = do
    containersInfo <- forM (Vector.toList jsonContainers) $ \case
        (Aeson.Object obj) -> castJsonObjectToBsMetadata obj
        other -> monadThrow $ UnknownSwift $
            "Container is not a object" <> (bs $ show other)
    return $ ContainerInfo <$> containersInfo
    -- contianerInfoContainerCount <- findHeaderValue headers
    --                                "x-contianer-object-count" $ parse int
    -- contianerInfoBytesUsed <- findHeaderValue headers
    --                           "x-contianer-bytes-used" $ parse int
    -- contianerInfoTimestamp <- findHeaderValue headers
    --                           "x-timestamp" $ parse float
    -- contianerInfoBytesUsed <- findHeaderValue headers
    --     "x-contianer-bytes-used" $ parse int

headContainer =

getContainer =

putContainer =

postContainer =

deleteConatiner =
