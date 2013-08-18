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
import Swift.Types (StrictByteString, LazyByteString)
import Swift.Common (castHeadersToBsPair)
import Swift.Internal (castJsonObjectToBsPair)

-- data ContainerInfo = ContainerInfo { containerInfoObjectCount :: Integer
--                                    , containerInfoBytesUsed   :: Integer
--                                    } deriving (Eq, Show)

newtype ContainerInfo = ContainerInfo [(StrictByteString, StrictByteString)]
  deriving (Eq, Show)

data Container = Container { containerHeaders :: ContainerInfo
                           , conatinerObjects :: [ObjectInfo] }
  deriving (Eq, Show)

-- TODO: rename Info to Meta
mkContainerInfoFromHeaders :: ResponseHeaders -> ContainerInfo
mkContainerInfoFromHeaders = ContainerInfo . castHeadersToBsPair

mkContainersInfoFromJson :: (SwiftAuthenticator auth info)
                         => Aeson.Array
                         -> Swift auth info [ContainerInfo]
mkContainersInfoFromJson jsonContainer = do
    containerInfo <- forM (Vector.toList jsonContainer) $ \case
        (Aeson.Object obj) -> castJsonObjectToBsPair obj
        other -> monadThrow $ UnknownSwift $
            "Container is not a object" <> (bs $ show other)
    return $ ContainerInfo <$> containerInfo
    -- contianerInfoContainerCount <- findHeaderValue headers
    --                                "x-contianer-object-count" $ parse int
    -- contianerInfoBytesUsed <- findHeaderValue headers
    --                           "x-contianer-bytes-used" $ parse int
    -- contianerInfoTimestamp <- findHeaderValue headers
    --                           "x-timestamp" $ parse float
    -- contianerInfoBytesUsed <- findHeaderValue headers
    --     "x-contianer-bytes-used" $ parse int


-- getContainer =
