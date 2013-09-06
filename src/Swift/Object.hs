module Swift.Object
    ( Object(..)
    , ObjectMetadata
    -- , getObject
    , mkObjectsMetadataFromJson
    ) where

import Data.Monoid ((<>))
import Data.Functor ((<$>))
import Control.Monad (forM)

import Data.Conduit (MonadThrow(monadThrow))
import Data.String.Like (bs)
import qualified Data.Vector as Vector

import qualified Data.Aeson as Aeson

import Swift.Types (StrictByteString, LazyByteString, Metadata)
import Swift.Monad (Swift, SwiftAuthenticator,
                    SwiftException(UnknownSwift, SomeSwiftException),
                    getManager, prepareRequestAndParseUrl)
import Swift.Internal (castJsonObjectToBsMetadata, castHeadersToMetadata,
                       addUriTokens)


newtype ObjectMetadata = ObjectMetadata Metadata
  deriving (Eq, Show)

data Object = Object { objectHeaders :: ObjectMetadata
                     , objectData    :: StrictByteString }
  deriving (Eq, Show)

mkObjectsMetadataFromJson :: (SwiftAuthenticator auth info)
                          => Aeson.Array
                          -> Swift auth info [ObjectMetadata]
mkObjectsMetadataFromJson jsonObjects = do
    objectsMetadata <- forM (Vector.toList jsonObjects) $ \case
        (Aeson.Object obj) -> castJsonObjectToBsMetadata obj
        other -> monadThrow $ UnknownSwift $
            "Object is not a object" <> (bs $ show other)
    return $ ObjectMetadata <$> objectsMetadata


-- getObject =

-- headObject =

-- putObject =

-- postObject =

-- deleteObject =
