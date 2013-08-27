module Swift.Object
    ( Object(..)
    , ObjectInfo
    -- , getObject
    ) where

import qualified Data.Aeson as Aeson

import Swift.Types (StrictByteString, LazyByteString)

newtype ObjectInfo = ObjectInfo [(StrictByteString, StrictByteString)]
  deriving (Eq, Show)

data Object = Object { objectHeaders :: ObjectInfo
                     , objectData    :: StrictByteString }
  deriving (Eq, Show)

getObject =

headObject =

putObject =

postObject =

deleteObject =
