module Swift.Object
    ( Object(..)
    , getObject
    ) where

import qualified Data.Aeson as Aeson

data ObjectInfo = ObjectInfo { objectInfoObjectCount :: Integer
                             , objectInfoBytesUsed   :: Integer
                             } deriving (Eq, Show)

data Object = Object { objectHeaders :: ObjectInfo
                     , conatinerObjects :: [ObjectInfo] }
  deriving (Eq, Show, Typeable)

getObject = undefined
