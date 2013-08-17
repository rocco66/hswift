module Swift.Container
    ( Container(..)
    , getContainer
    ) where

data ContainerInfo = ContainerInfo { containerInfoObjectCount :: Integer
                                   , containerInfoBytesUsed   :: Integer
                                   } deriving (Eq, Show)

data Container = Container { containerHeaders :: ContainerInfo
                           , conatinerObjects :: [ObjectInfo] }
  deriving (Eq, Show, Typeable)

mkContainerInfo :: (SwiftAuthenticator auth info)
                => Aeson.Object
                -> Swift auth info ContainerInfo
mkContainerInfo jsonContainer = do
    contianerInfoContainerCount <- findHeaderValue headers
                                   "x-contianer-object-count" $ parse int
    contianerInfoBytesUsed <- findHeaderValue headers
                              "x-contianer-bytes-used" $ parse int
    contianerInfoTimestamp <- findHeaderValue headers
                              "x-timestamp" $ parse float
    contianerInfoBytesUsed <- findHeaderValue headers
        "x-contianer-bytes-used" $ parse int


getContainer = undefined
