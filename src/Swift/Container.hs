module Swift.Account
    ( Container(..)
    , getContainer
    ) where

data ContainerInfo = ContainerInfo deriving (Eq, Show)

data Container = Container { containerHeaders :: ResponseHeaders
                           , conatinerObjects :: Objects }
  deriving (Eq, Show, Typeable)

getContainer = undefined
