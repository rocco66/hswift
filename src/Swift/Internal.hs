module Swift.Internal
    ( castJsonObjectToBsPair
    ) where

import Data.Monoid ((<>))
import Data.Functor ((<$>))

import Data.String.Like (bs)
import Data.Conduit (MonadThrow(monadThrow))
import qualified Data.Aeson as Aeson
import qualified Data.HashMap.Strict as HashMap

import Swift.Types (StrictByteString)
import Swift.Monad (Swift, SwiftAuthenticator, SwiftException(UnknownSwift))

-- TODO: extract to some other file
castJsonObjectToBsPair :: (SwiftAuthenticator auth info)
                       => Aeson.Object
                       -> Swift auth info [(StrictByteString, StrictByteString)]
castJsonObjectToBsPair = mapM castToBsPair . HashMap.toList
  where
    castToBsPair (name, (Aeson.String val)) = return (bs name, bs val)
    castToBsPair (_, val) = monadThrow $
        UnknownSwift $ "Not a string" <> (bs $ show val)
