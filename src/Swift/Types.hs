module Swift.Types
    ( URL
    , Metadata
    , LazyByteString
    , StrictByteString
    ) where

import qualified Data.ByteString as StrictByteString
import qualified Data.ByteString.Lazy as LazyByteString

import Data.HashMap.Strict (HashMap)
import qualified Data.Aeson as Aeson

type LazyByteString = LazyByteString.ByteString
type StrictByteString = StrictByteString.ByteString

type URL = StrictByteString

type Metadata = HashMap StrictByteString Aeson.Value
