module Swift.Types
    ( URL
    , LazyByteString
    , StrictByteString
    ) where

import qualified Data.ByteString as StrictByteString
import qualified Data.ByteString.Lazy as LazyByteString

type LazyByteString = LazyByteString.ByteString
type StrictByteString = StrictByteString.ByteString

type URL = StrictByteString
