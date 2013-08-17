module Swift.Url (useHttps) where

import Swift.Common (StrictByteString)
import qualified Data.ByteString as StrictByteString

-- monad function ???
-- TODO: StringLike argument ???
useHttps :: StictByteString -> Bool
useHttps = "https://" `StrictByteString.isPrefixOf
