module Swift.Url (useHttps) where

import Swift.Types (StrictByteString)
import qualified Data.ByteString as StrictByteString

-- monad function ???
-- TODO: StringLike argument ???
useHttps :: StrictByteString -> Bool
useHttps = StrictByteString.isPrefixOf "https://"
