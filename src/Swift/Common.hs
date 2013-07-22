module Swift.Common
    ( setPreferableFormat
    ) where

import Network.URI (URI(uriQuery))

setPreferableFormat :: URI -> URI
setPreferableFormat uri = uri { uriQuery = "?format=json" }
