module Swift.Common
    ( setPreferableFormat
    ) where

import Network.HTTP.Conduit (Request(queryString))

setPreferableFormat :: Request m -> Request m
setPreferableFormat req = req { queryString = "?format=json" }
