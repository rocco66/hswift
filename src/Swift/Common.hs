module Swift.Common
    ( setPreferableFormat
    ) where

setPreferableFormat :: String -> String
setPreferableFormat url = url ++ "?format=json"
