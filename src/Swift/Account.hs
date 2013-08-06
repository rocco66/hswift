{-# LANGUAGE RecordWildCards #-}

module Swift.Account
    ( Account(..)
    , getAccount
    ) where

import Data.ByteString.Lazy (toStrict)

import Data.Conduit (MonadThrow(monadThrow))
import Network.HTTP.Conduit (def, httpLbs, responseBody, responseHeaders)
import Network.HTTP.Types.Header (ResponseHeaders, Header)
import Data.Aeson (Value, json)
import Data.Attoparsec.ByteString.Lazy (Result(Fail, Done), parse)
import Data.Attoparsec.Text(digit, eitherResult, many1)
import qualified Data.List as List

import Swift.Connection (Swift, SwiftAuthenticator(..), prepareRequest,
                         getManager, getUserState,
                         SwiftException(SomeSwiftException, UnknownSwift))
import Swift.Common (setPreferableFormat, LazyByteString)

-- containerCount :: (SwiftAuthenticator auth info)
--                => Account
--                -> Swift auth info Integer
-- containerCount headers = case List.find countHeader (accountHeaders) of
--     Nothing -> monadThrow UnknownSwift "No countainer count info in account"
--     Just bs -> case eitherResult (parse $ many1 digit) of
--         Right i -> return i
--         Left error -> monadThrow UnknownSwift error
--   where
--     countHeader :: Header -> Bool
--     countHeader = (== "x-account-container-count") . fst

 -- 'x-account-bytes-used': '36040493071',
 -- 'x-account-container-count': '47',
 -- 'x-account-object-count': '301134',
 -- 'x-received-bytes': '141516444649',
 -- 'x-timestamp': '1331550165.46492',
 -- 'x-transfered-bytes': '318378717110'}

data AccountInfo = AccountInfo { accountInfoContainerCount :: Integer
                               , accountInfoBytesUsed      :: Integer
                               , accountInfoTimestamp      :: Float
                               } deriving (Eq, Show)

mkAccountInfo :: ResponseHeaders -> Either LazyByteString AccountInfo
mkAccountInfo headers = do
    findHeaderValue headers "x-account-object-count" $ parse int

data Account = Account { accountInfo       :: AccountInfo
                       , accountContainers :: Value
                       } deriving (Eq, Show)

getAccount :: (SwiftAuthenticator auth info) => Swift auth info Account
getAccount = do
    conInfo <- getUserState
    let request = setPreferableFormat $ prepareRequest conInfo $ def
    manager <- getManager
    response <- httpLbs request manager
    containers <- case parse json $ responseBody response of
        Fail _ _ _ -> monadThrow SomeSwiftException
        -- TODO: correct exception
        Done _ r -> return r
    let accountInfo = either (monadThrow . UnknownSwift)
                             return
                             (mkAccountInfo $ responseHeaders response)
        accountContainers = Just containers
    return Account { .. }
