{-# LANGUAGE RecordWildCards #-}

module Swift.Account
    ( Account(..)
    , headAccount
    , getAccount
    , postAccount
    ) where

import Data.ByteString.Lazy (toStrict)
import qualified Data.List as List

import Data.Monoid ((<>))
import Data.Functor ((<$>))

import Data.Conduit (MonadThrow(monadThrow))
import Network.HTTP.Conduit (def, httpLbs, responseBody, responseHeaders)
import Network.HTTP.Types.Header (ResponseHeaders, Header)
import Data.Aeson (Value(Array), json)
import Data.Attoparsec.ByteString.Lazy (Result(Fail, Done), parse)
import Data.Attoparsec.Text(digit, eitherResult, many1)
import Data.CaseInsensitive (original)
import Data.String.Like (bs)

import Swift.Monad (Swift, SwiftAuthenticator(..),
                    getManager, getUserState,
                    SwiftException(SomeSwiftException, UnknownSwift),
                    prepareRequestAndParseUrl)

import Swift.Common (setPreferableFormat)
import Swift.Types (StrictByteString, LazyByteString, Metadata)
import Swift.Container (ContainerInfo, mkContainersInfoFromJson)
import Swift.Internal (castHeadersToMetadata, debug)

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

-- data AccountInfo = AccountInfo { accountInfoContainerCount :: Integer
--                                , accountInfoBytesUsed      :: Integer
--                                , accountInfoTimestamp      :: Float
--                                } deriving (Eq, Show)

    -- accountInfoContainerCount <- parseHeader "x-account-object-count" int
    -- accountInfoBytesUsed <- parseHeader "x-account-bytes-used" int
    -- accountInfoTimestamp <- parseHeader "x-timestamp" float
    -- accountInfoBytesUsed <- parseHeader "x-account-bytes-used" int
    -- return AccountInfo { .. }

newtype AccountInfo = AccountInfo Metadata
  deriving (Eq, Show)

mkAccountInfoFromHeaders :: ResponseHeaders -> AccountInfo
mkAccountInfoFromHeaders = AccountInfo . castHeadersToMetadata

data Account = Account { accountInfo       :: AccountInfo
                       , accountContainers :: [ContainerInfo]
                       } deriving (Eq, Show)

-- TODO: use Network-HTTP-Types-URI for query stuff

headAccount = (SwiftAuthenticator auth info) => Swift auth info AccountInfo
headAccount = do
    request <- setPreferableFormat <$> prepareRequestAndParseUrl
    manager <- getManager
    response <- httpLbs (request {method = "HEAD"}) manager
    debug jsonContainers
    return $ mkAccountInfoFromHeaders $ responseHeaders response

getAccount :: (SwiftAuthenticator auth info) => Swift auth info Account
getAccount = do
    -- ??? join setPreferableFormat and prepareRequestAndParseUrl ???
    request <- setPreferableFormat <$> prepareRequestAndParseUrl
    manager <- getManager
    response <- httpLbs request manager
    jsonContainers <- case parse json $ responseBody response of
        Fail _ _ _ -> monadThrow SomeSwiftException
        -- TODO: correct exception
        Done _ (Array r) -> return r
        Done _ d -> monadThrow $
            UnknownSwift $ "Containers list is not array: " <> (bs $ show d)
    debug jsonContainers
    let accountInfo = mkAccountInfoFromHeaders $ responseHeaders response
    accountContainers <- mkContainersInfoFromJson jsonContainers
    return Account { .. }

postAccount =
