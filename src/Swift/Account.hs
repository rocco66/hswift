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
import Control.Monad (void)

import Data.Conduit (MonadThrow(monadThrow))
import Network.HTTP.Types (methodPost)
import Network.HTTP.Conduit (Request(method), def, httpLbs, responseBody,
                             responseHeaders, method)
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

import Swift.Types (StrictByteString, LazyByteString, Metadata)
import Swift.Helpers (addHeaders)
import Swift.Container (ContainerMetadata, mkContainersMetadataFromJson)
import Swift.Internal (castHeadersToMetadata, setPreferableFormat, debug)

newtype AccountMetadata = AccountMetadata Metadata
  deriving (Eq, Show)

mkAccountMetadataFromHeaders :: ResponseHeaders -> AccountMetadata
mkAccountMetadataFromHeaders = AccountMetadata . castHeadersToMetadata

data Account = Account { accountMetadata   :: AccountMetadata
                       , accountContainers :: [ContainerMetadata]
                       } deriving (Eq, Show)

-- TODO: use Network-HTTP-Types-URI for query stuff

headAccount :: (SwiftAuthenticator auth info) => Swift auth info AccountMetadata
headAccount = do
    request <- setPreferableFormat <$> prepareRequestAndParseUrl
    manager <- getManager
    response <- httpLbs (request {method = "HEAD"}) manager
    return $ mkAccountMetadataFromHeaders $ responseHeaders response

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
    let accountMetadata = mkAccountMetadataFromHeaders $
            responseHeaders response
    accountContainers <- mkContainersMetadataFromJson jsonContainers
    return Account { .. }

postAccount :: (SwiftAuthenticator auth info)
            => [Header]
            -> Swift auth info ()
postAccount headers = do
    baseRequest <- setPreferableFormat <$> prepareRequestAndParseUrl
    manager <- getManager
    -- TODO: get metadate not as Headers
    let request = addHeaders headers baseRequest {method = methodPost}
    void $ httpLbs request manager
