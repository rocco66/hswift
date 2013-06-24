{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Swift.Connection
    ( SwiftAuthenticator(..)
    , SwiftAuthUrl
    , addHeader
    ) where

import Data.Functor ((<$>))
import Data.Conduit (ResourceT, MonadResource)
import Data.ByteString (ByteString)
import Data.Typeable (Typeable)
import Data.Maybe (fromJust)
import Control.Applicative (Applicative)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Base (MonadBase)
import Control.Monad.Reader (ReaderT)
import Control.Monad.Reader.Class (MonadReader(ask))
import Control.Monad.State.Lazy (StateT(runStateT), MonadState(put, get))
import qualified Data.List as List

import Data.CaseInsensitive (CI)
import Data.Conduit (MonadResource, MonadBaseControl)
import Network.HTTP.Conduit (Request(requestHeaders), Response, parseUrl,
                             withManager, httpLbs, responseHeaders)
import Network.HTTP.Types.Header (ResponseHeaders, HeaderName)

type SwiftAuthUrl = String
type Url = String
type SwiftAuthToken = ByteString

-- MonadIO m
class SwiftAuthenticator a where
    addRequestAuthInfo :: a -> Request Swift -> Request Swift
    prepareRequest :: a -> Response body -> Request Swift -> Request Swift
    prepareRequest auth response request = request

data SelcdnAuth = SelcdnAuth
    { selcdnAuthAccount :: {-# UNPACK #-} !ByteString
    , selcdnAuthKey     :: {-# UNPACK #-} !ByteString
    , selcdnAuthUrl     :: Url
    } deriving (Show, Eq, Typeable)

instance SwiftAuthenticator SelcdnAuth where
    addRequestAuthInfo SelcdnAuth { .. } =
        addHeader "X-Auth-User" selcdnAuthAccount .
        (addHeader "X-Auth-Key" selcdnAuthKey)

-- TODO: add conduit manager to state
data SwiftConnectInfo = SwiftConnectInfo
    { swiftConnectInfoUrl :: {-# UNPACK #-} !Url
    , swiftConnectToken   :: {-# UNPACK #-} !SwiftAuthToken }

newtype Swift a = Swift { unSwift :: StateT SwiftConnectInfo
                                         (ReaderT SelcdnAuth
                                             (ResourceT IO)) a }
    deriving (Monad, Functor, Applicative, MonadBase IO, MonadReader SelcdnAuth,
              MonadState SwiftConnectInfo)


addHeader :: CI ByteString -> ByteString -> Request Swift -> Request Swift
addHeader name val req = req { requestHeaders = newHeaders }
  where
    newHeaders = requestHeaders req ++ [(name, val)]

-- withSwift :: (SwiftAuthenticator a, MonadIO m)
--           => SwiftAuthUrl
--           -> a
--           -> Swift ()
--           -> m ()
-- withSwift authUrl authInfo action = do
--     authRequest <- addRequestAuthInfo authInfo <$> parseUrl authUrl
--     withManager $ \manager -> do
--         -- authResponse <- httpLbs authRequest manager
--         action

makeAuthentification :: Swift ()
makeAuthentification req = do
    authInfo@SelcdnAuth { .. } <- ask
    authRequest <- addRequestAuthInfo authInfo <$> parseUrl selcdnAuthUrl
    withManager $ \manager -> do
        -- authResponse <- httpLbs authRequest manager
        headers <- responseHeaders <$> httpLbs authRequest manager
        let url = findHeader "X-Auth-Url" headers
            token = findHeader "X-Auth-Token" headers
        put SwiftConnectInfo { swiftConnectInfoUrl = url
                             , swiftConnectToken = token }
  where
    findHeader :: HeaderName -> ResponseHeaders -> ByteString
    findHeader name = fromJust . List.find (\(n, _) -> n == name)

runSwift :: (SwiftAuthenticator a, MonadIO m)
         => SwiftAuthUrl
         -> a
         -> Swift ()
         -> m ()
runSwift authUrl authInfo action = do
    withManager $ \manager -> do
        -- authResponse <- httpLbs authRequest manager
        action
