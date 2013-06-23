{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}

module Swift.Connection
    ( SwiftAuthenticator(..)
    , SwiftAuthUrl
    , addHeader
    ) where

import Data.Functor ((<$>))
import Data.ByteString (ByteString)
import Data.Typeable (Typeable)
import Control.Applicative (Applicative)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Base (MonadBase)
import Control.Monad.State.Lazy (StateT(runStateT))

import Data.CaseInsensitive (CI)
import Data.Conduit (MonadResource, MonadBaseControl)
import Network.HTTP.Conduit (Request(requestHeaders), Response, parseUrl,
                             withManager, http)

type SwiftAuthUrl = String

-- MonadIO m
class SwiftAuthenticator a where
    addRequestAuthInfo :: a -> Request Swift -> Request Swift
    prepareRequest :: a -> Response body -> Request Swift -> Request Swift
    prepareRequest auth response request = request

data SelcdnAuth = SelcdnAuth
    { selcdnAuthAccount :: {-# UNPACK #-} !ByteString
    , selcdnAuthKey     :: {-# UNPACK #-} !ByteString
    } deriving (Show, Eq, Typeable)

instance SwiftAuthenticator SelcdnAuth where
    addRequestAuthInfo SelcdnAuth { .. } =
        addHeader "X-Auth-User" selcdnAuthAccount .
        (addHeader "X-Auth-Key" selcdnAuthKey)

data SwiftConnectInfo = SwiftConnectInfo
    { swiftConnectAuthUrl      :: SwiftAuthUrl
    , swiftConnectAuthResponse :: Response () }

-- TODO monad???
data Swift auth = Swift { runSwift :: StateT SwiftConnectInfo IO () }
    deriving (Functor)


addHeader :: CI ByteString -> ByteString -> Request Swift -> Request Swift
addHeader name val req = req { requestHeaders = newHeaders }
  where
    newHeaders = requestHeaders req ++ [(name, val)]

withSwift :: (SwiftAuthenticator a, MonadIO m)
          => SwiftAuthUrl
          -> a
          -> Swift ()
          -> m ()
withSwift authUrl authInfo action = do
    authRequest <- addRequestAuthInfo authInfo <$> parseUrl authUrl
    withManager $ \manager -> do
        authResponse <- http authRequest manager
        action
