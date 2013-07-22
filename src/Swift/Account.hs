{-# LANGUAGE RecordWildCards #-}

module Swift.Account
    ( Account(..)
    , getAccount
    ) where

import Network.HTTP (Request, Response, HeaderName(HdrCustom), Header,
                     getRequest, setHeaders, mkHeader, simpleHTTP, findHeader,
                     getResponseBody)
import Control.Monad.Reader.Class (MonadReader(ask))
import Control.Monad.State.Lazy (StateT(runStateT), MonadState(put, get))
import Control.Monad.IO.Class (liftIO)

import Data.Aeson (Array)

import Swift.Common (setPreferableFormat)
import Swift.Connection (Swift, SwiftAuthenticator(..), SwiftConnectInfo(..))

data Account = Account { accountHeaders    :: [Header]
                       , accountContainers :: Array }

getAccount :: (SwiftAuthenticator auth) => Swift auth String
getAccount = do
    conInfo@SwiftConnectInfo { .. } <- get
    _a <- ask
    let targetUrl = setPreferableFormat swiftConnectInfoUrl
        baseRequest = getRequest targetUrl
    correctRequest <- prepareRequest _a conInfo baseRequest
    result <- liftIO $ simpleHTTP correctRequest
    liftIO $ getResponseBody result
