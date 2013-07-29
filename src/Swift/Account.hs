{-# LANGUAGE RecordWildCards #-}

module Swift.Account
    ( Account(..)
    , getAccount
    ) where

import Network.HTTP.Conduit (Request)
import Network.URI (nullURI)
import Control.Monad.Reader.Class (MonadReader(ask))
import Control.Monad.State.Lazy (StateT(runStateT), MonadState(put, get))
import Control.Monad.IO.Class (liftIO)

import Data.Aeson (Array)

import Swift.Common (setPreferableFormat)
import Swift.Connection (Swift, SwiftAuthenticator(..))

data Account = Account { accountHeaders    :: [Header]
                       , accountContainers :: Array }

getAccount :: (SwiftAuthenticator auth info) => Swift auth info String
getAccount = do
    conInfo <- get
    let request = prepareRequest conInfo (mkRequest GET nullURI)
    result <- liftIO $ simpleHTTP
        (request { rqURI = setPreferableFormat (rqURI request) })
    liftIO $ getResponseBody result
