{-# LANGUAGE RecordWildCards #-}

module Swift.Account
    ( Account(..)
    , getAccount
    ) where

import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString.Lazy as LazyByteString

import Network.HTTP.Conduit (Request(path), def, httpLbs, responseBody, withManager, newManager)
import Network.HTTP.Types.Header (ResponseHeaders)

import Data.Aeson (Array)

import Swift.Connection (Swift, SwiftAuthenticator(..), prepareRequest,
                         getManager, getUserState)

type LazyByteString = LazyByteString.ByteString

data Account = Account { accountHeaders    :: ResponseHeaders
                       , accountContainers :: Array }

getAccount :: (SwiftAuthenticator auth info) => Swift auth info LazyByteString
getAccount = do
    conInfo <- getUserState
    let request = prepareRequest conInfo $ def
    liftIO $ putStrLn $ show request
    manager <- getManager
    response <- httpLbs request manager
    return $ responseBody response
