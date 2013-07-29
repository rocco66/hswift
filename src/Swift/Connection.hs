{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE KindSignatures #-}

module Swift.Connection
    ( Swift
    , SwiftAuthenticator(..)
    , SwiftAuthUrl
    , SwiftException(..)
    , runSwift
    ) where

import Data.Functor ((<$>))
import Data.ByteString (ByteString)
import Data.Typeable (Typeable)
import Control.Applicative (Applicative)
import Control.Monad.Base (MonadBase)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (ReaderT)
import Control.Monad.Reader.Class (MonadReader(ask))
import Control.Monad.Reader (runReaderT)
import Control.Monad.State.Lazy (StateT(runStateT), MonadState(put, get))
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans (MonadTrans(lift))
import Control.Monad (void)
import qualified Data.ByteString.Lazy as LazyByteString

import Data.Conduit (ResourceT, MonadBaseControl(..))
import Control.Monad.Catch (CatchT, MonadCatch(throwM), Exception, runCatchT)
import Network.HTTP.Conduit (Request(method), Response, Manager, newManager,
                             def, httpLbs, responseStatus)
import Network.URI (nullURI)
import Network.HTTP.Types (statusIsSuccessful)

type SwiftAuthUrl = String
type Url = ByteString

data SwiftException = NoStorageUrlSwiftException
                    | NoStorageTokenSwiftException
                    | WrongPasswordSwiftException
                    | NoAuthHeaderSwiftException String
                    | CanNotMakeInfoStateSwiftException
                    | SomeSwiftException
     deriving (Show, Typeable)

data SwiftState info = SwiftState { swiftStateManager :: Manager
                                  , swiftStateInfo :: info }

instance Exception SwiftException

class SwiftAuthenticator auth state | auth -> state, state -> auth where
    mkRequestAuthInfo :: Request (SwiftT auth state m)
                      -> auth
                      -> Request (SwiftT auth state m)
    mkInfoState :: Response LazyByteString.ByteString -> Maybe state
    prepareRequest :: state
                   -> Request (SwiftT auth state m)
                   -> Request (SwiftT auth state m)
    prepareRequest _s = id

newtype SwiftT auth info m a = SwiftT { unSwift :: ReaderT auth
                                         (StateT (SwiftState info)
                                             (CatchT
                                                 (ResourceT m))) a }
    deriving (Monad, Functor, Applicative, MonadReader auth,
              MonadState (SwiftState info), MonadIO, MonadCatch, MonadBaseControl IO)

instance MonadBaseControl IO (SwiftT auth info) where

instance (SwiftAuthenticator auth info)
       => MonadTrans (SwiftT auth info) where
    lift :: (Monad m) => m a -> SwiftT auth info m a
    lift = SwiftT . lift . lift . lift . lift

type Swift auth info = SwiftT auth info IO

getManager :: (SwiftAuthenticator auth info, Monad m)
              => SwiftT auth info m Manager
getManager = swiftStateManager <$> get

putManager :: (SwiftAuthenticator auth info)
              => Manager
              -> Swift auth info ()
putManager m = get >>= (\s -> return $ s { swiftStateManager = m }) >>= put


getUserState :: (SwiftAuthenticator auth info)
              => Swift auth info info
getUserState = swiftStateInfo <$> get

putUserState :: (SwiftAuthenticator auth info)
              => info
              -> Swift auth info ()
putUserState i = get >>= (\s -> return $ s { swiftStateInfo = i }) >>= put

isSuccessfulResponse :: Response a -> Bool
isSuccessfulResponse = statusIsSuccessful . responseStatus

makeAuthentification :: (SwiftAuthenticator auth info, Monad m)
                     => SwiftT auth info m ()
makeAuthentification = do
    authReq <- mkRequestAuthInfo (def { method = "GET" }) <$> ask
    manager <- getManager

    response <- httpLbs authReq manager
    conInfo <-  case isSuccessfulResponse response of
        True -> maybe (throwM CanNotMakeInfoStateSwiftException)
                            return $ mkInfoState response
        False -> throwM WrongPasswordSwiftException

    put conInfo

runSwift :: (SwiftAuthenticator auth info) => auth -> Swift auth info () -> IO ()
runSwift authInfo action = do
    manager <- newManager def
    let initState = SwiftState { swiftStateManager = manager
                               , swiftStateInfo    = undefined } in
        void $ runCatchT
            (runStateT
                (runReaderT
                    (unSwift (makeAuthentification >> action))
                    authInfo)
                initState)
