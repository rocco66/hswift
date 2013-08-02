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
{-# LANGUAGE TypeFamilies #-}

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
import Control.Monad (liftM)
import Control.Monad.Base (MonadBase(..))
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (ReaderT(ReaderT))
import Control.Monad.Reader.Class (MonadReader(ask))
import Control.Monad.Reader (runReaderT)
import Control.Monad.State.Lazy (StateT(StateT, runStateT),
                                 MonadState(put, get))
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans (MonadTrans(lift))
import Control.Monad.Trans.Control (MonadBaseControl(..))
import Control.Monad.Trans.Resource (MonadUnsafeIO(..))
import Control.Monad (void)
import qualified Data.ByteString.Lazy as LazyByteString

import Data.Conduit (MonadResource, ResourceT, MonadThrow(monadThrow),
                     runResourceT)
import Control.Monad.Catch (CatchT(CatchT), MonadCatch(throwM), Exception,
                            runCatchT)
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
    mkRequestAuthInfo :: Request (Swift auth state )
                      -> auth
                      -> Request (Swift auth state)
    mkInfoState :: Response LazyByteString.ByteString -> Maybe state
    prepareRequest :: state
                   -> Request (Swift auth state)
                   -> Request (Swift auth state)
    prepareRequest _s = id

newtype Swift auth info a = Swift { unSwift :: ReaderT auth
                                        (StateT (SwiftState info)
                                            (ResourceT IO)) a }
    deriving (Monad, Functor, Applicative, MonadReader auth,
              MonadState (SwiftState info), MonadIO, MonadThrow, MonadResource,
              MonadUnsafeIO)

-- instance MonadTrans (Swift auth info) where
--     lift a = Swift $ ReaderT $ StateT $ CatchT $ ResourceT $ \_ -> a

instance MonadBase IO (Swift auth info) where
    liftBase = liftIO

instance MonadBaseControl IO (Swift auth info) where
    newtype StM (Swift auth info) a = SwiftStM
        { unSwiftStM :: StM (ReaderT auth
                                (StateT (SwiftState info)
                                        (ResourceT IO))) a }
    liftBaseWith f = Swift . liftBaseWith $
        \runInBase -> f $ liftM SwiftStM . runInBase . unSwiftStM
    restoreM = Swift . restoreM . unSwiftStM

-- instance (SwiftAuthenticator auth info)
--        => MonadTrans (Swift auth info) where
--     lift :: (Monad m) => m a -> Swift auth info a
--     lift = SwiftT . lift . lift . lift . lift

getManager :: (SwiftAuthenticator auth info)
              => Swift auth info Manager
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

makeAuthentification :: (SwiftAuthenticator auth info)
                     => Swift auth info ()
makeAuthentification = do
    authReq <- mkRequestAuthInfo (def { method = "GET" }) <$> ask
    manager <- getManager

    response <- httpLbs authReq manager
    conInfo <-  case isSuccessfulResponse response of
        True -> maybe (monadThrow CanNotMakeInfoStateSwiftException)
                            return $ mkInfoState response
        False -> monadThrow WrongPasswordSwiftException

    put conInfo

runSwift :: (SwiftAuthenticator auth info) => auth -> Swift auth info () -> IO ()
runSwift authInfo action = do
    manager <- newManager def
    let initState = SwiftState { swiftStateManager = manager
                               , swiftStateInfo    = undefined } in
        void $ runResourceT
            (runStateT
                (runReaderT
                    (unSwift (makeAuthentification >> action))
                    authInfo)
                initState)
