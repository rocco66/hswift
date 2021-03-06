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

module Swift.Monad
    ( Swift
    , SwiftAuthenticator(..)
    , SwiftAuthUrl
    , SwiftException(..)
    , runSwift
    , getUserState
    , getManager
    , prepareRequestAndParseUrl
    ) where

import Data.Functor ((<$>))
import Data.Typeable (Typeable)
import Control.Applicative (Applicative)
import Control.Monad (liftM)
import Control.Monad.Base (MonadBase(..))
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (ReaderT)
import Control.Monad.Reader.Class (MonadReader(ask))
import Control.Monad.Reader (runReaderT)
import Control.Monad.State.Lazy (StateT(runStateT),
                                 MonadState(put, get))
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Control (MonadBaseControl(..))
import Control.Monad.Trans.Resource (MonadUnsafeIO(..))
import Control.Monad (void)
import qualified Data.ByteString.Lazy as LazyByteString
import qualified Data.ByteString.Char8 as Char8

import Control.Failure (Failure)
import Data.Conduit (MonadResource, ResourceT, MonadThrow(monadThrow),
                     runResourceT)
import Control.Monad.Catch (Exception)
import Network.HTTP.Conduit (Request(method, secure, port), Response,
                             Manager,
                             newManager, httpLbs, responseStatus,
                             managerResponseTimeout, parseUrl, HttpException)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Types (statusIsSuccessful)

import Swift.Types (URL, LazyByteString, StrictByteString)
import Swift.Url (useHttps)

type SwiftAuthUrl = String

data SwiftException = NoStorageUrlSwiftException
                    | NoStorageTokenSwiftException
                    | WrongPasswordSwiftException
                    | NoAuthHeaderSwiftException String
                    | CanNotMakeInfoStateSwiftException
                    | NotJsonValueInHeader StrictByteString String
                    | UnknownSwift StrictByteString  -- split on many exceptions
                    | SomeSwiftException
     deriving (Show, Typeable)

data SwiftState info = SwiftState { swiftStateManager :: Manager
                                  , swiftStateInfo :: info }

instance Exception SwiftException

class SwiftAuthenticator auth state | auth -> state, state -> auth where
    mkRequestAuthInfo :: auth
                      -> Request
                      -> Request
    mkInfoState :: Response LazyByteString.ByteString -> Maybe state
    getStorageUrl :: state -> StrictByteString
    prepareRequest :: state
                   -> Request
                   -> Request
    prepareRequest _s = id

newtype Swift auth info a = Swift { unSwift :: ReaderT auth
                                        (StateT (SwiftState info)
                                            (ResourceT IO)) a }
    deriving (Monad, Functor, Applicative, MonadReader auth,
              MonadState (SwiftState info), MonadIO, MonadThrow, MonadResource,
              MonadUnsafeIO, Failure HttpException)

-- TODO: write monad instance manually

-- instance MonadTrans (Swift auth info) where
--     lift a = Swift $ ReaderT $ StateT $ CatchT $ ResourceT $ \_ -> a

instance MonadBase IO (Swift auth info) where
    liftBase = liftIO

instance MonadBaseControl IO (Swift auth info) where
    newtype StM (Swift auth info) a = SwiftStM
        { unSwiftStM :: StM (ReaderT auth
                                (StateT (SwiftState info)
                                        (ResourceT IO))) a }
    liftBaseWith f = Swift . liftBaseWith $ \runInBase ->
        f $ liftM SwiftStM . runInBase . unSwift
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
                     => URL -> Swift auth info ()
makeAuthentification url = do
    authentificator <- ask
    parsedUrl <- parseUrl $ Char8.unpack url
    let authReq = mkRequestAuthInfo authentificator (fixSslPort parsedUrl url)
    manager <- getManager

    response <- httpLbs authReq manager
    case isSuccessfulResponse response of
        True -> maybe (monadThrow CanNotMakeInfoStateSwiftException)
                            putUserState $ mkInfoState response
        False -> monadThrow WrongPasswordSwiftException
  where
    fixSslPort req url = if useHttps url then
            req { secure = True, port = 443 }
        else
            req

prepareRequestAndParseUrl :: (SwiftAuthenticator auth info)
                          => Swift auth info (Request)
prepareRequestAndParseUrl = do
    conInfo <- getUserState
    let url = getStorageUrl conInfo
    parsedUrl <- parseUrl $ Char8.unpack url
    return $ prepareRequest conInfo $ if useHttps url then
            parsedUrl { secure = True, port = 443}
        else
            parsedUrl

runSwift :: (SwiftAuthenticator auth info)
         => URL
         -> auth
         -> Swift auth info ()
         -> IO ()
runSwift url authInfo action = do
    manager <- newManager $
        tlsManagerSettings { managerResponseTimeout = Just 60000000 }
    let initState = SwiftState { swiftStateManager = manager
                               , swiftStateInfo    = undefined } in
        void $ runResourceT
            (runStateT
                (runReaderT
                    (unSwift (makeAuthentification url >> action))
                    authInfo)
                initState)
