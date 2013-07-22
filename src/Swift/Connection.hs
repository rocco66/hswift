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
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (ReaderT)
import Control.Monad.Reader.Class (MonadReader(ask))
import Control.Monad.Reader (runReaderT)
import Control.Monad.State.Lazy (StateT(runStateT), MonadState(put))
import Control.Monad.IO.Class (liftIO)
import Control.Monad (void)

import Control.Monad.Catch (CatchT, MonadCatch(throwM), Exception, runCatchT)
import Network.HTTP (Request, Response, HeaderName(HdrCustom),
                     RequestMethod(GET), simpleHTTP, findHeader, mkRequest)
import Network.URI (nullURI)

type SwiftAuthUrl = String
type Url = ByteString
type AuthRequest = Request String

data SwiftException = NoStorageUrlSwiftException
                    | NoStorageTokenSwiftException
                    | WrongPasswordSwiftException
                    | NoAuthHeaderSwiftException String
                    | CanNotMakeInfoStateSwiftException
                    | SomeSwiftException
     deriving (Show, Typeable)

instance Exception SwiftException

class SwiftAuthenticator auth state | auth -> state, state -> auth where
    mkRequestAuthInfo :: AuthRequest -> auth -> AuthRequest
    mkInfoState :: Response String -> Maybe state
    prepareRequest :: state -> Request String -> Request String
    prepareRequest _s = id

newtype Swift auth info res = Swift { unSwift :: ReaderT auth
                                         (StateT info
                                             (CatchT IO)) res }
    deriving (Monad, Functor, Applicative, MonadReader auth,
              MonadState info, MonadIO, MonadCatch)


makeAuthentification :: (SwiftAuthenticator auth info) => Swift auth info ()
makeAuthentification = do
    authReq <- mkRequestAuthInfo (mkRequest GET nullURI) <$> ask

    conInfo <- liftIO (simpleHTTP authReq) >>= \case
        Left err -> throwM WrongPasswordSwiftException
        Right resp -> maybe (throwM CanNotMakeInfoStateSwiftException)
                            return $ mkInfoState resp

    put conInfo

runSwift :: (SwiftAuthenticator auth info) => auth -> Swift auth info () -> IO ()
runSwift authInfo action =
    void $ runCatchT (runStateT (runReaderT (unSwift (makeAuthentification >> action)) authInfo ) undefined )
