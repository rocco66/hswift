{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Swift.Connection
    ( Swift
    , SwiftAuthenticator(..)
    , SwiftConnectInfo(..)
    , SwiftAuthUrl
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
import Network.HTTP (Request, HeaderName(HdrCustom), RequestMethod(GET),
                     simpleHTTP, findHeader, mkRequest)
import Network.URI (nullURI)

type SwiftAuthUrl = String
type Url = ByteString
type SwiftAuthToken = String
type AuthRequest = Request String

data SwiftException = NoStorageUrlSwiftException
                    | NoStorageTokenSwiftException
                    | WrongPasswordSwiftException
                    | NoAuthHeaderSwiftException String
                    | SomeSwiftException
     deriving (Show, Typeable)

instance Exception SwiftException

class SwiftAuthenticator a where
    mkRequestAuthInfo :: AuthRequest -> a -> AuthRequest
    prepareRequest :: a -> SwiftConnectInfo -> Request String -> Swift a (Request String)
    prepareRequest _a _conInfo = return

data SwiftConnectInfo = SwiftConnectInfo
    { swiftConnectInfoUrl :: {-# UNPACK #-} !String
    , swiftConnectToken   :: {-# UNPACK #-} !SwiftAuthToken }
  deriving (Eq, Show)

newtype Swift auth res = Swift { unSwift :: ReaderT auth
                                         (StateT SwiftConnectInfo
                                             (CatchT IO)) res }
    deriving (Monad, Functor, Applicative, MonadReader auth,
              MonadState SwiftConnectInfo, MonadIO, MonadCatch)

makeAuthentification :: (SwiftAuthenticator auth) => Swift auth ()
makeAuthentification = do
    authReq <- mkRequestAuthInfo (mkRequest GET nullURI) <$> ask

    let throwNoAuthHeader = throwM . NoAuthHeaderSwiftException
        findHeaderE name resp = maybe (throwNoAuthHeader name) return $
                findHeader (HdrCustom name) resp

    conInfo <- liftIO (simpleHTTP authReq) >>= \case
        Left err -> throwM WrongPasswordSwiftException
        Right resp-> do
            swiftConnectInfoUrl <- findHeaderE "X-Storage-Url" resp
            swiftConnectToken <- findHeaderE "X-Storage-Token" resp
            return SwiftConnectInfo { .. }

    put conInfo

runSwift :: (SwiftAuthenticator auth) => auth -> Swift auth () -> IO ()
runSwift authInfo action = let
    initConInfo = SwiftConnectInfo "" ""  in
    void $ runCatchT (runStateT (runReaderT (unSwift (makeAuthentification >> action)) authInfo ) initConInfo )
