{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Swift.Connection
    ( SwiftAuthenticator(..)
    , SwiftAuthUrl
    , test
    ) where

import Data.Functor ((<$>))
import Data.ByteString (ByteString)
import Data.Typeable (Typeable)
import Data.Maybe (fromJust)
import Control.Applicative (Applicative)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Base (MonadBase)
import Control.Monad.Reader (ReaderT)
import Control.Monad.Reader.Class (MonadReader(ask))
import Control.Monad.Reader (runReaderT)
import Control.Monad.State.Lazy (StateT(runStateT), MonadState(put, get))
import Control.Monad.IO.Class (liftIO)
import Control.Monad (void)
import qualified Data.List as List
import qualified Data.ByteString.Char8 as Char8

import Control.Monad.Catch (CatchT, MonadCatch(throwM), Exception, runCatchT)
import Data.CaseInsensitive (CI)
import Network.HTTP (Request, Response, HeaderName(HdrCustom),
                     getRequest, setHeaders, mkHeader, simpleHTTP, findHeader,
                     getResponseBody)
import Network.URI (parseURI)

type SwiftAuthUrl = String
type Url = ByteString
type SwiftAuthToken = String

data SwiftException = NoStorageUrlSwiftException
                    | NoStorageTokenSwiftException
                    | WrongPasswordSwiftException
                    | NoAuthHeaderSwiftException String
                    | SomeSwiftException
     deriving (Show, Typeable)

instance Exception SwiftException

-- MonadIO m
class SwiftAuthenticator a where
    addRequestAuthInfo :: a -> Request b -> Request b
    prepareRequest :: a -> Response r -> Request b -> Request b
    prepareRequest auth response = id

data SelcdnAuth = SelcdnAuth
    { selcdnAuthAccount :: {-# UNPACK #-} !ByteString
    , selcdnAuthKey     :: {-# UNPACK #-} !ByteString
    , selcdnAuthUrl     :: String
    } deriving (Show, Eq, Typeable)

mkHeader' :: String -> String -> Header
mkHeader' name val = mkHeader (HdrCustom name) $ Char8.unpack val

instance SwiftAuthenticator SelcdnAuth where
    addRequestAuthInfo SelcdnAuth { .. } = flip setHeaders
        [ mkHeader' "X-Auth-User" selcdnAuthAccount
        , mkHeader' "X-Auth-Key" selcdnAuthKey ]
    prepareRequest _a resp = flip setHeaders
        [ mkHeader' "X-STorage-Key" ]

data SwiftConnectInfo = SwiftConnectInfo
    { swiftConnectInfoUrl :: {-# UNPACK #-} !String
    , swiftConnectToken   :: {-# UNPACK #-} !SwiftAuthToken }
  deriving (Eq, Show)

newtype Swift a = Swift { unSwift :: ReaderT SelcdnAuth
                                         (StateT SwiftConnectInfo
                                             (CatchT IO)) a }
    deriving (Monad, Functor, Applicative, MonadReader SelcdnAuth,
              MonadState SwiftConnectInfo, MonadIO, MonadCatch)

makeAuthentification :: Swift ()
makeAuthentification = do
    authInfo <- ask

    let authReq = addRequestAuthInfo authInfo $
            getRequest $ selcdnAuthUrl authInfo
        throwNoAuthHeader = throwM . NoAuthHeaderSwiftException
        findHeaderE name resp = maybe (throwNoAuthHeader name) return $
                findHeader (HdrCustom name) resp

    conInfo <- liftIO $ simpleHTTP authReq >>= \case
        Left err -> throwM WrongPasswordSwiftException
        Right resp -> do
            swiftConnectInfoUrl <- findHeaderE "X-Storage-Url" resp
            swiftConnectToken <- findHeaderE "X-Storage-Token" resp
            return SwiftConnectInfo { .. }

    put conInfo

getAccount :: Swift String
getAccount = do
    SwiftConnectInfo { .. } <- get
    let baseRequest = getRequest swiftConnectInfoUrl
    result <- liftIO $ simpleHTTP $ prepareRequest baseRequest
    liftIO $ getResponseBody result

runSwift :: SelcdnAuth -> Swift () -> IO ()
runSwift authInfo action = let
    initConInfo = SwiftConnectInfo "" ""  in
    void $ runCatchT (runStateT (runReaderT (unSwift (makeAuthentification >> action)) authInfo ) initConInfo )

test :: IO ()
test = let
    authentificator = SelcdnAuth { selcdnAuthAccount = "7091_hswift"
                                 , selcdnAuthKey = "bh18Px6zxg"
                                 , selcdnAuthUrl = "http://auth.selcdn.ru" } in
    runSwift authentificator (getAccount >>= liftIO . putStrLn . show)
