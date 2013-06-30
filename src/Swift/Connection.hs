{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
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

import Control.Monad.Catch (CatchT, MonadCatch(throwM), Exception, runCatchT)
import Data.CaseInsensitive (CI)
import Network.Http.Client (Request, Response, RequestBuilder, Method(GET),
                            getHeader, http, receiveResponse,
                            setHeader, sendRequest, emptyBody, openConnection,
                            buildRequest)
import Data.String.Like (string)
import OpenSSL (withOpenSSL)

type SwiftAuthUrl = String
type Url = ByteString
type SwiftAuthToken = ByteString

data SwiftException = NoStorageUrlSwiftException
                    | NoStorageTokenSwiftException
     deriving (Show, Typeable)

instance Exception SwiftException

-- MonadIO m
class SwiftAuthenticator a where
    addRequestAuthInfo :: a -> RequestBuilder ()
    prepareRequest :: a -> Response -> RequestBuilder ()
    prepareRequest auth response = return ()

data SelcdnAuth = SelcdnAuth
    { selcdnAuthAccount :: {-# UNPACK #-} !ByteString
    , selcdnAuthKey     :: {-# UNPACK #-} !ByteString
    , selcdnAuthUrl     :: Url
    } deriving (Show, Eq, Typeable)

instance SwiftAuthenticator SelcdnAuth where
    addRequestAuthInfo SelcdnAuth { .. } = do
        setHeader "X-Auth-User" selcdnAuthAccount
        setHeader "X-Auth-Key" selcdnAuthKey

-- TODO: add conduit manager to state
data SwiftConnectInfo = SwiftConnectInfo
    { swiftConnectInfoUrl :: {-# UNPACK #-} !Url
    , swiftConnectToken   :: {-# UNPACK #-} !SwiftAuthToken }

newtype Swift a = Swift { unSwift :: ReaderT SelcdnAuth
                                         (StateT SwiftConnectInfo
                                             (CatchT IO)) a }
    deriving (Monad, Functor, Applicative, MonadReader SelcdnAuth,
              MonadState SwiftConnectInfo, MonadIO)

makeAuthentification :: Swift ()
makeAuthentification = do
    authInfo <- ask
    con <- liftIO $ openConnection "selcdn.ru" 443
    authRequest <- liftIO $ buildRequest $ do
         http GET "/redefine/me"
         addRequestAuthInfo authInfo

    liftIO $ sendRequest con authRequest emptyBody
    conInfo <- liftIO $ receiveResponse con $ \resp _is -> do
        url <- getHeaderExc NoStorageUrlSwiftException resp "X-Auth-Url"
        token <- getHeaderExc NoStorageTokenSwiftException resp "X-Auth-Token"
        return SwiftConnectInfo { swiftConnectInfoUrl = url
                                , swiftConnectToken   = token }
    put conInfo
  where
    getHeaderExc :: (Exception e) => e -> Response -> ByteString -> IO ByteString
    getHeaderExc exc resp name = maybe (throwM exc) return $ getHeader resp name

runSwift :: SelcdnAuth -> Swift () -> IO ()
runSwift authInfo action = let
    initConInfo = SwiftConnectInfo "" ""  in
    void $ runCatchT (runStateT (runReaderT (unSwift (makeAuthentification >> action)) authInfo ) initConInfo )

test :: IO ()
test = withOpenSSL $ let
    authentificator = SelcdnAuth { selcdnAuthAccount = "7091_hswift"
                                 , selcdnAuthKey = "bh18Px6zxg"
                                 , selcdnAuthUrl = "auth.selcdn.ru" } in
    runSwift authentificator (return ())
