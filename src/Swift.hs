{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Swift
    ( module Swift.Connection
    , module Swift.Account
    -- , module Swift.Container
    -- , module Swift.Object
    , test
    ) where

import Swift.Connection
import Swift.Account
-- import Swift.Container
-- import Swift.Object

import Data.Typeable (Typeable)
import Data.Maybe (fromJust)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Catch (MonadCatch(throwM))

import Network.URI (URI, parseURI)

type SwiftAuthToken = String

data SelcdnAuth = SelcdnAuth
    { selcdnAuthAccount :: String
    , selcdnAuthKey     :: String
    , selcdnAuthUrl     :: URI
    } deriving (Show, Eq, Typeable)

data SwiftConnectInfo = SwiftConnectInfo
    { swiftConnectInfoUrl :: {-# UNPACK #-} !String
    , swiftConnectToken   :: {-# UNPACK #-} !SwiftAuthToken }
  deriving (Eq, Show)

instance SwiftAuthenticator SelcdnAuth SwiftConnectInfo where
    mkRequestAuthInfo req SelcdnAuth { .. } =
      setHeaders (req { rqURI = selcdnAuthUrl })
          [ mkHeader (HdrCustom "X-Auth-User") selcdnAuthAccount
          , mkHeader (HdrCustom "X-Auth-Key") selcdnAuthKey ]
    prepareRequest SwiftConnectInfo { .. } req =
        setHeaders (req { rqURI = fromJust (parseURI swiftConnectInfoUrl) })
        [ mkHeader (HdrCustom "X-Auth-Token") swiftConnectToken ]
    mkInfoState resp = do
        swiftConnectInfoUrl <- findHeader (HdrCustom "X-Storage-Url") resp
        swiftConnectToken <- findHeader (HdrCustom "X-Storage-Token") resp
        return SwiftConnectInfo { .. }

test :: IO ()
test = let
    authUri = fromJust $ parseURI "http://auth.selcdn.ru"
    authentificator = SelcdnAuth { selcdnAuthAccount = "7091_hswift"
                                 , selcdnAuthKey = "bh18Px6zxg"
                                 , selcdnAuthUrl = authUri } in
    runSwift authentificator (getAccount >>= liftIO . putStrLn . show)
