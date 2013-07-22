{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}

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

import Network.HTTP (Request(rqURI), HeaderName(HdrCustom),
                     setHeaders, mkHeader)
import Network.URI (URI, parseURI)

data SelcdnAuth = SelcdnAuth
    { selcdnAuthAccount :: String
    , selcdnAuthKey     :: String
    , selcdnAuthUrl     :: URI
    } deriving (Show, Eq, Typeable)

instance SwiftAuthenticator SelcdnAuth where
    mkRequestAuthInfo req SelcdnAuth { .. } =
      setHeaders (req { rqURI = selcdnAuthUrl })
          [ mkHeader (HdrCustom "X-Auth-User") selcdnAuthAccount
          , mkHeader (HdrCustom "X-Auth-Key") selcdnAuthKey ]
    prepareRequest _a SwiftConnectInfo { .. } req = return $ setHeaders req
        [ mkHeader (HdrCustom "X-Auth-Token") swiftConnectToken ]

test :: IO ()
test = let
    authUri = fromJust $ parseURI "http://auth.selcdn.ru"
    authentificator = SelcdnAuth { selcdnAuthAccount = "7091_hswift"
                                 , selcdnAuthKey = "bh18Px6zxg"
                                 , selcdnAuthUrl = authUri } in
    runSwift authentificator (getAccount >>= liftIO . putStrLn . show)
