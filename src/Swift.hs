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

import Data.Functor ((<$>))
import Data.Foldable (find)
import Data.Typeable (Typeable)
import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString as StrictByteString
import qualified Data.ByteString.Char8 as Char8

import Network.HTTP.Conduit (Request(requestHeaders), responseHeaders,
                             host, secure, port, parseUrl, withManager, httpLbs)
import Network.HTTP.Types (HeaderName, ResponseHeaders)

type StrictByteString = StrictByteString.ByteString
type SwiftAuthToken = StrictByteString

data SelcdnAuth = SelcdnAuth
    { selcdnAuthAccount :: StrictByteString
    , selcdnAuthKey     :: StrictByteString
    , selcdnAuthUrl     :: StrictByteString
    } deriving (Show, Eq, Typeable)

data SwiftConnectInfo = SwiftConnectInfo
    { swiftConnectInfoHost :: {-# UNPACK #-} !StrictByteString
    , swiftConnectInfoSecure :: {-# UNPACK #-} !Bool
    , swiftConnectInfoPort :: {-# UNPACK #-} !Int
    , swiftConnectToken    :: {-# UNPACK #-} !SwiftAuthToken }
  deriving (Eq, Show)

instance SwiftAuthenticator SelcdnAuth SwiftConnectInfo where
    mkRequestAuthInfo req SelcdnAuth { .. } =
        req { requestHeaders = updatedHeaders
            , host           = selcdnAuthUrl
            -- , secure         = True
            -- , port           = 443
            }
      where
        updatedHeaders = requestHeaders req ++
            [ ("X-Auth-User", selcdnAuthAccount)
            , ("X-Auth-Key", selcdnAuthKey) ]
    prepareRequest SwiftConnectInfo { .. } req =
        req { requestHeaders = updatedHeaders
            , host = swiftConnectInfoHost
            , port = swiftConnectInfoPort
            , secure = swiftConnectInfoSecure }
      where
        updatedHeaders = requestHeaders req ++
            [ ("X-Auth-Token", swiftConnectToken) ]
    mkInfoState resp = let headers = responseHeaders resp in do
        storageUrl <- findHeader "X-Storage-Url" headers
        defReq <- parseUrl $ Char8.unpack storageUrl
        let swiftConnectInfoHost   = host defReq
            swiftConnectInfoPort = port defReq
            swiftConnectInfoSecure = swiftConnectInfoPort == 443
        swiftConnectToken <- findHeader "X-Storage-Token" headers
        return SwiftConnectInfo { .. }
      where
        findHeader :: HeaderName -> ResponseHeaders -> Maybe StrictByteString
        findHeader name headers = snd <$> find ((name ==) . fst) headers

test :: IO ()
test = let
    authentificator = SelcdnAuth { selcdnAuthAccount = "7091_hswift"
                                 , selcdnAuthKey = "bh18Px6zxg"
                                 , selcdnAuthUrl = "auth.selcdn.ru" } in do

    -- req' <- parseUrl "https://auth.selcdn.ru"
    -- putStrLn $ show req'
    -- withManager (httpLbs req') >>= putStrLn . show

    runSwift authentificator (getAccount >>= liftIO . putStrLn . show)
