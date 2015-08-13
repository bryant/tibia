{-# LANGUAGE OverloadedStrings #-}

module CreateAccount where

import Network.Socks5
    ( socksConnectName
    , socksConnectAddr
    , SocksAddress(..)
    , SocksHostAddress(SocksAddrDomainName)
    , defaultSocksConf
    , defaultSocksConfFromSockAddr
    )
import Data.Serialize (decode, encode)
import Network.Socket
    ( withSocketsDo
    , socket
    , Family(AF_INET)
    , SocketType(Stream)
    , connect
    , SockAddr(SockAddrInet)
    )
import Network.Socket.ByteString (recv, send)
import Network.BSD (getProtocolNumber, getHostByName, hostAddress)
import Control.Applicative ((<$>))
import Control.Monad (forever)
import LibTIB.Event (TibEvent(Challenge), decode_event)
import LibTIB.Request (TibRequest(NewAcc, Auth), encode_request)
import LibTIB.Common (Account(..), server_ip, Server(ServGray))
import Util.XXD (xxd)

tor = SockAddrInet 9050 . hostAddress <$> getHostByName "127.0.0.1"

tor_connect sock host port = tor >>= \t -> socksConnectName sock t host port

doctek = Account "i\x9814you" "i\x9814you" "8ef4d93d0123316a4fbc85f65446b156"
            "android os mifitech44u" "client_v1.7-gp"

main = withSocketsDo $ do
    sock <- socket AF_INET Stream =<< getProtocolNumber "tcp"
    tor_connect sock server_ip 32040
    shouldbeiv <- recv sock 1024
    case decode_event shouldbeiv of
        Left wut -> error wut
        Right (Challenge iv ver) -> do
            putStrLn $ "Server version: " ++ show ver
            putStrLn $ "IV received: " ++ show iv
            putStrLn $ "Creating new account: " ++ show doctek
            send sock . encode_request $ NewAcc iv doctek ServGray
            recvdump sock 65536
            send sock . encode_request $ Auth iv doctek ServGray
            forever $ recvdump sock 1024

        Right unknown-> do
            putStrLn $ "Unknown response from server: " ++ show unknown

recvdump sock n = do
    stuff <- recv sock n
    putStrLn $ "got stuff!\n\n"
    putStrLn $ xxd 16 4 stuff
