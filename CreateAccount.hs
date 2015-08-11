module CreateAccount where

import Pwned hiding (main)

import Network.Socks5
    ( socksConnectName
    , socksConnectAddr
    , SocksAddress(..)
    , SocksHostAddress(SocksAddrDomainName)
    , defaultSocksConf
    , defaultSocksConfFromSockAddr
    )
import Data.Serialize (runGet)
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

tor = SockAddrInet 9050 . hostAddress <$> getHostByName "127.0.0.1"

tor_connect sock host port = tor >>= \t -> socksConnectName sock t host port

main = withSocketsDo $ do
    sock <- socket AF_INET Stream =<< getProtocolNumber "tcp"
    tor_connect sock tib 32040
    shouldbeiv <- recv sock 1024
    case runGet get_tib_response shouldbeiv of
        Left wut -> error wut
        Right (Challenge iv ver) -> do
            putStrLn $ "Server version: " ++ show ver
            putStrLn $ "IV received: " ++ show iv
            putStrLn $ "Creating new account: " ++ show doctek
            send sock . put_tib_request $ NewAcc iv doctek GrayServer
            return ()

        Right unknown-> do
            putStrLn $ "Unknown response from server: " ++ show unknown
