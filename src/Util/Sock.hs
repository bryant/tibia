module Util.Sock where

import Network.Socket
    ( socket
    , close
    , Family(AF_INET)
    , SocketType(Stream)
    , Socket
    , connect
    , send
    , SockAddr(SockAddrInet)
    )
import Network.Socks5
    ( socksConnect
    , SocksAddress(..)
    , SocksHostAddress(SocksAddrDomainName)
    , SocksConf
    , defaultSocksConf
    )
import Network.BSD (getProtocolNumber, getHostByName, hostAddress)
import Data.Word (Word16)
import Data.ByteString (ByteString)

-- | helper that opens a tcp socket
tcp :: IO Socket
tcp = socket AF_INET Stream =<< getProtocolNumber "tcp"

-- | default tor listen iface
tor :: SocksConf
tor = defaultSocksConf "127.0.0.1" 9050

-- | port number really should be a word16.
sconnect :: SocksConf -> ByteString -> Word16 -> IO Socket
sconnect socksiface dhost dport = fst `fmap` socksConnect socksiface socksaddr
    where
    socksaddr = SocksAddress (SocksAddrDomainName dhost) $ fromIntegral dport

-- | requests tor daemon for new exit ip. hostname/port are those of control
-- iface, typically localhost:9051
newnym :: String -> Word16 -> IO ()
newnym hostname port = do
    sock <- tcp
    addr <- hostAddress `fmap` getHostByName hostname
    connect sock $ SockAddrInet (fromIntegral port) addr
    send sock "AUTHENTICATE\n"
    send sock "SIGNAL NEWNYM\n"
    close sock
