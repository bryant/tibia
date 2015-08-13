module Util.Sock where

import Network.Socket
    ( socket
    , Family(AF_INET)
    , SocketType(Stream)
    , Socket
    )
import Network.Socks5
    ( socksConnect
    , SocksAddress(..)
    , SocksHostAddress(SocksAddrDomainName)
    , SocksConf
    , defaultSocksConf
    )
import Network.BSD (getProtocolNumber)
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
