{-# LANGUAGE OverloadedStrings, FlexibleInstances #-}

module Pwned where

import qualified Data.ByteString as BStr
import qualified Data.ByteString.Builder as Builder
import qualified Data.ByteString.Lazy as L
import Data.Serialize
    ( Get
    , getWord8
    , ensure
    , getByteString
    , runGetPartial
    , runGet
    )
import Control.Applicative ((<$>), (<*>))
import Crypto.Cipher.AES (initAES, encryptCBC)
import Data.ByteString (ByteString)
import Control.Monad (guard)
import Data.Word (Word8)
import Data.Monoid (mconcat, mappend)

import Network.Socket
    ( withSocketsDo
    , socket
    , Family(AF_INET)
    , SocketType(Stream)
    , connect
    , SockAddr(SockAddrInet)
    , Socket
    )
import Network.Socket.ByteString (recv, send)
import Network.BSD (getProtocolNumber, getHostByName, hostAddress)
import Numeric (showHex)

import Control.Concurrent (threadDelay, forkIO)
import XXD (xxd)

data Account
    = Account
    { acc_user :: ByteString
    , acc_pass :: ByteString
    , acc_device_id :: ByteString
    , acc_device_type :: ByteString
    , acc_client_version :: ByteString
    }
    deriving Show

data TibResponse
    = Challenge ByteString Word8  -- ^ iv and version byte
    | Alive
    | UpdateSector ByteString
    -- | 
    | Unknown Word8 ByteString
    deriving Show

data Node = Rift Word8 Word8 | NonRift Word8 Word8

data TibRequest
    = Auth ByteString Account  -- ^ iv, account params
    | Ping

data TimeUnit = Sec | Msec | Usec deriving Show

instance Num a => Num (TimeUnit -> a) where
    fromInteger n = fromIntegral . go
        where
        go Usec = n
        go Msec = n * 1000
        go Sec =  n * 1000000

md5sum :: Account
md5sum = Account
    { acc_user = "md5sum"
    , acc_pass = "md5sum"
    , acc_device_id = "8ef4d93d0123316a4fbc85f65446b156"
    , acc_device_type = "linux 38 unknown 64bit pc newhost2"
    , acc_client_version = "unity_1.3.2-L(86)"
    }

test_challenge = BStr.pack [0, 19, 132, 16, 88, 13, 73, 28, 92, 83, 60, 170, 220, 75, 213, 123, 196, 139, 136, 84, 86]

tib_key :: ByteString
tib_key = BStr.pack [
        0x02, 0x27, 0x75, 0xfe, 0xfe, 0x08, 0xbd, 0x35, 0xf7, 0x92, 0x58, 0x5d,
        0xd6, 0xc1, 0x18, 0xd9
    ]

expect8 byte = getWord8 >>= \b -> guard (b == byte)

get_tib_response :: Get TibResponse
get_tib_response = do
    expect8 0x00
    len <- getWord8  -- remaining bytes in current packet
    ensure $ fromIntegral len
    command <- getWord8
    parse_response command $ fromIntegral len - 1

parse_response :: Word8 -> Int -> Get TibResponse
parse_response 0x84 _ =
    expect8 0x10 >> Challenge <$> getByteString 16 <*> getWord8

parse_response 0x86 _ = return Alive

-- parse_response 0x9e _ = 

parse_response unknown_code remaining =
    Unknown unknown_code <$> getByteString remaining

tib_false, tib_true :: Num a => a
tib_false = 0x80
tib_true = 0x7f

put_tib_request :: TibRequest -> ByteString
put_tib_request (Auth iv (Account user pass devid devtype cliver)) =
    as_tib_packet 0xbd . build_strict $ mconcat [
          len_as_16be user
        , len_as_16be pass
        , len_as_16be devid
        , len_as_16be devtype
        , len_as_16be cliver
        , encbuilder user
        , encbuilder pass
        , encbuilder devid
        , encbuilder devtype
        , encbuilder cliver
        , Builder.word8 tib_false  -- new account: false
        , Builder.word8 tib_false  -- hardcore: false
        , Builder.word8 gray_server
        ]
    where
    len_as_16be = Builder.word16BE . fromIntegral . BStr.length . enc
    enc = aes128_cbc_pkcs7_enc tib_key iv
    encbuilder = Builder.byteString . enc
    gray_server = 0x03  -- gray server id: 0x03

put_tib_request Ping = as_tib_packet 0x86 ""

as_tib_packet :: Word8 -> ByteString -> ByteString
as_tib_packet code payload = build_strict $ mconcat
    [ Builder.word8 0
    , Builder.word8 . fromIntegral $ BStr.length payload + 1  -- +1 for code
    , Builder.word8 code
    , Builder.byteString payload
    ]

build_strict = L.toStrict . Builder.toLazyByteString

aes128_cbc_pkcs7_enc :: ByteString -> ByteString -> ByteString -> ByteString
aes128_cbc_pkcs7_enc key iv plaintext = encrypt iv $ padded plaintext
    where encrypt = encryptCBC $ initAES key

padded bs = bs `BStr.append` padding
    where
    nearest16 = 16 * quot (BStr.length bs + 16) 16  -- fast integer ceil
    padlen = nearest16 - BStr.length bs
    padding = BStr.replicate padlen $ fromIntegral padlen

tib = "66.119.27.227"

main = withSocketsDo $ do
    sock <- socket AF_INET Stream =<< getProtocolNumber "tcp"
    connect sock =<< SockAddrInet 32040 . hostAddress <$> getHostByName tib
    shouldbeiv <- recv sock 1024
    case runGet get_tib_response shouldbeiv of
        Left wut -> error wut
        Right (Challenge iv ver) -> do
            putStrLn $ "Server version: " ++ show ver
            putStrLn $ "IV received: " ++ show iv
            putStrLn $ "Sending md5sum's creds: " ++ show md5sum
            send sock . put_tib_request $ Auth iv md5sum

            forkIO $ ping_thread sock
            recvloop sock
        Right unknown-> do
            putStrLn $ "Unknown response from server: " ++ show unknown

ping_thread :: Socket -> IO ()
ping_thread sock = do
    send sock $ put_tib_request Ping
    threadDelay $ 15 Sec
    -- TODO: do something about the inevitable server response
    ping_thread sock

recvloop :: Socket -> IO ()
recvloop sock = do
    stuff <- recv sock 1024
    putStrLn "Received: "
    putStrLn $ xxd 16 4 stuff
    putStrLn ""
    recvloop sock
