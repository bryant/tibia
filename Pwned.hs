{-# LANGUAGE OverloadedStrings, FlexibleInstances #-}

module Pwned where

import qualified Data.ByteString as BStr
import qualified Data.ByteString.Builder as Builder
import qualified Data.ByteString.Lazy as L
import qualified Data.IntMap as IntMap
import Data.Serialize
    ( Get
    , getWord8
    , getWord16be
    , getWord32be
    , ensure
    , getByteString
    , runGet
    , remaining
    )
import Control.Applicative ((<$>), (<*>))
import Crypto.Cipher.AES (initAES, encryptCBC, decryptCBC)
import Data.ByteString (ByteString)
import Control.Monad (guard)
import Data.Word (Word8, Word16, Word32)
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

import Control.Concurrent (threadDelay, forkIO)
import LibTIB.Entity

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
    | Notice NoticeType
    | ChatEvent ByteString ByteString ByteString ChatType
    | UpdateSectorEnts Node [(Word32, Entity)]
    | AttackEvent Word32 Word32 Word16 Bool
    | SetShipResources Word32 Resources
    | EntityArrival Word32 Entity
    | EntityDepart Word32 DepartType
    | NameToID ByteString Word32
    | Unknown Word8 ByteString
    deriving Show

data NoticeType
    = NoteNone  -- = int32(0x00)
    | NoteLoginInvalid  -- = int32(0x01)
    | NoteNameUnavailable  -- = int32(0x02)
    | NotePlayerNotFound  -- = int32(0x03)
    | NoteExploreBonus  -- = int32(0x06)
    | NoteRequestFailed  -- = int32(0x08)
    | NoteTradeFailCancel  -- = int32(0x0a)
    | NoteBanned  -- = int32(0x0c)
    | NoteAccountCreateSuccess  -- = int32(0x0d)
    deriving Show

data ChatType
    = Server
    | Corp
    | Sector
    | Private
    | Alliance
    | Console
    | Event
    | Alert
    | Market
    | MarketEvent
    | Universe
    | NullChatType  -- 0xffffff80
    deriving (Show, Enum)

data DepartType
    = Silent  -- = int32(0x00000000)
    | Destroyed  -- = int32(0x00000001)
    | Moved Direction  -- = int32(0x00000002)
    | EarthJump  -- = int32(0x00000003)
    | CorpJump  -- = int32(0x00000004)
    | RiftJump  -- = int32(0x00000005)
    | Logout  -- = int32(0x00000006)
    | Dragged Direction  -- = int32(0x00000007)
    deriving (Show, Eq)

data Node = Rift Word8 Word8 | NonRift Word8 Word8 deriving Show

data TibRequest
    = Auth ByteString Account Server  -- ^ iv, account params
    | NewAcc ByteString Account Server
    | Move Direction
    | ReqAttack Word32  -- ^ entity id, 0x8000_0000 to disengage attack
    | ReqFollow Word32
    | ReqResourceTransfer Word32 Resources
    | ReqChat String String String ChatType
    | ReqDisconnect Bool
    | ReqAuctions Word8 Rarity
    | Ping
    deriving Show

data Server = RedServer | BlueServer | GreenServer | GrayServer
    deriving (Show, Enum)

data Direction
    = Northwest | North | Northeast | East | Southeast | South | Southwest
    | West
    deriving (Show, Enum, Eq)

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

moineau :: Account
moineau = Account
    { acc_user = "moineau"
    , acc_pass = "volumetric"
    , acc_device_id = "eb5332abc59f8d965989703bcdcb067e"
    , acc_device_type = "android os 444 api19 ktu84q9042e71c samsung gti9"
    , acc_client_version = "unity_1.3.2-AGP(86)"
    }

doctek :: Account
doctek = Account
    { acc_user = "doctek"
    , acc_pass = "ketcod"
    , acc_device_id = "cdd34b7d596a75c43c236d7a424eab5a"
    , acc_device_type = "android os 444 api19 ktu84q9042e71c huawei p8"
    , acc_client_version = "unity_1.3.2-AGP(86)"
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
    len <- getWord16be  -- packet length sans first two
    command <- getWord8
    g <- getByteString . fromIntegral $ len - 1
    either fail return $ runGet (parse_response command) g

parse_response :: Word8 -> Get TibResponse
parse_response 0x84 =
    expect8 0x10 >> Challenge <$> getByteString 16 <*> getWord8

parse_response 0x86 = return Alive

parse_response 0xbe =
    ChatEvent <$> parse_tib_string <*> parse_tib_string <*> parse_tib_string
              <*> (chat_type <$> getWord8)
    where
    chat_type 0x80 = NullChatType
    chat_type n = toEnum $ fromIntegral n

parse_response 0x88 = Notice <$> (getWord8 >>= ntype)
    where
    ntype n = case n of
        0x00 -> return NoteNone  -- = int32(0x00)
        0x01 -> return NoteLoginInvalid  -- = int32(0x01)
        0x02 -> return NoteNameUnavailable  -- = int32(0x02)
        0x03 -> return NotePlayerNotFound  -- = int32(0x03)
        0x06 -> return NoteExploreBonus  -- = int32(0x06)
        0x08 -> return NoteRequestFailed  -- = int32(0x08)
        0x0a -> return NoteTradeFailCancel  -- = int32(0x0a)
        0x0c -> return NoteBanned  -- = int32(0x0c)
        0x0d -> return NoteAccountCreateSuccess  -- = int32(0x0d)
        n -> fail $ "unknown notice type " ++ show n

parse_response 0x8f = do
    -- UpdateCurrentSector
    rift <- getWord8
    xpos <- getWord8
    ypos <- getWord8
    entity_count <- fromIntegral <$> getWord16be
    entities <- sequence $ replicate entity_count get_entity
    let node = (if rift == 0 then NonRift else Rift) xpos ypos
    return $ UpdateSectorEnts node entities

parse_response 0xaa = do
    entid <- getWord32be
    departtype <- getWord8
    EntityDepart entid <$> case departtype of
        0x00 -> return Silent
        0x01 -> return Destroyed
        0x02 -> fmap Moved $ getWord8 >>= getdir
        0x03 -> return EarthJump
        0x04 -> return CorpJump
        0x05 -> return RiftJump
        0x06 -> return Logout
        0x07 -> fmap Dragged $ getWord8 >>= getdir
    where
    getdir n
        | 0 <= n && n <= 7 = return . toEnum $ fromIntegral n
        | otherwise = fail $ "unknown direction type" ++ show n

parse_response 0xab = do
    (entid, ent) <- get_entity
    return $ EntityArrival entid ent

-- parse_response 0x9e _ = 
parse_response 0xad = do
    attacker <- getWord32be
    target <- getWord32be
    damage <- getWord16be
    hit <- getWord8
    return $ AttackEvent attacker target damage (hit /= 0x00)

parse_response 0x9d = SetShipResources <$> entid <*> get_ship_resources
    where entid = getWord32be

parse_response 0x8a = do
    name <- parse_tib_string
    pid <- getWord32be
    hardcore <- get_tib_bool
    getByteString 8  -- ship skin and reserved bytes
    return $ NameToID name pid

parse_response unknown_code =
    Unknown unknown_code <$> (remaining >>= getByteString)

parse_tib_string :: Get ByteString
parse_tib_string = getWord8 >>= getByteString . fromIntegral

encode_tib_string s = Builder.word8 len `mappend` Builder.stringUtf8 s
    where len = fromIntegral $ length s

tib_false, tib_true :: Num a => a
tib_false = 0x80
tib_true = 0x7f

put_tib_request :: TibRequest -> ByteString
put_tib_request (Auth iv acc sv) = as_tib_packet 0xbd . build_strict $ mconcat [
          build_auth iv acc
        , Builder.word8 tib_false  -- new account: false
        , Builder.word8 tib_false  -- hardcore: false
        , Builder.word8 . fromIntegral $ fromEnum sv
    ]

put_tib_request (NewAcc iv acc sv) = as_tib_packet 0xbd . build_strict $ mconcat [
          build_auth iv acc
        , Builder.word8 tib_true -- new account: true
        , Builder.word8 tib_false
        , Builder.word8 . fromIntegral $ fromEnum sv
        , Builder.word16BE 0x00  -- face as u16
        , Builder.word16BE 0x00  -- attributes
        , Builder.word16BE 0x00  -- hair
    ]

put_tib_request (Move dir) =
    as_tib_packet 0xc6 . BStr.singleton . fromIntegral $ fromEnum dir

put_tib_request (ReqAttack entid) =
    as_tib_packet 0xcc . build_strict $ Builder.word32BE entid

put_tib_request (ReqFollow entid) =
    as_tib_packet 0xcb . build_strict $ Builder.word32BE entid

put_tib_request (ReqResourceTransfer target (orgs, gas, mets, rads, dm)) =
    as_tib_packet 0x10 . build_strict $ mconcat [
              Builder.word8 orgs
            , Builder.word8 gas
            , Builder.word8 mets
            , Builder.word8 rads
            , Builder.word8 dm
            , Builder.word32BE target
        ]

put_tib_request (ReqChat msg pmsender pmreceiver ty) =
    as_tib_packet 0xbe . build_strict $ mconcat [
              encode_tib_string msg
            , encode_tib_string pmsender
            , encode_tib_string pmreceiver
            , Builder.word8 . fromIntegral $ fromEnum ty
        ]

put_tib_request (ReqDisconnect defend) =
    as_tib_packet 0x82 . BStr.singleton $ put_tib_bool defend

put_tib_request (ReqAuctions itemcls rarity) =
    as_tib_packet 0x2c $ BStr.pack [itemcls, fromIntegral $ fromEnum rarity]

put_tib_request Ping = as_tib_packet 0x86 ""

as_tib_packet :: Word8 -> ByteString -> ByteString
as_tib_packet code payload = build_strict $ mconcat
    [ Builder.word8 0
    , Builder.word8 . fromIntegral $ BStr.length payload + 1  -- +1 for code
    , Builder.word8 code
    , Builder.byteString payload
    ]

build_auth :: ByteString -> Account -> Builder.Builder
build_auth iv (Account user pass devid devtype cliver) = mconcat [
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
    ]
    where
    len_as_16be = Builder.word16BE . fromIntegral . BStr.length . enc
    enc = aes128_cbc_pkcs7_enc tib_key iv
    encbuilder = Builder.byteString . enc

build_strict = L.toStrict . Builder.toLazyByteString

aes128_cbc_pkcs7_enc :: ByteString -> ByteString -> ByteString -> ByteString
aes128_cbc_pkcs7_enc key iv plaintext = encrypt iv $ padded plaintext
    where encrypt = encryptCBC $ initAES key

aes128_cbc_pkcs7_dec :: ByteString -> ByteString -> ByteString -> ByteString
aes128_cbc_pkcs7_dec key = decryptCBC $ initAES key

padded bs = bs `BStr.append` padding
    where
    nearest16 = 16 * quot (BStr.length bs + 16) 16  -- fast integer ceil
    padlen = nearest16 - BStr.length bs
    padding = BStr.replicate padlen $ fromIntegral padlen

tib = "66.119.27.227"

{-
main = withSocketsDo $ do
    sock <- socket AF_INET Stream =<< getProtocolNumber "tcp"
    connect sock =<< SockAddrInet 32040 . hostAddress <$> getHostByName tib
    shouldbeiv <- recv sock 1024
    case runGet get_tib_response shouldbeiv of
        Left wut -> error wut
        Right (Challenge iv ver) -> do
            putStrLn $ "Server version: " ++ show ver
            putStrLn $ "IV received: " ++ show iv
            putStrLn $ "Sending creds: " ++ show doctek
            send sock . put_tib_request $ Auth iv doctek

            forkIO $ ping_thread sock
            recvloop sock $ runGetPartial get_tib_response

        Right unknown-> do
            putStrLn $ "Unknown response from server: " ++ show unknown
-}
