{-# LANGUAGE OverloadedStrings, FlexibleInstances #-}

module LibTIB.Common where

import qualified Data.ByteString.Char8 as Char8
import Data.ByteString (ByteString)
import Data.String (IsString)
import Data.Serialize
    ( putWord8
    , putWord16be
    , putWord32be
    , putByteString
    , getWord8
    , getWord16be
    , getByteString
    , Serialize(..)
    , Get
    , Put
    , isolate
    , encode
    , lookAhead
    , runGetState
    )
import Data.Word (Word8, Word32)
import Control.Applicative ((<$>), (<*>))
import Control.Monad (mzero, mplus)
import Util.XXD (xxd)

data Account
    = Account
    { acc_user :: ByteString
    , acc_pass :: ByteString
    , acc_device_id :: ByteString
    , acc_device_type :: ByteString
    , acc_client_version :: ByteString
    }
    deriving Show

newtype EntID = EntID Word32 deriving (Show, Eq)

type Resources = (Word8, Word8, Word8, Word8, Word8)

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
    deriving (Show, Enum, Bounded)

data DepartType
    = Silent  -- 0x00
    | Destroyed  -- 0x01
    | Moved Direction  -- 0x02
    | EarthJump  -- 0x03
    | CorpJump  -- 0x04
    | RiftJump  -- 0x05
    | Logout  -- 0x06
    | Dragged Direction  -- 0x07
    deriving (Show, Eq)

data Direction
    = Northwest | North | Northeast | East | Southeast | South | Southwest
    | West
    deriving (Show, Enum, Eq, Bounded)

data Server = ServRed | ServBlue | ServGreen | ServGray
    deriving (Show, Enum)

data ItemType
    = TyNull  -- ^ never happens, used to properly offset Enum values
    | TyWeapon
    | TyArmor
    | TyStorage
    | TyHarvester
    | TyEngine
    | TyComputer
    | TySpecial
    deriving (Show, Enum, Bounded)

data Rarity
    = RarityNull  -- 0xffffff80, should never happen
    | Common  -- 0x01
    | Uncommon  -- 0x02
    | Rare  -- 0x03
    | UltraRare  -- 0x04
    | Legendary  -- 0x05
    | Precursor  -- 0x06
    | Ultimate  -- 0x07
    deriving (Show, Enum, Bounded)

data Node = Rift Word8 Word8 | NonRift Word8 Word8 deriving Show

newtype TibPacket t = TibPacket t deriving Show

instance Serialize t => Serialize (TibPacket t) where
    put (TibPacket req) = do
        let pktdat = encode req
        putWord16be . fromIntegral $ Char8.length pktdat
        putByteString pktdat

    get = do
        len <- getWord16be  -- packet length sans first two
        bytes <- getByteString $ fromIntegral len
        -- ^ ensures that parsing is kept aligned with packet boundaries
        case runGetState get bytes 0 of
            Left err -> fail $ "error parsing:\n" ++ xxd 16 4 bytes
            Right (rv, "") -> return $ TibPacket rv
            Right (_, unconsumed) ->
                -- require that all input of a packet be consumed
                fail $ "unconsumed input:\n" ++ xxd 16 4 unconsumed ++
                       "\nthe original packet was:\n" ++ xxd 16 4 bytes

instance Serialize Rarity where
    put = putWord8 . fromIntegral . fromEnum
    get = get_enum getWord8 >>= maybe mzero return

instance Serialize ItemType where
    put = putWord8 . fromIntegral . fromEnum
    get = get_enum getWord8 >>= maybe mzero return

instance Serialize EntID where
    put (EntID eid) = putWord32be eid
    get = EntID <$> get

instance Serialize DepartType where
    put = undefined

    get = getWord8 >>= \n -> case n of
        0x00 -> return Silent
        0x01 -> return Destroyed
        0x02 -> Moved <$> get
        0x03 -> return EarthJump
        0x04 -> return CorpJump
        0x05 -> return RiftJump
        0x06 -> return Logout
        0x07 -> Dragged <$> get

instance Serialize Direction where
    put = putWord8 . fromIntegral . fromEnum
    get = get_enum getWord8 >>= maybe mzero return

instance Serialize ChatType where
    put = putWord8 . fromIntegral . fromEnum

    get = getWord8 >>= \n -> case safe_enum n of
        Just n -> return n
        Nothing | n == 0x80 -> return NullChatType
        _ -> mzero

newtype TibPrim p = TibPrim { unprim :: p }

instance Serialize (TibPrim String) where
    put (TibPrim s)
        | length s > 255 = fail $ "tib string exceeds 255: " ++ show s
        | otherwise = putWord8 (fromIntegral $ length s) >> mapM put s
                                                         >> return ()
    get = fmap (TibPrim . Char8.unpack) $
            getWord8 >>= getByteString . fromIntegral

instance Serialize (TibPrim Bool) where
    put (TibPrim False) = putWord8 0x80
    put (TibPrim True) = putWord8 0x7f
    get = getWord8 >>= \c -> case c of
        0x80 -> return $ TibPrim False
        0x7f -> return $ TibPrim True
        n -> fail $ "unexpected tib bool " ++ show n

get_enum :: (Enum a, Bounded a, Integral b) => Get b -> Get (Maybe a)
get_enum getter = safe_enum <$> getter

safe_enum :: (Enum a, Bounded a, Integral b) => b -> Maybe a
safe_enum = _safe_enum minBound maxBound . fromIntegral

_safe_enum :: (Enum a, Bounded a) => a -> a -> Int -> Maybe a
_safe_enum lower upper val
    | fromEnum lower <= val && val <= fromEnum upper = Just $ toEnum val
    | otherwise = Nothing

getstr :: Get String
getstr = fmap unprim get

getbool :: Get Bool
getbool = fmap unprim get

putstr :: String -> Put
putstr = put . TibPrim

putbool :: Bool -> Put
putbool = put . TibPrim

tib_key :: ByteString
tib_key = Char8.pack
    "\x02\x27\x75\xfe\xfe\x08\xbd\x35\xf7\x92\x58\x5d\xd6\xc1\x18\xd9"

-- | deliberately kept polymorphic for bs and [char]
server_ip :: IsString a => a
server_ip = "66.119.27.227"

is_gray :: Node -> Bool
is_gray (NonRift x y) = d (fromIntegral x) (fromIntegral y) 39 39 <= 100
    where
    d :: Int -> Int -> Int -> Int -> Int
    d x y u v = (x - u) ^ 2 + (y - v) ^ 2
is_gray _ = False
