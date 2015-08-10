{-# LANGUAGE FlexibleInstances #-}

module LibTIB.Common where

import qualified Data.ByteString.Char8 as Char8
import Data.Serialize (putWord8, getWord8, getByteString, Serialize(..), Get)
import Data.Word (Word8, Word32)
import Control.Applicative ((<$>), (<*>))

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
    deriving (Show, Enum)

data DepartType
    = Silent  -- 0x00
    | Destroyed  -- 0x01
    | Moved Direction  -- 0x02
    | EarthJump  -- 0x03
    | CorpJump  -- 0x04
    | RiftJump  -- 0x05
    | Logout  -- 0x06
    | Dragged Direction  -- 0x07
    deriving Show

data Direction
    = Northwest | North | Northeast | East | Southeast | South | Southwest
    | West
    deriving (Show, Enum)

instance Serialize EntID where
    put (EntID eid) = put eid
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
    get = toEnum . fromIntegral <$> getWord8

instance Serialize ChatType where
    put = putWord8 . fromIntegral . fromEnum

    get = getWord8 >>= \n -> return $ case n of
        0x80 -> NullChatType
        n -> toEnum $ fromIntegral n

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

getstr :: Get String
getstr = fmap unprim get

getbool :: Get Bool
getbool = fmap unprim get
