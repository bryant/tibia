module LibTIB.Packet where

import qualified Data.ByteString as BStr
import Data.Serialize (Serialize(..), encode, decode, putWord16be,
                       putByteString, getWord16be, getByteString, Get, isolate)

newtype TibPacket t = TibPacket t deriving Show

instance Serialize t => Serialize (TibPacket t) where
    put (TibPacket req) = do
        let pktdat = encode req
        putWord16be . fromIntegral $ BStr.length pktdat
        putByteString pktdat

    get = do
        len <- getWord16be  -- packet length sans first two
        TibPacket `fmap` isolate (fromIntegral len) get
        -- ^ ensures that entire packet is consumed to keep parsing aligned with
        -- packet boundaries
