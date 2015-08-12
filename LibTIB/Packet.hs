module LibTIB.Packet where

import qualified Data.ByteString as BStr
import qualified Data.Serialize as Ser
import Data.Serialize (Serialize(..), encode, decode, putWord16be,
                       putByteString, getWord16be, getByteString, Get)

newtype TibPacket t = TibPacket t deriving Show

instance Serialize t => Serialize (TibPacket t) where
    put (TibPacket req) = do
        let pktdat = encode req
        putWord16be . fromIntegral $ BStr.length pktdat
        putByteString pktdat

    get = do
        len <- getWord16be  -- packet length sans first two
        g <- getByteString $ fromIntegral len
        -- ^ slurp entire packet length to ensure parsing resumes on a packet
        -- boundary, even upon failure. also, leave in the command code byte
        -- for event deserializer to dispatch on.
        either fail (return . TibPacket) $ decode g
