module LibTIB.Packet where

import qualified Data.Serialize as Ser
import Data.Serialize (Serialize)
import LibTIB.Event ()  -- imports serialize instances
import LibTIB.Request (request_code)

newtype TibPacket t = TibPacket t deriving Show

instance Serialize t => Serialize (TibPacket t) where
    put (TibPacket req) = do
        let pktdat = Ser.encode req
        let len = BStr.length pktdat + 1  -- +1 for code
        Ser.putWord16be len
        Ser.putWord8 $ request_code req
        Ser.putByteString pktdat

    get = do
        len <- Ser.getWord16be  -- packet length sans first two
        g <- Ser.getByteString $ fromIntegral len
        -- ^ slurp entire packet length to ensure parsing resumes on a packet
        -- boundary, even upon failure. also, leave in the command code byte
        -- for event deserializer to dispatch on.
        either fail return $ Ser.decode g
