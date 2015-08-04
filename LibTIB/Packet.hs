module LibTIB.Packet where

import LibTIB.Event ()  -- imports serialize instances
import LibTIB.Request (request_code)

newtype TibPacket t = TibPacket t deriving Show

instance Serialize t => Serialize (TibPacket t) where
    put (TibPacket req) = do
        let pktdat = runPut $ put req
        let len = BStr.length pktdat + 1  -- +1 for code
        putWord16be len
        putWord8 $ request_code req
        putByteString pktdat

    get = do
        len <- getWord16be  -- packet length sans first two
        g <- getByteString $ fromIntegral len
        -- ^ slurp entire packet length to ensure parsing resumes on a packet
        -- boundary, even upon failure. also, leave in the command code byte
        -- for event deserializer to dispatch on.
        either fail return $ runGet (parse_response command) g
