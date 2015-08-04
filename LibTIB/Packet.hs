module LibTIB.Packet where

import LibTIB.Event ()  -- imports serialize instances
import LibTIB.Request (command_code_of)

newtype TibPacket t = TibPacket t deriving Show

instance Serialize t => Serialize (TibPacket t) where
    put (TibPacket req) = do
        let pktdat = runPut $ put req
        let len = BStr.length pktdat + 1  -- +1 for code
        putWord16be len
        putWord8 $ command_code_of req
        putByteString pktdat

    get = do
        len <- getWord16be  -- packet length sans first two
        command <- getWord8
        g <- getByteString . fromIntegral $ len - 1
        -- ^ slurp entire packet length to ensure parsing resumes on a packet
        -- boundary, even upon failure
        either fail return $ runGet (parse_response command) g
