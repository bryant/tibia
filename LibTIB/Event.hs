module LibTIB.Event where

import Data.Serialize
import LibTIB.Common

data TibEvent
    = Challenge ByteString Word8  -- ^ iv and version byte
    | Alive
    | Chat ByteString ByteString ByteString ChatType
    -- ^ message, sender, possible private recipient
    | UpdateSectorEnts Node (IntMap.IntMap Entity)
    | Attacked EntID EntID Word16 Bool  -- ^ damage
    | SetShipResources Word32 Resources
    | EntityArrival Word32 Entity
    | EntityDepart Word32 DepartType
    | Unknown Word8 ByteString
    deriving Show

newtype TibPacket t = TibEvent t deriving Show
