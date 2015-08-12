module LibTIB.Event where

import Data.Serialize
    ( Serialize(..)
    , remaining
    , getByteString
    , getWord8
    , getWord16be
    , getWord32be
    )
import Data.ByteString (ByteString)
import Control.Applicative ((<$>), (<*>))
import Data.Word (Word8, Word16)
import LibTIB.Entity (Entity, get_entity, get_ship_resources)
import LibTIB.Common (ChatType, DepartType, EntID, Resources, getstr)

data TibEvent
    = Challenge ByteString Word8  -- ^ iv and version byte
    | Alive
    | Chat String String String ChatType
    -- ^ message, sender, possible private recipient
    | SectorEnts Node [(EntID, Entity)]
    | Attacked EntID EntID Word16 Bool  -- ^ damage
    | SetShipResources EntID Resources
    | Arrived EntID Entity
    | Departed EntID DepartType
    | Unknown Word8 ByteString
    deriving Show

data Node = Rift Word8 Word8 | NonRift Word8 Word8 deriving Show

instance Serialize TibEvent where
    put = undefined  -- unused for now.

    get = do
        code <- getWord8
        case lookup code event_code of
            Nothing -> Unknown code <$> (remaining >>= getByteString)
            Just parseit -> parseit

event_code =
    [ (0x84, challenge)
    , (0x86, ping)
    , (0x8f, sector_info)
    , (0x9d, set_ship_resources)
    , (0xbe, chat)
    , (0xaa, departure)
    , (0xab, arrival)
    , (0xad, attack)
    ]

expect8 byte = getWord8 >>= \b -> if b == byte then return ()
        else fail $ "expected " ++ show byte ++ "; got " ++ show b

challenge = Challenge <$> (expect8 0x10 >> getByteString 16) <*> getWord8

ping = return Alive

chat = Chat <$> getstr <*> getstr <*> getstr <*> get

sector_info = do
    -- UpdateCurrentSector
    rift <- getWord8
    xpos <- getWord8
    ypos <- getWord8
    entity_count <- fromIntegral <$> getWord16be
    entities <- sequence $ replicate entity_count get_entity
    let node = (if rift == 0 then NonRift else Rift) xpos ypos
    return $ SectorEnts node entities

set_ship_resources = SetShipResources <$> get <*> get_ship_resources

departure = Departed <$> get <*> get

arrival = get_entity >>= \(entid, ent) -> return (Arrived entid ent)

attack = do
    attacker <- get
    target <- get
    damage <- getWord16be
    hit <- getWord8
    return $ Attacked attacker target damage (hit /= 0x00)
