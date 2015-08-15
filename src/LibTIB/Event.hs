module LibTIB.Event where

import Data.Serialize
    ( Serialize(..)
    , remaining
    , getByteString
    , getWord8
    , getWord16be
    , getWord32be
    , decode
    , Get
    )
import Data.ByteString (ByteString)
import Control.Applicative ((<$>), (<*>))
import Data.Word (Word8, Word16, Word32)
import LibTIB.Entity (Entity, Item(..), PlayerID, get_item, get_entity,
                      get_ship_resources)
import LibTIB.Common (ChatType, DepartType, EntID, Resources, getstr, getbool,
                      TibPacket(..))

data TibEvent
    = Challenge ByteString Word8  -- ^ iv and version byte
    | Alive
    | Notice NoticeType
    | Chat String String String ChatType
    -- ^ message, sender, possible private recipient
    | SectorEnts Node [(EntID, Entity)]
    | Attacked EntID EntID Word16 Bool  -- ^ damage
    | SetShipResources EntID Resources
    | Arrived EntID Entity
    | Departed EntID DepartType
    | RecvTrade PlayerID Word32 Word32 (Maybe Item) PlayerID Word32 Word32 (Maybe Item) Bool
    -- ^ player (not entity!) id, creds, bds
    | LoggedIn String PlayerID
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
    | NoteCreateSuccess  -- = int32(0x0d)
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
    , (0x88, notice)
    , (0x8a, logged_in)
    , (0x8f, sector_info)
    , (0x9d, set_ship_resources)
    , (0xbe, chat)
    , (0xaa, departure)
    , (0xab, arrival)
    , (0xad, attack)
    , (0xd8, trade_received)
    ]

expect8 byte = getWord8 >>= \b -> if b == byte then return ()
        else fail $ "expected " ++ show byte ++ "; got " ++ show b

challenge = Challenge <$> (expect8 0x10 >> getByteString 16) <*> getWord8

ping = return Alive

notice = Notice <$> (getWord8 >>= ntype)
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
        0x0d -> return NoteCreateSuccess  -- = int32(0x0d)
        n -> fail $ "unknown notice type " ++ show n

logged_in = do
    username <- getstr
    pid <- getWord32be
    hardcore <- getbool
    shipskin <- getWord8
    reserved <- getByteString 7
    return $ LoggedIn username pid

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

trade_received = do
    [andere, moi] <- sequence $ replicate 2 getWord32be
    [andcreds, andbds] <- sequence $ replicate 2 getWord32be
    anditem <- to_maybe_item <$> get_item
    [moncreds, monbds] <- sequence $ replicate 2 getWord32be
    monitem <- to_maybe_item <$> get_item
    final <- getbool
    return $ RecvTrade andere andcreds andbds anditem moi moncreds monbds monitem final
    where
    to_maybe_item (UnknownItem _) = Nothing
    to_maybe_item item = Just item

decode_event :: ByteString -> Either String TibEvent
decode_event bs = case decode bs of
    { Left e -> Left e; Right (TibPacket rv) -> Right rv; }

get_event :: Get TibEvent
get_event = get >>= \(TibPacket ev) -> return ev
