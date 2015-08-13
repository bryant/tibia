module LibTIB.Entity where

import Data.Serialize
    ( getWord8
    , getWord16be
    , getWord32be
    , getByteString
    , Get
    , label
    , remaining
    , get
    )
import Data.Word (Word8, Word16, Word32)
import Data.ByteString (ByteString)
import Control.Applicative ((<$>), (<*>))

import LibTIB.ItemClasses (WeaponClass, ArmorClass, weap_class, armor_class)
import LibTIB.Common (getbool, EntID, Rarity)

type Hull = Word16

data Entity
    = Planet  -- = int32(0x00000001)
    | Asteroid  -- = int32(0x00000002)
    | Starport [Item]  -- = int32(0x00000003)
    | Ship Hull PlayerID Bool  -- = int32(0x00000004)
    | Npc Hull Word8 Word8  -- = int32(0x00000005)
    | Fighter  -- = int32(0x00000007)
    | DefensePlatform  -- = int32(0x0000000c)
    | Mines  -- = int32(0x0000000d)
    | Intradictor  -- = int32(0x0000000e)
    | RepairDrone  -- = int32(0x0000000f)
    | Garrison  -- = int32(0x00000010)
    | CargoMoney Word32 Word32 -- = int32(0x00000011)
    | CargoItem Item  -- = int32(0x00000012)
    | CargoResource ResourceType Word16  -- = int32(0x00000013)
    | CapturePoint  -- = int32(0x00000014)
    | EntityIDGAF ByteString
    | EntityUnknown Word8 ByteString  -- ^ entity code, bytes
    deriving Show

data PlayerShipClass
    = Shuttle
    | Corvette
    | Frigate
    | Destroyer
    | Cruiser
    | Battleship
    | Dreadnought
    | Titan
    | Flagship
    | Carrier
    | Flayer
    | Executioner
    | Devastator
    | Despoiler
    | Annihilator
    | WyrdInvader
    | WyrdAssassin
    | WyrdReaper
    | WyrdTerminus
    deriving (Show, Enum)

type PlayerID = Word32
type CorpID = Word16
type AllianceID = Word16
type XP = Word32

data Player = Player PlayerID CorpID AllianceID

data ResourceType = Organic | Gas | Metal | Radioactive | Darkmatter
    deriving (Show, Enum)

data Item
    = Weapon WeaponClass Rarity Word8 Bool Bool Word8
    | Armor ArmorClass Rarity Word8 Bool Bool Word8
    | Storage Rarity  -- TODO: fill in
    | Harvester Rarity
    | Engine Rarity
    | Computer Rarity
    | Special Rarity
    | UnknownItem Word8
    deriving Show

get_player_ship_class :: Get PlayerShipClass
get_player_ship_class = label "player ship class" $
    toEnum . fromIntegral <$> getWord8

get_ship_resources = do
    darkmatter <- getWord8
    radioactives <- getWord8
    metals <- getWord8
    gas <- getWord8
    organics <- getWord8
    return $ (darkmatter, radioactives, metals, gas, organics)

get_item_stats :: Get (Rarity, Word8, Bool, Bool, Word8)
get_item_stats = do
    rarity <- get
    dura <- getWord8
    nodrop <- getbool
    boe <- if nodrop then return False else getbool
    cls <- getWord8
    return (rarity, dura, nodrop, boe, cls)

get_weapon = do
    (r, d, nd, boe, cls) <- get_item_stats
    rank <- getWord8
    return $ Weapon (weap_class cls) r d nd boe rank

get_armor = do
    (r, d, nd, boe, cls) <- get_item_stats
    rank <- getWord8
    return $ Armor (armor_class cls) r d nd boe rank

get_harvester = do
    (r, d, nd, boe, cls) <- get_item_stats
    return $ Harvester r

get_engine = do
    (r, d, nd, boe, cls) <- get_item_stats
    return $ Engine r

get_computer = do
    (r, d, nd, boe, cls) <- get_item_stats
    shipcls <- get_player_ship_class  -- computer analogue to rank is class
    return $ Computer r

get_special = do
    (r, d, nd, boe, cls) <- get_item_stats
    shipcls <- get_player_ship_class
    return $ Special r

get_item = label "generic item" $ getWord8 >>= \c -> case c of
    0x01 -> get_weapon
    0x02 -> get_armor
    0x03 -> do
        (r, _, _, _, _) <- get_item_stats
        rank <- getWord8
        return $ Storage r
    0x04 -> get_harvester
    0x05 -> get_engine
    0x06 -> get_computer
    0x07 -> get_special
    n -> return $ UnknownItem n
        -- fail $ "what item is " ++ show n ++ " supposed to be?"

get_combat_entity = label "combat entity" $ do
    hull <- getWord16be
    playerid <- getWord32be
    corpid <- getWord16be
    allid <- getWord16be
    return (hull, playerid, corpid, allid)

get_player_ship = label "player ship update" $ do
    (hull, pid, _, _) <- get_combat_entity
    cls <- get_player_ship_class
    res <- get_ship_resources
    pvpable <- getbool
    xp <- getWord32be
    [weap, armor, stor, harv, eng, comp, spec] <-
        sequence . zipWith label (words "weap armor storage harvester engine computer special") $ replicate 7 get_item
    inv_size <- getWord8
    inventory <- sequence $ replicate (fromIntegral inv_size) get_item
    return $ Ship hull pid pvpable

get_npc = label "npc info" $ do
    hull <- getWord16be
    cls <- getWord8
    lvl <- getWord8
    return $ Npc hull cls lvl

get_starport = label "starport" $ do
    inv_size <- getWord8
    inventory <- sequence $ replicate (fromIntegral inv_size) get_item
    return $ Starport inventory

get_entity :: Get (EntID, Entity)
get_entity = label "sector entity" $ do
    enttype <- getWord8
    entid <- get
    ent <- case enttype of
        0x01 -> EntityIDGAF <$> getByteString 5   -- planet
        0x02 -> EntityIDGAF <$> getByteString 1  -- asteroid
        0x03 -> get_starport
        0x04 -> get_player_ship
        0x05 -> get_npc
        0x07 -> get_combat_entity >> return Fighter
        0x0c -> get_combat_entity >> return DefensePlatform
        0x0d -> get_combat_entity >> return Mines
        0x0e -> get_combat_entity >> return Intradictor
        0x0f -> get_combat_entity >> return RepairDrone
        0x10 -> get_combat_entity >> return Garrison
        0x11 -> CargoMoney <$> getWord32be <*> getWord32be  -- creds, bd
        0x12 -> CargoItem <$> get_item
        0x13 -> CargoResource <$> (toEnum . fromIntegral <$> getWord8)
                              <*> getWord16be
        0x14 -> EntityIDGAF <$> getByteString 1  -- capture point, what is it?
        n -> EntityUnknown n <$> (remaining >>= getByteString)
    return (entid, ent)

is_npc :: Entity -> Bool
is_npc Npc{} = True
is_npc _ = False
