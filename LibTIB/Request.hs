module LibTIB.Request where

import Data.Serialize
    ( Serialize(..)
    , remaining
    , getByteString
    , getWord8
    , getWord16be
    , getWord32be
    )
import Data.ByteString (ByteString)
import LibTIB.Common
    ( EntID
    , ChatType
    , ItemType
    , Resources
    , Rarity
    , Server
    , Direction
    )
import LibTIB.Account (Account(..))

data TibRequest
    = Auth ByteString Account Server
    -- ^ iv
    | NewAcc ByteString Account Server
    | Move Direction
    | Attack (Maybe EntID)
    -- ^ Nothing == disengage
    | Follow (Maybe EntID)
    -- ^ Nothing == disengage, 0x8000_0000
    | ResourceTransfer EntID Resources
    | Chat String String String ChatType
    -- ^ message, sender, recipient if ChatType == Private
    | Disconnect Bool -- ^ "defend sector after quit"
    | ListAuctions ItemType Rarity
    | Ping
    deriving Show
