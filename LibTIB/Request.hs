module LibTIB.Request where

import qualified Data.ByteString as BStr
import Crypto.Cipher.AES (initAES, encryptCBC, decryptCBC)
import Data.Serialize
    ( Serialize(..)
    , Put
    , putByteString
    , putWord8
    , putWord16be
    , putWord32be
    )
import Data.ByteString (ByteString)
import Data.Word (Word8)
import LibTIB.Common
    ( EntID(..)
    , ChatType
    , ItemType
    , Resources
    , Rarity
    , Server
    , Direction
    , putbool
    , putstr
    , Account(..)
    )
import LibTIB.Constants (tib_key)

data TibRequest
    = Auth ByteString Account Server
    -- ^ iv
    | NewAcc ByteString Account Server
    | Move Direction
    | Attack (Maybe EntID)
    -- ^ Nothing == disengage
    | Follow (Maybe EntID)
    -- ^ Nothing == disengage, 0x8000_0000
    | TransferRes EntID Resources
    | Chat String String String ChatType
    -- ^ message, sender, recipient if ChatType == Private
    | Disconnect Bool -- ^ "defend sector after quit"
    | ListAuctions ItemType Rarity
    | Ping
    deriving Show

instance Serialize TibRequest where
    put (Auth iv acc serv) = cmd 0xbd $ do
        put_auth iv acc
        putbool False  -- not a new account
        putbool hardcore
        putWord8 . fromIntegral $ fromEnum serv
        where hardcore = False

    put (NewAcc iv acc serv) = cmd 0xbd $ do
        put_auth iv acc
        putbool True  -- new account
        putbool False
        putWord8 . fromIntegral $ fromEnum serv
        mapM_ putWord16be [face, attr, hair]
        where (face, attr, hair) = (0, 0, 0)

    put (Move dir) = cmd 0xc6 . putWord8 . fromIntegral $ fromEnum dir
    put (Attack entid) = cmd 0xcc $ maybe nullid put entid
    put (Follow entid) = cmd 0xcb $ maybe nullid put entid
    put (TransferRes (EntID targid) (orgs, gas, mets, rads, dm)) = cmd 0x10 $
        mapM_ putWord8 [orgs, gas, mets, rads, dm] >> putWord32be targid
    put (Chat msg pmsender pmreceiver ty) = cmd 0xbe $ do
        mapM_ putstr [msg, pmsender, pmreceiver]
        putWord8 . fromIntegral $ fromEnum ty
    put (Disconnect defend) = cmd 0x82 $ putbool defend
    put (ListAuctions itemcls rarity) = cmd 0x2c $
        mapM_ (putWord8 . fromIntegral) [fromEnum itemcls, fromEnum rarity]
    put Ping = cmd 0x86 $ return ()

    -- maybe later
    get = undefined

cmd code = (putWord8 code >>)

nullid = putWord32be 0x80000000

put_auth iv (Account user pass devid devtype ver) = do
    mapM_ (putWord16be . fromIntegral . BStr.length . padded) acc_stuff
    mapM_ (putByteString . aes128_cbc_pkcs7_enc tib_key iv) acc_stuff
    where acc_stuff = [user, pass, devid, devtype, ver]

aes128_cbc_pkcs7_enc :: ByteString -> ByteString -> ByteString -> ByteString
aes128_cbc_pkcs7_enc key iv plaintext = encrypt iv $ padded plaintext
    where encrypt = encryptCBC $ initAES key

aes128_cbc_pkcs7_dec :: ByteString -> ByteString -> ByteString -> ByteString
aes128_cbc_pkcs7_dec key = decryptCBC $ initAES key

padded bs = bs `BStr.append` padding
    where
    nearest16 = 16 * quot (BStr.length bs + 16) 16  -- fast integer ceil + 1
    padlen = nearest16 - BStr.length bs
    padding = BStr.replicate padlen $ fromIntegral padlen
