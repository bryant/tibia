{-# LANGUAGE FlexibleInstances #-}

import qualified Data.ByteString.Char8 as Char8
import Data.Serialize (putWord8, getWord8, getByteString, Serialize(..), Get)
import Data.Word (Word8, Word32)

newtype EntID = EntID Word32 deriving (Show, Eq)

type Resources = (Word8, Word8, Word8, Word8, Word8)

data ChatType
    = Server
    | Corp
    | Sector
    | Private
    | Alliance
    | Console
    | Event
    | Alert
    | Market
    | MarketEvent
    | Universe
    | NullChatType  -- 0xffffff80
    deriving (Show, Enum)

instance Serialize ChatType where
    put = undefined

    get = getWord8 >>= \n -> return $ case n of
        0x80 -> NullChatType
        n -> toEnum $ fromIntegral n

newtype TibPrim p = TibPrim { unprim :: p }

instance Serialize (TibPrim String) where
    put (TibPrim s)
        | length s > 255 = fail $ "tib string exceeds 255: " ++ show s
        | otherwise = putWord8 (fromIntegral $ length s) >> mapM put s
                                                         >> return ()
    get = fmap (TibPrim . Char8.unpack) $
            getWord8 >>= getByteString . fromIntegral

instance Serialize (TibPrim Bool) where
    put (TibPrim False) = putWord8 0x80
    put (TibPrim True) = putWord8 0x7f
    get = getWord8 >>= \c -> case c of
        0x80 -> return $ TibPrim False
        0x7f -> return $ TibPrim True
        n -> fail $ "unexpected tib bool " ++ show n

getstr :: Get String
getstr = fmap unprim get

getbool :: Get Bool
getbool = fmap unprim get
