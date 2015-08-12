{-# LANGUAGE OverloadedStrings #-}

module LibTIB.Test where

import qualified Data.ByteString as BStr
import Data.Serialize (encode, decode)
import LibTIB.Account (Account(..))
import LibTIB.Common (TibPacket(..), Server(ServGray))
import XXD (xxd)
import LibTIB.Request (TibRequest(Auth))
import LibTIB.Event (TibEvent(Challenge))

md5sum :: Account
md5sum = Account
    { acc_user = "md5sum"
    , acc_pass = "md5sum"
    , acc_device_id = "8ef4d93d0123316a4fbc85f65446b156"
    , acc_device_type = "linux 38 unknown 64bit pc newhost2"
    , acc_client_version = "unity_1.3.2-L(86)"
    }

challenge0 = BStr.pack [0, 19, 132, 16, 88, 13, 73, 28, 92, 83, 60, 170, 220, 75, 213, 123, 196, 139, 136, 84, 86]

check_eq a b = if a == b then putStrLn "Passed." else dump a >> dump b
    where dump = putStrLn . xxd 16 4

test0 = do
    check_eq expected $ encode . TibPacket $ Auth iv md5sum ServGray
    where
    Right (TibPacket (Challenge iv _)) = decode challenge0
    expected = BStr.pack
        [0x00, 0xae, 0xbd, 0x00, 0x10, 0x00, 0x10, 0x00, 0x30, 0x00, 0x30, 0x00, 0x20, 0xa4, 0xab, 0x40, 0x73, 0x2a, 0xde, 0x3c, 0xeb, 0xc4, 0xec, 0x55, 0x47, 0x2d, 0x42, 0x78, 0xce, 0xa4, 0xab, 0x40, 0x73, 0x2a, 0xde, 0x3c, 0xeb, 0xc4, 0xec, 0x55, 0x47, 0x2d, 0x42, 0x78, 0xce, 0xc4, 0x45, 0x45, 0x3b, 0x6f, 0x64, 0x3a, 0x16, 0xda, 0xb5, 0xeb, 0xaf, 0x4d, 0x15, 0x50, 0xa5, 0x2d, 0x6e, 0x24, 0xe7, 0x1a, 0x42, 0x86, 0x38, 0x23, 0xb1, 0x1d, 0x3d, 0x3f, 0x0a, 0x18, 0x55, 0xf5, 0xf5, 0x5a, 0xdf, 0xa0, 0x21, 0x9b, 0xf9, 0xf0, 0xb7, 0x42, 0x4e, 0x76, 0x07, 0x79, 0x16, 0x32, 0x47, 0x48, 0x92, 0x94, 0x78, 0x97, 0x08, 0x9c, 0xfe, 0xca, 0x73, 0x88, 0x0e, 0x7f, 0xf8, 0xdf, 0xa3, 0x3a, 0x94, 0x8a, 0xa3, 0x57, 0x93, 0x18, 0x1b, 0xb7, 0x62, 0xd8, 0xaa, 0x60, 0xc7, 0x8c, 0x16, 0x04, 0x79, 0xdb, 0xdc, 0xff, 0x31, 0xfd, 0x77, 0x7c, 0x9b, 0xf6, 0x5b, 0x0d, 0x8b, 0xb7, 0x18, 0xc6, 0x40, 0x68, 0x60, 0xd8, 0x3e, 0xc7, 0x19, 0x6b, 0xbe, 0xd6, 0x7f, 0x4f, 0x55, 0x7a, 0xa6, 0x84, 0xa6, 0xaa, 0x5e, 0x8f, 0xe2, 0x01, 0x22, 0xcd, 0x09, 0x93, 0x56, 0xc3, 0xae, 0x80, 0x80, 0x03]


