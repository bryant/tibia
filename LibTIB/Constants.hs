module LibTIB.Constants where

import Data.ByteString (ByteString, pack)

tib_key :: ByteString
tib_key = pack [
        0x02, 0x27, 0x75, 0xfe, 0xfe, 0x08, 0xbd, 0x35, 0xf7, 0x92, 0x58, 0x5d,
        0xd6, 0xc1, 0x18, 0xd9
    ]

server_ip = "66.119.27.227"
