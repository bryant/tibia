module XXD where

import qualified Data.ByteString as BStr
import Data.ByteString (ByteString)
import Data.Char (chr)
import Data.Word (Word8)
import Data.List (intercalate)
import Text.Printf (printf)

xxd :: Int -> Int -> ByteString -> String
xxd rowsize groupsize raw = unlines $
        zipWith (xxd_row rowsize groupsize) rows [0, rowsize..]
    where rows = BStr.unpack raw `groups_of` rowsize

xxd_row :: Int -> Int -> [Word8] -> Int -> String
xxd_row rowsize grpsize row offset =
        printf "%08x" offset ++ " │ " ++ hexes ++ spacer ++ ascii ++ "\n" ++
        replicate 8 ' ' ++ " │ " ++ chars ++ spacer
    where
    chunked = intercalate "  " . map unwords . flip groups_of grpsize
    hexes = chunked $ map as_hex row
    chars = chunked $ map ((: " ") . as_ascii ' ') row
    ascii = map (as_ascii '.') row
    spacer = replicate padlen ' ' ++ " │ "
    padlen = if rlen >= rowsize then 0
                                else width_of rowsize - width_of rlen
        where rlen = length row
    width_of n = n * 3 + (n - 1) `quot` grpsize

groups_of :: [a] -> Int -> [[a]]
groups_of xs size
    | length xs <= size = [xs]
    | otherwise = take size xs : groups_of (drop size xs) size

as_ascii :: Char -> Word8 -> Char
as_ascii fillval byte = if printable byte then chr $ fromIntegral byte
                                          else fillval
    where printable b = b >= 32 && b <= 126

as_hex :: Word8 -> String
as_hex = printf "%02x"
