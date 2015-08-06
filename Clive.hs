{-# LANGUAGE OverloadedStrings, NoMonomorphismRestriction #-}

module Clive where

import qualified Data.ByteString as BStr
import qualified Data.ByteString.Char8 as Char8
import qualified RadixTrie as R
import qualified Text.Parsec as P

import Pwned  -- , Direction(..), doctek, tib, get_tib_response, TibResponse(..))
import CreateAccount (tor_connect)
import XXD (xxd, as_hex)

import Data.ByteString (ByteString)
import Network.Socket
    ( withSocketsDo
    , socket
    , Family(AF_INET)
    , SocketType(Stream)
    , Socket
    )
import Data.Serialize (runGet, runGetPartial, Result(..))
import Data.Char (isSpace)
import Text.Parsec.ByteString (Parser)
import Network.Socket.ByteString (recv, send)
import Network.BSD (getProtocolNumber)
import Control.Concurrent (threadDelay, forkIO)
import Control.Monad.Trans (liftIO)
import System.IO (withFile, IOMode(ReadMode, WriteMode), hGetLine, hPutStr)
import System.IO.Error (isEOFError)
import Control.Exception (handleJust)
import Control.Applicative ((<$>), (<*>), (<*))
import Control.Monad (mzero, join, void)
import Numeric (readHex, readDec)
import System.Random (randomRIO)

get_ip :: Socket -> IO ByteString
get_ip sock = do
    tor_connect sock "whatismyip.org" 80
    send sock gitit
    recv sock 65536
    where
    gitit = "GET / HTTP/1.1\r\n\
            \User-Agent: curl/7.35.0\r\n\
            \Host: localhost:8000\r\n\
            \Accept: */*\r\n\r\n"

register_account :: Socket -> ByteString -> Account -> IO (Maybe ByteString)
register_account sock iv acc = do
    putStrLn $ "Registering account: " ++ show acc
    send sock . put_tib_request $ NewAcc iv acc
    reply <- recv sock 1024
    case runGet get_tib_response reply of
        Right (Notice NoteAccountCreateSuccess) -> do
            putStrLn "Success!"
            return Nothing
        Right n@Notice{} -> do
            putStrLn $ "Uh oh, received notice: " ++ show n
            return $ Just reply
        _ -> do
            putStrLn "Unrecognized reply."
            return $ Just reply

rand_str :: ByteString -> Int -> IO ByteString
rand_str alpha len = do
    indices <- sequence . replicate len $ randomRIO (0, BStr.length alpha - 1)
    return . BStr.pack $ map (BStr.index alpha) indices

get_name :: IO ByteString
get_name = do
    pos <- withFile "./current" ReadMode $ fmap read . hGetLine
    withFile "./current" WriteMode $ \h -> hPutStr h . show $ pos + 1
    withFile "./usernames" ReadMode $ \h -> do
        sequence_ . replicate pos $ BStr.hGetLine h
        BStr.hGetLine h

generate_account :: IO Account
generate_account = do
    user <- get_name
    pass <- randomRIO (6, 12) >>= rand_str alpha_num
    devid <- rand_str "0123456789abcdef" 32
    devty <- randomRIO (4, 21) >>= rand_str alpha_num
    let cli = "unity_1.3.2-AGP(86)"
    return $ Account user pass devid devty cli
    where alpha_num = "abcdefghijklmnopqrstuvwxyz0123456789"

main :: IO ()
main = withSocketsDo . withFile "./clive.in" ReadMode $ \h -> do
    BStr.putStrLn =<< get_ip =<< socket AF_INET Stream
                             =<< getProtocolNumber "tcp"

    sock <- socket AF_INET Stream =<< getProtocolNumber "tcp"
    tor_connect sock tib 32040
    shouldbeiv <- recv sock 1024

    case runGet get_tib_response shouldbeiv of
        Left wut -> error wut
        Right (Challenge iv ver) -> do
            putStrLn $ "Server version: " ++ show ver
            putStrLn $ "IV received: " ++ show iv

            acc <- generate_account
            err <- register_account sock iv acc
            case err of
                Just bytes -> putStrLn (xxd 16 4 bytes) >> mzero
                Nothing -> do
                    putStrLn "Logging in."
                    send sock . put_tib_request $ Auth iv acc

                    forkIO $ ping_thread sock
                    forkIO $ cmd_loop sock h
                    recvloop sock $ runGetPartial get_tib_response

        Right unknown-> do
            putStrLn $ "Unknown response from server: " ++ show unknown

ping_thread :: Socket -> IO ()
ping_thread sock = do
    send sock $ put_tib_request Ping
    threadDelay $ 15 Sec
    -- TODO: do something about the inevitable server response
    ping_thread sock

recvloop :: Socket -> (ByteString -> Result TibResponse) -> IO ()
recvloop sock f = do
    bytes <- recv sock 1024
    if bytes == ""
        then putStrLn "Disconnected by peer."
        else do
            newf <- consume_with f bytes
            recvloop sock newf

consume_with f bytes = do
    case f bytes of
        Partial f -> return f
        Fail err remaining -> do
            putStrLn $ "parse error on " ++ xxd 16 4 bytes
            putStrLn $ "error was: " ++ err
            consume_with (runGetPartial get_tib_response) remaining
        Done (Unknown cmd hex) remaining -> do
            putStrLn $ "Unknown command " ++ as_hex cmd ++ "\n" ++ xxd 16 4 hex
            consume_with (runGetPartial get_tib_response) remaining
        Done event remaining -> do
            putStrLn $ "Received " ++ show event ++ "\n"
            consume_with (runGetPartial get_tib_response) remaining

eof_error e = if isEOFError e then Just () else Nothing

cmd_loop sock fifo = handleJust eof_error retry $ do
    cli <- BStr.hGetLine fifo
    case parse_command cli of
         Left err -> print err
         Right cmd -> do
            putStrLn $ "Sending received command: " ++ show cmd
            void . send sock $ put_tib_request cmd
    cmd_loop sock fifo
    where retry _ = threadDelay (5 Sec) >> cmd_loop sock fifo

not_spaces1 = P.many1 . P.satisfy $ not . isSpace

lexeme :: Parser a -> Parser a
lexeme act = act <* P.spaces

trie_lookup lab trie inp = do
    entry <- inp
    case R.lookup trie entry of
         Nothing -> P.parserFail $ "not found in trie: " ++ lab ++ ", "
                                   ++ show entry
         Just blah -> return blah

direction = trie_lookup "direction" dirs $ lexeme not_spaces1
    where
    dirs = R.from_list
        [ ("nw", Northwest)
        , ("n", North)
        , ("ne", Northeast)
        , ("e", East)
        , ("se", Southeast)
        , ("s", South)
        , ("sw", Southwest)
        , ("w", West)
        ]

numeral :: (Eq a, Num a) => Parser a
numeral = do
    (reader, digs) <- P.option (readDec, P.digit) prefix
    fst . head . reader <$> P.many1 digs
    where prefix = P.try $ P.string "0x" >> return (readHex, P.hexDigit)

resources = (,,,,) <$> num <*> num <*> num <*> num <*> num
    where num = lexeme numeral

rarity = do
    n <- numeral
    if 1 <= n && n <= 7 then return $ toEnum n else mzero

chat_command = do
    -- only sector chat allowed for now
    msg <- P.many1 P.anyChar
    return $ ReqChat msg "" "" Sector

command :: Parser TibRequest
command = join . trie_lookup "command" cmds $ lexeme not_spaces1
    where
    cmds = R.from_list
        [ ("move", Move <$> direction)
        , ("attack", ReqAttack <$> lexeme numeral)
        , ("follow", ReqFollow <$> lexeme numeral)
        , ("transfer", ReqResourceTransfer <$> lexeme numeral
                                           <*> lexeme resources)
        , ("chat", chat_command)
        , ("auctionhouse", ReqAuctions <$> lexeme numeral <*> lexeme rarity)
        , ("quit", return $ ReqDisconnect False)
        -- , ("sectormsg", CSectorChat <$> rest)
        -- , ("msg", CPrivMsg <$> rest)
        -- , ("list", CListSector)
        -- , ("kill", CAttack . fromIntegral <$> getWord8)
        -- , ("killfirst", CAttackFirst)
        ]

parse_command :: ByteString -> Either P.ParseError TibRequest
parse_command = P.runParser (P.spaces >> command <* P.eof) () ""
