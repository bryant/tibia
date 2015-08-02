{-# LANGUAGE OverloadedStrings, NoMonomorphismRestriction #-}

module Clive where

import qualified Data.ByteString as BStr
import qualified RadixTrie as R
import qualified Text.Parsec as P

import Pwned hiding (main)  -- , Direction(..), doctek, tib, get_tib_response, TibResponse(..))
import CreateAccount (tor_connect)

import Data.ByteString (ByteString)
import Network.Socket
    ( withSocketsDo
    , socket
    , Family(AF_INET)
    , SocketType(Stream)
    , Socket
    )
import Data.Serialize (runGet, runGetPartial)
import Data.Char (isSpace)
import Text.Parsec.ByteString (Parser)
import Network.Socket.ByteString (recv, send)
import Network.BSD (getProtocolNumber)
import Control.Concurrent (threadDelay, forkIO)
import Control.Monad.Trans (liftIO)
import System.IO (withFile, IOMode(ReadMode))
import System.IO.Error (isEOFError)
import Control.Exception (handleJust)
import Control.Applicative ((<$>), (<*>), (<*))
import Control.Monad (mzero, join, void)

get_ip :: Socket -> IO ByteString
get_ip sock = do
    tor_connect sock "whatismyip.org" 80
    send sock gitit
    recv sock 1024
    where
    gitit = "GET / HTTP/1.1\r\n\
            \User-Agent: curl/7.35.0\r\n\
            \Host: localhost:8000\r\n\
            \Accept: */*\r\n\r\n"

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
            putStrLn $ "Sending creds: " ++ show doctek
            send sock . put_tib_request $ Auth iv doctek

            forkIO $ ping_thread sock
            forkIO $ cmd_loop sock h
            recvloop sock $ runGetPartial get_tib_response

        Right unknown-> do
            putStrLn $ "Unknown response from server: " ++ show unknown

eof_error e = if isEOFError e then Just () else Nothing

cmd_loop sock fifo = handleJust eof_error (const $ cmd_loop sock fifo) $ do
    cli <- BStr.hGetLine fifo
    case parse_command cli of
         Left err -> print err
         Right cmd -> do
            putStrLn $ "Sending received command: " ++ show cmd
            void . send sock $ put_tib_request cmd
    cmd_loop sock fifo

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

command :: Parser TibRequest
command = join . trie_lookup "command" cmds $ lexeme not_spaces1
    where
    cmds = R.from_list
        [ ("move", Move <$> direction)
        -- , ("sectormsg", CSectorChat <$> rest)
        -- , ("msg", CPrivMsg <$> rest)
        -- , ("list", CListSector)
        -- , ("kill", CAttack . fromIntegral <$> getWord8)
        -- , ("killfirst", CAttackFirst)
        ]

parse_command :: ByteString -> Either P.ParseError TibRequest
parse_command = P.runParser (P.spaces >> command <* P.eof) () ""
