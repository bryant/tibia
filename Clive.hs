{-# LANGUAGE OverloadedStrings, NoMonomorphismRestriction #-}

module Clive where

import qualified Data.ByteString as BStr
import qualified Data.ByteString.Char8 as Char8
import qualified RadixTrie as R
import qualified Text.Parsec as P

import Pwned  -- , Direction(..), doctek, tib, get_tib_response, TibResponse(..))
import LibTIB.Entity (is_npc, Resources, Entity(Npc))
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
import Data.Word (Word32)
import Data.List (delete)
import Text.Parsec.ByteString (Parser)
import Network.Socket.ByteString (recv, send)
import Network.BSD (getProtocolNumber)
import Control.Concurrent (threadDelay, forkIO)
import Control.Monad.Trans (liftIO)
import System.IO (withFile, openFile, IOMode(ReadMode, WriteMode), hGetLine,
                  hPutStr, Handle)
import System.IO.Error (isEOFError)
import System.Posix.Files (createNamedPipe)
import Control.Exception (handleJust)
import Control.Applicative ((<$>), (<*>), (<*))
import Control.Monad (mzero, join, void, forever, when)
import Control.Monad.State (get, put, modify, runStateT, StateT)
import Data.IORef (newIORef, readIORef, writeIORef, atomicModifyIORef', IORef)
import Numeric (readHex, readDec)
import System.Random (randomRIO)

data CliveState
    = CliveState
    { c_id :: Word32
    , c_auto_attack :: Bool
    , c_following :: Maybe Word32
    , c_attacking :: Maybe Word32
    , c_targets :: [Word32]
    , c_resources :: Resources
    }
    deriving Show

data CliveCmd
    = RawCmd TibRequest
    | ListStatus
    | SetAuto Bool
    | Follow Word32

get_ip :: Socket -> IO ByteString
get_ip sock = do
    tor_connect sock "whatismyip.org" 80
    send sock gitit
    collect ""
    where
    gitit = "GET / HTTP/1.1\r\n\
            \User-Agent: curl/7.35.0\r\n\
            \Host: localhost:8000\r\n\
            \Accept: */*\r\n\r\n"
    collect bs = do
        more <- recv sock 65536
        if more == "" then return bs else collect $ BStr.append bs more

send_tib :: Socket -> TibRequest -> IO ()
send_tib sock req = void $ do
    putStrLn $ "Sending " ++ show req
    putStrLn $ ">>>>>>\n" ++ xxd 16 4 (put_tib_request req)
    send sock $ put_tib_request req

register_account :: Socket -> ByteString -> Account -> IO (Maybe ByteString)
register_account sock iv acc = do
    putStrLn $ "Registering account: " ++ show acc
    send_tib sock $ NewAcc iv acc
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

create_fifo :: Account -> IO Handle
create_fifo (Account {acc_user=u}) = do
    let fifo = "./" ++ Char8.unpack u ++ ".fifo"
    putStrLn $ "Creating " ++ fifo
    createNamedPipe fifo 0o640
    openFile fifo ReadMode

main :: IO ()
main = withSocketsDo $ do
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
            h <- create_fifo acc
            err <- register_account sock iv acc
            case err of
                Just bytes -> putStrLn (xxd 16 4 bytes) >> mzero
                Nothing -> do
                    putStrLn "Logging in."
                    send_tib sock $ Auth iv acc

                    var <- newIORef . Partial $ runGetPartial get_tib_response
                    me <- get_clives_id acc sock var

                    clive <- newIORef $ CliveState me False Nothing Nothing []
                                                   (0, 0, 0, 0, 0)
                    forkIO $ ping_thread sock
                    forkIO $ cmd_loop sock h clive
                    forever $ run_clive sock var clive

        Right unknown-> do
            putStrLn $ "Unknown response from server: " ++ show unknown

get_clives_id :: Account -> Socket -> IORef (Result TibResponse) -> IO Word32
get_clives_id acc sock var = do
    ev <- get_event sock var
    case ev of
        Just (NameToID name cid) | name == acc_user acc -> return cid
        _ -> get_clives_id acc sock var

ping_thread :: Socket -> IO ()
ping_thread sock = do
    send_tib sock Ping
    threadDelay $ 15 Sec
    -- TODO: do something about the inevitable server response
    ping_thread sock

get_event :: Socket -> IORef (Result TibResponse) -> IO (Maybe TibResponse)
get_event sock var = do
    result <- readIORef var
    case result of
        Partial f -> do
            bytes <- recv sock 1024
            writeIORef var $ f bytes
            return Nothing
        Fail err remaining -> do
            -- putStrLn $ "parse error on " ++ xxd 16 4 bytes
            putStrLn $ "error was: " ++ err
            writeIORef var $ runGetPartial get_tib_response remaining
            return Nothing
        Done event remaining -> do
            case event of
                Unknown code bs -> do
                    putStrLn $ "Unknown event " ++ show (as_hex code)
                    putStrLn $ "<<<<<<\n" ++ xxd 16 4 bs
                e -> putStrLn $ "Received event: " ++ show e
            writeIORef var $ runGetPartial get_tib_response remaining
            return $ Just event

run_clive :: Socket -> IORef (Result TibResponse) -> IORef CliveState -> IO ()
run_clive sock var cliveref = do
    ev <- liftIO $ get_event sock var
    case ev of
        Nothing -> return ()
        Just ev -> do
            clive <- readIORef cliveref
            (_, clivenew) <- runStateT (update_clive sock ev >> decide_clive sock) clive
            writeIORef cliveref clivenew

decide_clive:: Socket -> StateT CliveState IO ()
decide_clive sock = do
    CliveState _ autoon owner target targets rs <- get

    case targets of
        npc : _ | autoon && target == Nothing -> attack npc
        _ -> return ()

    case rs of
        (0, 0, 0, 0, 0) -> return ()
        erf | Just ownerid <- owner -> do
            liftIO . putStrLn $ "Got resources! " ++ show erf
            transfer_to ownerid erf
        _ -> return ()
    where
    attack npc = do
        liftIO . send_tib sock $ ReqAttack npc
        modify $ \st -> st { c_attacking = Just npc }

    transfer_to whom rs = do
        liftIO . send_tib sock $ ReqResourceTransfer whom rs
        modify $ \st -> st { c_resources = (0, 0, 0, 0, 0) }

update_clive :: Socket -> TibResponse -> StateT CliveState IO ()
update_clive _ (ChatEvent msg sender _ _) = return ()  -- TODO: chat commands!
update_clive _ (UpdateSectorEnts _ entmap) = do
    modify $ \st -> st { c_targets = map fst $ filter (is_npc . snd) entmap }
    update_targeting
-- update_clive (SetShipResources entid res) = do
--     me <- c_id <$> get
--     if entid == me then modify $ \st -> st { c_resources = res } else return ()
update_clive _ (EntityArrival entid Npc{}) =
    modify $ \st -> st { c_targets = entid : c_targets st }
update_clive sock (EntityDepart entid death) = do
    CliveState _ autoon owner target targets rs <- get
    when (death == Destroyed && Just entid == target && Nothing /= owner) $ do
        -- transfer our resources
        let Just oid = owner
        liftIO . send_tib sock $ ReqResourceTransfer oid (99, 99, 99, 99, 99)
    modify $ \st -> st { c_targets = delete entid $ targets }
    update_targeting
update_clive _ _ = return ()

update_targeting :: StateT CliveState IO ()
update_targeting = do
    st <- get
    case c_attacking st of
        Nothing -> return ()
        Just bloop -> if bloop `elem` c_targets st then return () else do
            put $ st { c_attacking = Nothing }

eof_error e = if isEOFError e then Just () else Nothing

cmd_loop sock fifo cliveref = forever . handleJust eof_error retry $ do
    cli <- BStr.hGetLine fifo
    case parse_command cli of
        Left err -> print err
        Right (RawCmd cmd) -> do
            putStrLn $ "Sending raw command: " ++ show cmd
            send_tib sock cmd
        Right (Follow 0) -> do
            atomicModifyIORef' cliveref $
                \c -> (c { c_following = Nothing }, ())
            send_tib sock $ ReqFollow 0
        Right (Follow owner) -> do
            atomicModifyIORef' cliveref $
                \c -> (c { c_following = Just owner }, ())
            send_tib sock $ ReqFollow owner
        Right ListStatus -> do
            putStrLn "clive status: "
            readIORef cliveref >>= print
        Right (SetAuto n) -> do
            -- TODO: below is clumsy. consider stm
            c <- readIORef cliveref
            atomicModifyIORef' cliveref $ \c -> (c { c_auto_attack = n }, ())
    where retry _ = threadDelay (5 Sec) >> cmd_loop sock fifo cliveref

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

command :: Parser CliveCmd
command = join . trie_lookup "command" cmds $ lexeme not_spaces1
    where
    cmds = R.from_list
        [ ("move", RawCmd . Move <$> direction)
        , ("attack", RawCmd . ReqAttack <$> lexeme numeral)
        , ("follow", Follow <$> lexeme numeral)
        , ("transfer", RawCmd <$> (ReqResourceTransfer <$> lexeme numeral
                                                       <*> lexeme resources))
        , ("chat", RawCmd <$> chat_command)
        , ("auctionhouse", RawCmd <$> (ReqAuctions <$> lexeme numeral <*> lexeme rarity))
        , ("quit", return . RawCmd $ ReqDisconnect False)
        , ("status", return ListStatus)
        , ("auto", return $ SetAuto True)
        , ("stop", return $ SetAuto False)
        -- , ("sectormsg", CSectorChat <$> rest)
        -- , ("msg", CPrivMsg <$> rest)
        -- , ("list", CListSector)
        -- , ("kill", CAttack . fromIntegral <$> getWord8)
        -- , ("killfirst", CAttackFirst)
        ]

parse_command :: ByteString -> Either P.ParseError CliveCmd
parse_command = P.runParser (P.spaces >> command <* P.eof) () ""
