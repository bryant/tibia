{-# LANGUAGE OverloadedStrings, FlexibleInstances #-}

module Clive where

import qualified Data.ByteString as BStr
import qualified Data.ByteString.Char8 as Char8
import qualified RadixTrie as Radix
import qualified Text.Parsec as P
import qualified Data.IntMap as IMap

import Data.ByteString (ByteString)
import Network.Socket (withSocketsDo, Socket)
import Data.Serialize (runGet, runGetPartial, Result(..))
import Data.Char (isSpace)
import Data.Word (Word32)
import Data.List (delete, foldl')
import Text.Parsec.ByteString (Parser)
import Network.Socket.ByteString (recv, send)
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

import LibTIB.Entity (is_npc, Entity(Npc, Ship), PlayerID)
import LibTIB.Common
import qualified LibTIB.Request as R
import qualified LibTIB.Event as E
import Util.Sock (sconnect, tor)
import Util.XXD (xxd, as_hex)

data CliveState
    = CliveState
    { c_auto_attack :: Bool
    , c_following :: Maybe EntID
    , c_attacking :: Maybe EntID
    , c_targets :: [EntID]
    , c_resources :: Resources
    , c_location :: E.Node
    , c_player_ids :: Radix.RadixTrie Char PlayerID
    , c_player_ents :: IMap.IntMap EntID  -- ^ playerid -> entid
    }
    deriving Show

data CliveCmd
    = RawCmd R.TibRequest
    | ListStatus
    | SetAuto Bool
    | Follow (Maybe EntID)
    | LookUpPlayer String

data ToUSec = Sec | Ms | Us

instance Num a => Num (ToUSec -> a) where
    fromInteger n unit = fromInteger $ n * scale
        where scale = case unit of { Us -> 1; Ms -> 1000; Sec -> 1000000; }

uninitialized_clive = CliveState
    { c_auto_attack = False
    , c_following = Nothing
    , c_attacking = Nothing
    , c_targets = []
    , c_resources = (0, 0, 0, 0, 0)
    , c_location = E.NonRift 0 0
    , c_player_ids = Radix.trie_empty
    , c_player_ents = IMap.empty
    }

get_ip :: IO ByteString
get_ip = do
    sock <- sconnect tor "whatismyip.org" 80
    send sock gitit
    collect sock ""
    where
    gitit = "GET / HTTP/1.1\r\n\
            \User-Agent: curl/7.35.0\r\n\
            \Host: localhost:8000\r\n\
            \Accept: */*\r\n\r\n"
    collect s bs = do
        more <- recv s 65536
        if more == "" then return bs else collect s $ BStr.append bs more

send_tib :: Socket -> R.TibRequest -> IO ()
send_tib sock req = void $ do
    putStrLn $ "Sending " ++ show req
    putStrLn $ ">>>>>>\n" ++ xxd 16 4 payload
    send sock payload
    where payload = R.encode_request req

register_account :: Socket -> ByteString -> Account -> IO (Maybe ByteString)
register_account sock iv acc = do
    putStrLn $ "Registering account: " ++ show acc
    send_tib sock $ R.NewAcc iv acc ServGray
    reply <- recv sock 1024
    case E.decode_event reply of
        Right (E.Notice E.NoteCreateSuccess) -> do
            putStrLn "Success!"
            return Nothing
        Right n@E.Notice{} -> do
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
    BStr.putStrLn =<< get_ip
    sock <- sconnect tor server_ip 32040
    shouldbeiv <- recv sock 1024

    case E.decode_event shouldbeiv of
        Left wut -> error wut
        Right (E.Challenge iv ver) -> do
            putStrLn $ "Server version: " ++ show ver
            putStrLn $ "IV received: " ++ show iv

            acc <- generate_account
            err <- register_account sock iv acc
            case err of
                Just bytes -> putStrLn (xxd 16 4 bytes) >> mzero
                Nothing -> do
                    putStrLn "Logging in."
                    send_tib sock $ R.Auth iv acc ServGray

                    var <- newIORef . Partial $ runGetPartial E.get_event
                    clive <- newIORef uninitialized_clive
                    h <- create_fifo acc
                    forkIO $ ping_thread sock
                    forkIO $ cmd_loop sock h clive
                    forever $ run_clive sock var clive

        Right unknown-> do
            putStrLn $ "Unknown response from server: " ++ show unknown

ping_thread :: Socket -> IO ()
ping_thread sock = do
    send_tib sock R.Ping
    threadDelay $ 15 Sec
    -- TODO: do something about the inevitable server response
    ping_thread sock

recv_event :: Socket -> IORef (Result E.TibEvent) -> IO (Maybe E.TibEvent)
recv_event sock var = do
    result <- readIORef var
    case result of
        Partial f -> do
            bytes <- recv sock 1024
            writeIORef var $ f bytes
            return Nothing
        Fail err remaining -> do
            -- putStrLn $ "parse error on " ++ xxd 16 4 bytes
            putStrLn $ "error was: " ++ err
            writeIORef var $ runGetPartial E.get_event remaining
            return Nothing
        Done event remaining -> do
            case event of
                E.Unknown code bs -> do
                    putStrLn $ "Unknown event " ++ show (as_hex code)
                    putStrLn $ "<<<<<<\n" ++ xxd 16 4 bs
                e -> putStrLn $ "Received event: " ++ show e
            writeIORef var $ runGetPartial E.get_event remaining
            return $ Just event

run_clive :: Socket -> IORef (Result E.TibEvent) -> IORef CliveState -> IO ()
run_clive sock var cliveref = do
    ev <- liftIO $ recv_event sock var
    case ev of
        Nothing -> return ()
        Just ev -> do
            clive <- readIORef cliveref
            (_, clivenew) <- runStateT (update_clive sock ev >> decide_clive sock) clive
            writeIORef cliveref clivenew

decide_clive:: Socket -> StateT CliveState IO ()
decide_clive sock = do
    CliveState autoon owner target targets rs _ _ _ <- get

    case targets of
        npc : _ | autoon && target == Nothing -> attack $ Just npc
        _ -> return ()

    case rs of
        (0, 0, 0, 0, 0) -> return ()
        erf | Just ownerid <- owner -> do
            liftIO . putStrLn $ "Got resources! " ++ show erf
            transfer_to ownerid erf
        _ -> return ()
    where
    attack npc = do
        liftIO . send_tib sock $ R.Attack npc
        modify $ \st -> st { c_attacking = npc }

    transfer_to whom rs = do
        liftIO . send_tib sock $ R.TransferRes whom rs
        modify $ \st -> st { c_resources = (0, 0, 0, 0, 0) }

update_clive :: Socket -> E.TibEvent -> StateT CliveState IO ()
update_clive _ (E.Chat msg sender _ _) = return ()  -- TODO: chat commands!
update_clive _ (E.SectorEnts loc entmap) = do
    modify $ \st -> st {
            c_targets = map fst $ filter (is_npc . snd) entmap,
            c_location = loc,
            c_player_ents = foldl' collect_players IMap.empty entmap
        }
    update_targeting
    where
    collect_players im (entid, Ship _ pid _) =
        IMap.insert (fromIntegral pid) entid im
    collect_players im _ = im
-- update_clive (SetShipResources entid res) = do
--     me <- c_id <$> get
--     if entid == me then modify $ \st -> st { c_resources = res } else return ()
update_clive _ (E.Arrived entid Npc{}) =
    modify $ \st -> st { c_targets = entid : c_targets st }
update_clive _ (E.Arrived entid (Ship _ pid _)) =
    modify $ \st -> st {
        c_player_ents = IMap.insert (fromIntegral pid) entid $ c_player_ents st
        }
update_clive sock (E.Departed entid death) = do
    CliveState autoon owner target targets rs _ _ _ <- get
    when (death == Destroyed && Just entid == target && Nothing /= owner) $ do
        -- transfer our resources
        let Just oid = owner
        liftIO . send_tib sock $ R.TransferRes oid (99, 99, 99, 99, 99)
    modify $ \st -> st { c_targets = delete entid $ targets }
    update_targeting
update_clive _ (E.LoggedIn username pid) = modify $ \st -> st {
        c_player_ids = Radix.upsert (c_player_ids st) username pid
    }
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
        Right (Follow owner) -> do
            atomicModifyIORef' cliveref $
                \c -> (c { c_following = owner }, ())
            send_tib sock $ R.Follow owner
        Right ListStatus -> do
            putStrLn "clive status: "
            readIORef cliveref >>= print
        Right (SetAuto n) -> do
            -- TODO: below is clumsy. consider stm
            c <- readIORef cliveref
            atomicModifyIORef' cliveref $ \c -> (c { c_auto_attack = n }, ())
        Right (LookUpPlayer name) -> do
            cstate <- readIORef cliveref
            let pids = c_player_ids cstate
                pents = c_player_ents cstate
            case Radix.lookup pids name of
                Nothing -> putStrLn $ name ++ " not found"
                Just pid | Just entid <- IMap.lookup (fromIntegral pid) pents ->
                    putStrLn $ name ++ " has pid " ++ show pid ++ " entid " ++
                               show entid
                Just pid -> putStrLn $ name ++ " has pid " ++ show pid
    where retry _ = threadDelay (5 Sec) >> cmd_loop sock fifo cliveref

not_spaces1 = P.many1 . P.satisfy $ not . isSpace

lexeme :: Parser a -> Parser a
lexeme act = act <* P.spaces

trie_lookup lab trie inp = do
    entry <- inp
    case Radix.lookup trie entry of
         Nothing -> P.parserFail $ "not found in trie: " ++ lab ++ ", "
                                   ++ show entry
         Just blah -> return blah

direction = trie_lookup "direction" dirs $ lexeme not_spaces1
    where
    dirs = Radix.from_list
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

numerbool = try_choice [numbool, strfalse, strtrue]
    where
    numbool = numeral >>= \n -> return $ if n <= 0 then False else True
    strfalse = try_choice [P.string "false", P.string "False"] >> return False
    strtrue = try_choice [P.string "true", P.string "True"] >> return True

try_choice = P.choice . map P.try

entid = EntID <$> numeral

mb_entid = do
    n <- numeral
    return $ if n == 0 then Nothing else Just $ EntID n

resources = (,,,,) <$> num <*> num <*> num <*> num <*> num
    where num = lexeme numeral

rarity = do
    n <- numeral
    if 1 <= n && n <= 7 then return $ toEnum n else mzero

chat_command = do
    -- only sector chat allowed for now
    msg <- P.many1 P.anyChar
    return $ R.Chat msg "" "" Sector

command :: Parser CliveCmd
command = join . trie_lookup "command" cmds $ lexeme not_spaces1
    where
    cmds = Radix.from_list
        [ ("move", RawCmd . R.Move <$> direction)
        , ("attack", RawCmd . R.Attack <$> lexeme mb_entid)
        , ("follow", Follow <$> lexeme mb_entid)
        , ("transfer", RawCmd <$> (R.TransferRes <$> lexeme entid <*> lexeme resources))
        , ("chat", RawCmd <$> chat_command)
        , ("auctionhouse", RawCmd <$> (R.ListAuctions <$> (toEnum . fromIntegral <$> lexeme numeral) <*> lexeme rarity))
        , ("quit", return . RawCmd $ R.Disconnect False)
        , ("status", return ListStatus)
        , ("auto", return $ SetAuto True)
        , ("stop", return $ SetAuto False)
        , ("trade", RawCmd <$> (R.SendTrade <$> lexeme numeral
                                            <*> lexeme numeral
                                            <*> lexeme numeral
                                            <*> return Nothing
                                            <*> lexeme numeral
                                            <*> lexeme numeral
                                            <*> lexeme numeral
                                            <*> return Nothing
                                            <*> lexeme numerbool))
        , ("whois", LookUpPlayer <$> lexeme (P.many1 P.anyChar))
        -- , ("sectormsg", CSectorChat <$> rest)
        -- , ("msg", CPrivMsg <$> rest)
        -- , ("list", CListSector)
        -- , ("kill", CAttack . fromIntegral <$> getWord8)
        -- , ("killfirst", CAttackFirst)
        ]

parse_command :: ByteString -> Either P.ParseError CliveCmd
parse_command = P.runParser (P.spaces >> command <* P.eof) () ""
