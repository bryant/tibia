{-# LANGUAGE FlexibleInstances, StandaloneDeriving, DeriveGeneric #-}

import qualified Data.Set as Set
import qualified Data.ByteString as BStr
import qualified LibTIB.Event as E
import qualified LibTIB.Request as R

import GHC.Generics (Generic)
import Control.Applicative ((<$>), (<*>), (<*))
import Network.Socket.ByteString (recv, send)
import Data.Time (getCurrentTime, diffUTCTime)
import Data.Serialize (runGetPartial, Result(..), decode, encode, Serialize)
import Data.Graph.AStar
import Data.Array
import LibTIB.Common (Node(..), Direction(..), Server(..), server_ip, is_gray)
import LibTIB.Test (md5sum)
import Util.Sock (tcp)

type GNode = (Int, Int)
type Graph = Array GNode [GNode]  -- adj list

--instance Ord GNode where

to_adj_list :: [(GNode, [GNode])] -> Graph
to_adj_list neighbors = listArray ((0, 0), (79, 79)) (repeat []) // neighbors

to_gnode :: Node -> GNode
to_gnode (NonRift x y) = (fromIntegral x, fromIntegral y)
to_gnode n@Rift{} = error $ "how did rift get through filtering? " ++ show n

to_neighbor :: Node -> Direction -> Node
to_neighbor (NonRift x y) dir = NonRift x' y'
    where (x', y') = neigh x y dir
to_neighbor (Rift x y) dir = Rift x' y'
    where (x', y') = neigh x y dir

neigh x y dir = case dir of
    Northwest -> (x - 1, y - 1)
    North -> (x, y - 1)
    Northeast -> (x + 1, y - 1)
    East -> (x + 1, y)
    Southeast -> (x + 1, y + 1)
    South -> (x, y + 1)
    Southwest -> (x - 1, y + 1)
    West -> (x - 1, y)

calc_path :: Graph -> GNode -> GNode -> Maybe [GNode]
calc_path g from to = aStar neighbors_of dist heurestic_dist (== to) from
    where
    neighbors_of :: GNode -> Set.Set GNode
    neighbors_of = Set.fromList . (g !)
    dist = manhattan
    heurestic_dist = manhattan to

deriving instance Generic Node
instance Serialize Node

manhattan :: (Int, Int) -> (Int, Int) -> Int
manhattan (x, y) (u, v) = max (abs $ x - u) (abs $ y - v)

save_cached_nodes :: [(Node, [Direction])] -> IO ()
save_cached_nodes = BStr.writeFile "./cachednodes" . encode

get_cached_nodes :: IO [(Node, [Direction])]
get_cached_nodes = either fail id . decode <$> BStr.readFile "./cachednodes"

get_gray_nodes :: IO [(Node, [Node])]
get_gray_nodes =
    filter (is_gray . fst)
    . map (\(n, ds) -> (n, filter is_gray $ map (to_neighbor n) ds))
    <$> get_cached_nodes

main = do
    gray <- to_adj_list . map (\(n, ds) -> (to_gnode n, map to_gnode ds)) <$> get_gray_nodes
    --flip mapM_ gray $ \(i, e) ->
    --    putStrLn $ "Node " ++ show i ++ " " ++ show e
    let Just t0 = calc_path gray (34, 40) (35, 45)  -- 9 jumps
        Just t1 = calc_path gray (37, 40) (45, 35)  -- 17 jumps
    print $ length t0 <= 9
    print $ length t1 <= 17

offset (x, y) = (x + 1, y + 1)

get_nodes :: IO [(Node, [Direction])]
get_nodes = do
    sock <- tcp server_ip 32040
    shouldbeiv <- recv sock 1024

    case E.decode_event shouldbeiv of
        Left wut -> fail wut
        Right (E.Challenge iv ver) -> do
            send sock . R.encode_request $ R.Auth iv md5sum ServGray
            now <- getCurrentTime
            collect_loop sock 20 now [] $ runGetPartial E.get_event
        Right unknown -> do
            fail $ "Unknown response from server: " ++ show unknown

collect_loop sock lim n col f = do
    now <- getCurrentTime
    if diffUTCTime now n > lim then return col else do
        stuff <- recv sock 1024
        let (col', f') = process f col stuff
        collect_loop sock lim n col' f'
    where
    cont = runGetPartial E.get_event
    process f col stuff = case f stuff of
        Partial f -> (col, f)
        Fail err remaining -> process cont col remaining
        Done event remaining -> case event of
            E.NodeConns n dirs -> process cont ((n, dirs) : col) remaining
            _ -> process cont col remaining
