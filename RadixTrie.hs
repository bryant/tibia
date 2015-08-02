{-
chatous
chat
champion
champinator
chow
cow
coo
                        v
* -c- * -h- * -a- * -t- * -o- [us]
                    -m- * -p- * -i- * -o- [n]
                                      -n- [ator]
              -o- [w]
        -o- * -o- []
              -w- []
-}

module RadixTrie where

import qualified Data.Map as Map
import Data.Map (Map)
import Data.List (isPrefixOf)

type RadixTrie k v = Map k (Node k v)

data Node k v
    = Branch (Maybe v) (RadixTrie k v)
    | Leaf v [k]
    deriving Show

mb = maybe Nothing

trie_empty = Map.empty

from_list :: Ord k => [([k], v)] -> RadixTrie k v
from_list = foldr (\(ks, v) trie -> upsert trie ks v) trie_empty

node_lookup :: Ord k => [k] -> Node k v -> Maybe v
node_lookup [] (Branch (Just val) _) = Just val
node_lookup [] (Leaf val _) = Just val
node_lookup kks@(k:ks) (Branch _ next) = mb (node_lookup ks) $ Map.lookup k next
node_lookup kks@(k:ks) (Leaf val remaining)
    | kks `isPrefixOf` remaining = Just val
node_lookup _ _ = Nothing

node_insert [] v (Branch _ rest) = Branch (Just v) rest
node_insert [] vnew (Leaf vold []) = Leaf vnew []
node_insert [] vnew (Leaf vold (r:rs)) =  -- evict old to one level down
    Branch (Just vnew) (Map.singleton r $ Leaf vold rs)
node_insert (k:ks) v (Branch u next) = case Map.lookup k next of
    Nothing -> Branch u $ Map.insert k (Leaf v ks) next
    Just further -> Branch u $ Map.insert k (node_insert ks v further) next
node_insert kks@(_:_) vnew (Leaf vold rrs) = case lcp kks rrs of
    {-([], (k:ks), []) -> Branch (Just vold) . Map.singleton k $ Leaf vnew ks
    ([], (k:ks), (r:rs)) ->
        Branch Nothing $ Map.fromList [(k, Leaf vnew ks), (r, Leaf vold rs)]-}
    (common, [], []) -> Leaf vnew rrs  -- update
    (common, [], (r:rs)) ->
        branches common . Branch (Just vnew) . Map.singleton r $ Leaf vold rs
    (common, (k:ks), []) ->
        branches common . Branch (Just vold) . Map.singleton k $ Leaf vnew ks
    (common, (k:ks), (r:rs)) ->
        branches common . Branch Nothing $
            Map.fromList [(k, Leaf vnew ks), (r, Leaf vold rs)]

upsert :: Ord k => RadixTrie k v -> [k] -> v -> RadixTrie k v
upsert trie (k:ks) v = case Map.lookup k trie of
    Nothing -> Map.insert k (Leaf v ks) trie
    Just node -> Map.insert k (node_insert ks v node) trie

lookup :: Ord k => RadixTrie k v -> [k] -> Maybe v
lookup trie (k:ks) = mb (node_lookup ks) $ Map.lookup k trie

-- | Longest common prefix
lcp :: Eq a => [a] -> [a] -> ([a], [a], [a])
lcp xs ys = go [] xs ys
    where
    go acc (x:xs) (y:ys)
        | x == y = go (x : acc) xs ys
        | otherwise = (reverse acc, x : xs, y : ys)
    go acc xs ys = (reverse acc, xs, ys)

branches stuff tip = foldr (\c f -> Branch Nothing $ Map.singleton c f) tip stuff
