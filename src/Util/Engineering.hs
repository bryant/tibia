module Util.Engineering where

import LibTIB.Common (ItemType(..), Rarity(..))

type Rank = Int

path_cost :: Bool -> ItemType -> Rarity -> Rank -> Rarity -> Rank -> Double
path_cost boe ty ra0 rk0 ra1 rk1 =
        rank_cost + aug_cost * if boe then 0.5 else 1
    where
    -- note: rank before aug, as this is always cheaper.
    rank_cost = rank_factor * rarity0 * abs (fromIntegral rk1 - fromIntegral rk0)
    aug_cost = aug_factor * sum (map (** 4) [rarity0..rarity1 - 1])
    (rarity0, rarity1) = (enum2d ra0, enum2d ra1)
    (rank_factor, aug_factor) = case ty of
        TyWeapon -> (22, 3)
        TyArmor -> (18, 2.5)
        TyStorage -> (14, 2)
        ty -> error $ show ty ++ " has no rank. use ship_cost instead."

enum2d = fromIntegral . fromEnum
