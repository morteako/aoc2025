module IS where

import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.IntSet (IntSet)

newtype IS = IS (IntMap Int) deriving (Show)

data IsInterval = IsInterval Int Int deriving (Show, Eq, Ord)

-- member :: Int -> IS -> Bool
-- member i (IS im) =
--     case IntMap.lookupGE i im of
--         Just (ge, _) -> ge <= i
--         Nothing -> False

member :: Int -> IntMap Int -> Bool
member i im =
  case IntMap.lookupGE i im of
    Just (ge, _) -> ge <= i
    Nothing -> False

-- insert :: IsInterval -> IntMap Int -> IntMap Int
-- insert (IsInterval a b) im =
--     case la of
--         Nothing ->
--             case lb of
--                 -- empty map
--                 Nothing -> IntMap.insert a a $ IntMap.insert a b im
--                 --
--                 Just (bk, bv) -> error $ "DOESNT make sense. invariant doesnt hold" ++ show (a, b, im)
--         Just (ak, ab)  ->
--             case lb of
--                 Nothing -> _
--                 Just (bk, bv) -> _
--   where

--     la = IntMap.lookupGE a im
-- lb = IntMap.lookupGE b im

--   1-2, 4-5
--   1:2 2:2, 4:5 5:5
--   3? => 4 NONO
--
--
--
--  cur: 3-5, add 1-2
--
--
--
--
----
