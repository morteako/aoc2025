module IS where

import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.IntSet (IntSet)
import Data.Map (Map)

newtype IS = IS (IntMap IKey) deriving (Show)

data IKey = Start Int Int | End Int Int deriving (Show)

-- data Split = None | Covering

data IsInterval = IsInterval Int Int deriving (Show, Eq, Ord)

-- member :: Int -> IS -> Bool
-- member i (IS im) =
--     case IntMap.lookupGE i im of
--         Just (ge, _) -> ge <= i
--         Nothing -> False

-- lookupUpper point im =
--   case IntMap.lookupGE point im of
--     Nothing -> Missing
--     Just (gk, gv)
--       | gk == gv -> End gk
--       | gk == gv -> End gk
--       | gk < gv -> Start gk gv

-- lookupLower point im =
--   case IntMap.lookupLE point im of
--     Nothing -> Missing
--     Just (gk, gv)
--       | gk == gv -> End gk
--       | gk < gv -> Start gk gv

member :: Int -> IntMap IKey -> Bool
member i im =
  case IntMap.lookupGE i im of
    Just (ge, _) -> ge <= i
    Nothing -> False

-- Invariant : interval a-b
-- lookupGE a
--    Nothing -> since b >= a, interval is fully not in map
--    Just (ka,va)
--        | ka == va ->

--
--
-- --

-- (lowerThan a, geThan a)
lowerSplit :: IsInterval -> IntMap IKey -> ((IntMap IKey, IntMap IKey), IsInterval)
lowerSplit int@(IsInterval a b) im =
  case IntMap.lookupGE a im of
    Nothing -> ((im, IntMap.empty), int)
    Just (_, m) -> case m of
      Start sa sb
        | a == sa ->
            let
              withoutCurInterval = IntMap.delete sa $ IntMap.delete sb im
             in
              (IntMap.split a withoutCurInterval, IsInterval a (max sb b))
      Start sa sb
        | a < sa -> -- before , ok to split
            (IntMap.split a im, int)
      Start sa sb | a > sa -> error $ "invariant bad" ++ show (a, m, im)
      End sa sb ->
        -- in the middle!
        let
          withoutCurInterval = IntMap.delete sa $ IntMap.delete sb im
         in
          (IntMap.split a withoutCurInterval, IsInterval (min sa a) (max b sb))

-- (lowerThan b, geThan b)
upperSplit :: IsInterval -> IntMap IKey -> ((IntMap IKey, IntMap IKey), IsInterval)
upperSplit int@(IsInterval a b) im =
  case IntMap.lookupLE b im of
    Nothing -> ((IntMap.empty, im), int)
    Just (_, m) -> case m of
      End sa sb
        | b > sb -> -- after, ok to split
            (IntMap.split b im, int)
      End sa sb
        | b == sb ->
            let
              withoutCurInterval = IntMap.delete sa $ IntMap.delete sb im
             in
              (IntMap.split b withoutCurInterval, IsInterval (min sa a) b)
      Start sa sb ->
        -- in the middle
        let
          withoutCurInterval = IntMap.delete sa $ IntMap.delete sb im
         in
          (IntMap.split b withoutCurInterval, IsInterval (min sa a) (max b sb))

insert :: IsInterval -> IntMap IKey -> IntMap IKey
insert int@(IsInterval a b) im = inserted
 where
  (_, IsInterval la lb) = lowerSplit int im
  (_, IsInterval ua ub) = lowerSplit int im
  IsInterval na nb = IsInterval (min la ua) (max lb ub)

  (lowSplit, rest) = IntMap.split na im
  (_, upSplit) = IntMap.split nb rest

  newMap = lowSplit <> upSplit

  inserted = IntMap.insert na (Start na nb) $ IntMap.insert nb (End na nb) newMap

--     insertFullInterval = IntMap.insert a a $ IntMap.insert a b im

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
