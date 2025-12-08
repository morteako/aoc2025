module Day.Day08 (run) where

import Control.Arrow ((>>>))
import Control.Lens
import Control.Monad (guard)
import Data.List
import Data.List.Split (splitOn)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Ord (Down (Down))
import Data.Set qualified as Set
import Linear
import Map (fromListIndexed)
import Test.HUnit ((@=?))
import Utils (readInt)

parse :: String -> [V3 Integer]
parse = lines >>> map (splitOn "," >>> map (readInt >>> toInteger) >>> toV3)
 where
  toV3 [x, y, z] = V3 x y z

allCombs poses = do
  p1 <- poses
  p2 <- poses
  guard $ p1 < p2
  pure $ (p1, p2)

dist (V3 a b c) (V3 aa bb cc) = sqrt $ fromIntegral ((a - aa) ^ 2 + (b - bb) ^ 2 + (c - cc) ^ 2)

solveA :: Int -> [V3 Integer] -> Int
solveA n poses =
  shortestConnections
    & foldl' addPair initMap
    & Map.elems
    & map Set.size
    & sortOn Down
    & take 3
    & product
 where
  shortestConnections =
    allCombs poses
      & sortOn (uncurry dist)
      & take n

  initMap = Map.fromListIndexed poses & fmap Set.singleton

solveB :: [V3 Integer] -> Integer
solveB poses = productOf (both . _x) $ shortestConnections !! (ind - 1)
 where
  Just ind =
    shortestConnections
      & scanl' addPair initMap
      & findIndex ((== 1) . Map.size)

  shortestConnections =
    allCombs poses
      & sortOn (uncurry dist)

  initMap = Map.fromListIndexed poses & fmap Set.singleton

addPair :: (Show a, Ord a) => Map Int (Set.Set a) -> (a, a) -> Map Int (Set.Set a)
addPair curs (x, y) =
  case (findSet x, findSet y) of
    (Nothing, Nothing) ->
      undefined
    (Just (xi, _), Nothing) ->
      Map.adjust (Set.insert y) xi curs
    (Nothing, Just (yi, _)) ->
      Map.adjust (Set.insert x) yi curs
    (Just (xi, _), Just (yi, yset)) ->
      if
        | xi == yi -> curs
        | otherwise -> Map.delete yi $ Map.adjust (<> yset) xi curs
 where
  findSet t = ifind (\_ x -> Set.member t x) curs

run :: String -> IO ()
run input = do
  let n = 1000
  let parsed = parse input

  let resA = solveA n parsed
  print resA
  resA @=? 115885

  let resB = solveB parsed
  print resB
  resB @=? 274150525
