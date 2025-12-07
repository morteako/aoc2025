module Day.Day07 (run) where

import Control.Arrow (Arrow (..), (>>>))

import Control.Lens ((&))
import Data.Foldable (Foldable (fold))
import Data.Functor ((<&>))
import Data.List (find)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (fromJust, isNothing)
import Data.Monoid (Sum)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Tuple (swap)
import Linear (V2 (V2))
import Test.HUnit ((@=?))
import Utils (parseAsciiMap)

data Tile = Split | Empty deriving (Show)

parse :: String -> Map (V2 Int) (Maybe Tile)
parse = parseAsciiMap f
 where
  f '^' = Just $ Just Split
  f '.' = Just $ Just Empty
  f 'S' = Just $ Nothing
  f _ = Nothing

type Grid = Map (V2 Int) (Tile)

moveAll :: Grid -> Map (V2 Int) Res -> Map (V2 Int) _
moveAll grid beams = Map.foldMapWithKey (move grid) beams & Map.fromListWith (<>)

type Res = (Sum Int, Set (V2 Int))

move :: Grid -> V2 Int -> Res -> [(V2 Int, Res)]
move grid pos (count, prevSplitPos) = do
  let n = V2 0 1 + pos
  case Map.lookup n grid of
    Nothing -> []
    Just Empty -> [(n, (count, prevSplitPos))]
    Just Split -> splitBeam n <&> (\newPos -> (newPos, (count, Set.insert n prevSplitPos)))
     where
      splitBeam v = [V2 1 0 + v, V2 (-1) 0 + v] & filter (flip Map.member grid)

solve :: Map (V2 Int) (Maybe Tile) -> _
solve (getStartAndCleanup -> (start, grid)) = iterate (moveAll grid) (Map.singleton start (1, mempty)) & takeWhile (not . null) & last & fold & (id *** Set.size) & swap

getStartAndCleanup :: Map k (Maybe b) -> (k, Map k b)
getStartAndCleanup grid = (Map.toList grid & find (isNothing . snd) & fromJust & fst, Map.mapMaybe id grid)

run :: String -> IO ()
run input = do
  let parsed = parse input

  let (resA, resB) = solve parsed

  print resA
  resA @=? 1687

  print resB
  resB @=? 390684413472684
