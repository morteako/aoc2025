module Day.Day07 (run) where

import Control.Arrow (Arrow (..), (>>>))

import Control.Lens (FoldableWithIndex (ifoldMap), ifoldMapByOf, ifolded, (&))
import Data.Foldable (Foldable (fold))
import Data.Functor ((<&>))
import Data.List (find)

import Data.Map (Map)
import Data.Map qualified as OldMap
import Data.Map.Monoidal (MonoidalMap)
import Data.Map.Monoidal qualified as MMap
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

type Grid = MonoidalMap (V2 Int) (Tile)

type Res = (Sum Int, Set (V2 Int))

move :: Grid -> V2 Int -> Res -> MonoidalMap (V2 Int) (Res)
move grid pos (count, prevSplitPos) = do
  let n = V2 0 1 + pos
  case MMap.lookup n grid of
    Nothing -> mempty
    Just Empty -> MMap.singleton n (count, prevSplitPos)
    Just Split -> splitBeam n <&> (\newPos -> (newPos, (count, Set.insert n prevSplitPos))) & MMap.fromList
     where
      splitBeam v = [V2 1 0 + v, V2 (-1) 0 + v] & filter (flip MMap.member grid)

solve :: Map (V2 Int) (Maybe Tile) -> _
solve (getStartAndCreateGrid -> (start, grid)) = iterate next seed & takeWhile (not . null) & last & fold & (id *** Set.size) & swap
 where
  seed = (MMap.singleton start (1, mempty))
  next = ifoldMap (move grid)

getStartAndCreateGrid :: Map (V2 Int) (Maybe Tile) -> (V2 Int, Grid)
getStartAndCreateGrid grid = (OldMap.toList grid & find (isNothing . snd) & fromJust & fst, OldMap.mapMaybe id grid & OldMap.toList & MMap.fromList)

run :: String -> IO ()
run input = do
  let parsed = parse input

  let (resA, resB) = solve parsed

  print resA
  resA @=? 1687

  print resB
  resB @=? 390684413472684
