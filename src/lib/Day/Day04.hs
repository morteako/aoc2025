module Day.Day04 (run) where

import Control.Arrow ((>>>))
import Control.Lens ((&))
import Data.Map qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Linear.V2
import Test.HUnit ((@=?))
import Utils (parseAsciiMap)

parse :: String -> Set (V2 Int)
parse = parseAsciiMap f >>> Map.keysSet
 where
  f '@' = Just ()
  f _ = Nothing

neighbors :: (Num a) => V2 a -> [V2 a]
neighbors (V2 x y) = tail $ [V2 x y | x <- [x, x + 1, x - 1], y <- [y, y + 1, y - 1]]

canBeAccessed :: Set (V2 Int) -> V2 Int -> Bool
canBeAccessed grid k = (< 4) $ length $ filter (flip Set.member grid) $ neighbors k

solveA :: Set (V2 Int) -> Int
solveA grid = Set.filter (canBeAccessed grid) grid & Set.size

solveB :: Set (V2 Int) -> Int
solveB grid = Set.size grid - Set.size (fixpoint removeNotAccessible grid)
 where
  removeNotAccessible grid = Set.filter (not . canBeAccessed grid) grid

fixpoint :: (Eq t) => (t -> t) -> t -> t
fixpoint f grid =
  let next = f grid
   in if grid == next then grid else fixpoint f next

run :: String -> IO ()
run input = do
  let parsed = parse input
  let resA = solveA parsed
  print resA
  resA @=? 1351

  let resB = solveB parsed
  print resB
  resB @=? 8345
