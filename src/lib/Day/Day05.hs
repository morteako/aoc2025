module Day.Day05 (run) where

import Control.Arrow (Arrow (..), (>>>))
import Control.Lens ((&))
import Data.Interval (Extended (Finite), width, (<=..<=))
import Data.IntervalSet qualified as IntSet
import Data.List.Split (splitOn)
import Test.HUnit ((@=?))
import Utils (countP, readInt, toTuple)

parse :: [Char] -> (IntSet.IntervalSet Int, [Int])
parse = splitOn "\n\n" >>> map lines >>> toTuple >>> (intervals *** nums)
 where
  nums = map readInt

  range (splitOn "-" -> [a, b]) = (readInt a, readInt b)
  intervals = IntSet.fromList . map (makeInterval . range)
  makeInterval (a, b) = Finite a <=..<= Finite b

solveA :: (IntSet.IntervalSet Int, [Int]) -> _
solveA (intervals, nums) = countP (flip IntSet.member intervals) nums

solveB :: (IntSet.IntervalSet Int, [Int]) -> Int
solveB (intervals, _) = IntSet.toList intervals & map wholeWidth & sum
 where
  wholeWidth = (+ 1) . width

run :: String -> IO ()
run input = do
  let parsed = parse input

  let resA = solveA parsed
  print resA
  resA @=? 563

  let resB = solveB parsed
  print resB
  resB @=? 338693411431456
