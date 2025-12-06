module Day.Day03 (run) where

import Control.Arrow ((>>>))
import Data.Char (digitToInt)
import Data.List (foldl')
import Data.List.Extra (takeEnd)
import Test.HUnit ((@=?))
import Utils (readInt)

parse :: String -> [[Int]]
parse = lines >>> map (map digitToInt)

findBiggest :: Int -> [Int] -> [Int]
findBiggest n xs = foldl' (flip addElem) start elems
 where
  start = takeEnd n xs
  elems = drop n $ reverse xs

addElem :: (Ord a) => a -> [a] -> [a]
addElem x [] = []
addElem new (x : xs)
  | new >= x = new : addElem x xs
  | otherwise = x : xs

sumBiggest :: Int -> [[Int]] -> Int
sumBiggest n = map (findBiggest n) >>> map (concatMap show >>> readInt) >>> sum

run :: String -> IO ()
run input = do
  let parsed = parse input

  let resA = sumBiggest 2 parsed
  print resA
  resA @=? 17524

  let resB = sumBiggest 12 parsed
  print resB
  resB @=? 173848577117276
