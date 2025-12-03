module Day.Day02 (run) where

import Control.Arrow ((>>>))
import Data.List (inits, stripPrefix, tails)
import Data.List.Extra (splitOn)
import Test.HUnit ((@=?))
import Utils (readInt)

parse :: String -> [(Int, Int)]
parse = splitOn "," >>> fmap (splitOn "-") >>> fmap f
 where
  f [a, b] = (readInt a, readInt b)

hasDuplicate :: (Show p) => p -> Bool
hasDuplicate x = let (front, back) = getRev (show x) in front == back
 where
  getRev xs = splitAt (length xs `div` 2) xs

countInvalidIds :: (Int -> Bool) -> [(Int, Int)] -> Int
countInvalidIds invalidCheck = concatMap flatten >>> filter invalidCheck >>> sum
 where
  flatten (a, b) = [a .. b]

repPrefix :: (Eq a) => ([a], [a]) -> Bool
repPrefix (startPart, rest) = inner startPart rest
 where
  inner rep [] = True
  inner rep end | Just rest <- stripPrefix rep end = inner rep rest
  inner _ _ = False

hasAnyRepeat :: (Show a, Eq a) => [a] -> Bool
hasAnyRepeat xs = any repPrefix (allPairs xs)

allPairs :: [a] -> [([a], [a])]
allPairs xs = tail $ init $ zip (inits xs) (tails xs)

run :: String -> IO ()
run input = do
  let parsed = parse input

  let resA = countInvalidIds hasDuplicate parsed
  print resA
  resA @=? 18700015741

  let resB = countInvalidIds (show >>> hasAnyRepeat) parsed
  print resB
  resB @=? 20077272987
