module Day.Day01 (run) where

import Control.Arrow ((>>>))
import Test.HUnit ((@=?))
import Utils (count)

data Rotation = L | R deriving (Read, Show)

parse :: String -> [(Rotation, Int)]
parse = lines >>> map f
 where
  f (dir : n) = (read @Rotation [dir], read @Int n)

countZeros :: [Int] -> Int
countZeros = count 0 . map (flip mod 100) . scanl (+) start
 where
  start = 50

countExactZeros :: [(Rotation, Int)] -> Int
countExactZeros = countZeros . map f
 where
  f (L, n) = -n
  f (R, n) = n

countAllZeros :: [(Rotation, Int)] -> Int
countAllZeros = countZeros . concatMap f
 where
  start = 50
  f (L, n) = replicate n (-1)
  f (R, n) = replicate n 1

run :: String -> IO ()
run input = do
  let parsed = parse input

  let resA = countExactZeros parsed
  print resA
  resA @=? 1147

  let resB = countAllZeros parsed
  print resB
  resB @=? 6789
