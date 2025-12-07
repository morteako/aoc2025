module Day.Day06 (run) where

import Control.Arrow ((>>>))
import Data.List (transpose)
import Data.List.Extra (sumOn')
import Data.List.Split (splitWhen)
import Test.HUnit ((@=?))
import Utils (readInt)

apply :: [Int] -> String -> Int
apply nums op = case op of
  "+" -> foldr (+) 0 nums
  "*" -> foldr (*) 1 nums

grandTotal :: [([Int], String)] -> Int
grandTotal = sumOn' (uncurry apply)

parseA :: String -> [([Int], String)]
parseA = lines >>> map words >>> transpose >>> map f
 where
  f xs = (map readInt $ init xs, last xs)

parseB :: String -> [([Int], String)]
parseB =
  lines
    >>> transpose
    >>> splitWhen (all (== ' '))
    >>> map (map (filter (/= ' ')) >>> getOp)
 where
  getOp (rowWithOp : rest) = (fmap readInt (init rowWithOp : rest), [last rowWithOp])

run :: String -> IO ()
run input = do
  let resA = grandTotal (parseA input)

  print resA
  resA @=? 6725216329103

  let resB = grandTotal (parseB input)
  print resB
  resB @=? 10600728112865
