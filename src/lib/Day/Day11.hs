module Day.Day11 (run) where

import Control.Arrow ((>>>))
import Control.Monad (void)
import Control.Monad.RWS
import Control.Monad.State
import Control.Monad.State (execState)
import Data.Foldable (fold, toList, traverse_)
import Data.Functor (void, ($>))
import Data.List qualified as List
import Data.List.Split (splitOn)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Map qualified
import Test.HUnit ((@=?))
import Utils (zipWithNext)

parse :: String -> Map String [String]
parse = lines >>> map line >>> Map.fromList
 where
  line (splitOn ": " -> [k, vs]) = (k, words vs)

solveA :: Map.Map String [String] -> _
solveA paths = countFind paths _YOU _OUT

fastGo :: String -> Map String [String] -> String -> State (Map (String, String) Int) Int
fastGo to paths from | to == from = modify (Map.insert (from, to) 1) $> 1
fastGo to paths from = do
  counts :: Map.Map (String, String) Int <- get
  case Map.lookup (from, to) counts of
    Just res -> pure res
    Nothing -> do
      ans <- sum <$> traverse (fastGo to paths) (Map.findWithDefault [] from paths)
      modify (Map.insert (from, to) ans)
      pure ans

countFind :: Map String [String] -> String -> String -> Int
countFind paths from to = Map.find (from, to) $ runFastGo to paths from
 where
  runFastGo :: String -> Map String [String] -> String -> Map (String, String) Int
  runFastGo paths from cur = execState (fastGo paths from cur) Map.empty

solveB :: Map.Map String [String] -> Int
solveB paths = do
  let getCombs = product . zipWithNext (countFind paths)
  getCombs [_SRV, _FFT, _DAC, _OUT] + getCombs [_SRV, _DAC, _FFT, _OUT]

_SRV = "svr"
_DAC = "dac"
_FFT = "fft"
_OUT = "out"
_YOU = "you"

run :: String -> IO ()
run input = void $ do
  let parsed = parse input

  let resA = solveA parsed
  print resA

  resA @=? 448
  let resB = solveB parsed
  print resB
  resB @=? 553204221431080
