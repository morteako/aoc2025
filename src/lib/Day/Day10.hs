module Day.Day10 (run) where

import Control.Arrow ((>>>))
import Control.Lens
import Control.Monad (void)
import Data.Bits (Bits (xor))
import Data.List (findIndices)
import Data.List.Split (splitOn)
import Data.Map qualified as Map
import Data.Numbers.Primes (primes)
import Data.SBV (
  EqSymbolic ((.==)),
  Modelable (getModelValue),
  OptimizeResult (LexicographicResult),
  OptimizeStyle (Lexicographic),
  OrdSymbolic ((.>=)),
  constrain,
  minimize,
  optimize,
  sInteger,
 )
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Traversable (for)
import Test.HUnit ((@=?))
import Utils (readInt)

data Machine = Machine [Bool] [[Int]] [Int] deriving (Show)

parse :: String -> [Machine]
parse = lines >>> map machine
 where
  machine (words -> map removeParens -> (indic : buttonsjoltage)) =
    Machine (map (== '#') indic) (map ints $ init buttonsjoltage) (ints $ last buttonsjoltage)
  machine (words -> failure) = error $ show failure

  ints = map readInt . splitOn ","

  removeParens = filter (flip notElem ("()[]{}" :: String))

pressButton :: [Bool] -> [Int] -> [Bool]
pressButton state button = imap (\i cur -> xor (flip elem button i) cur) state

findFewest :: Machine -> _
findFewest (Machine s b _) = iterate (pressAll (Set.fromList b)) (Set.singleton start) & findIndices (Set.member s) & head
 where
  start = map (const False) s

pressAll :: Set [Int] -> Set [Bool] -> Set [Bool]
pressAll buttons states = foldMap (\s -> Set.map (pressButton s) buttons) states

solveA :: [Machine] -> Int
solveA = map findFewest >>> sum
 where
  tweakinput = drop 1 >>> take 1

groupButtons :: Machine -> _
groupButtons (Machine _ buttons voltages) =
  (eqs, indexes)
 where
  eqs =
    iconcatMap f buttons
      & Map.fromListWith (++)
      & Map.map (map show)
      & Map.toList
      & over (traverse . _1) (voltages !!)

  indexes = imap (\i _ -> show i) buttons

  f i bs = fmap (\b -> (b, [i])) bs

findSol :: ([(Int, [String])], [String]) -> IO OptimizeResult
findSol (eqs, indexes) = optimize Lexicographic $ do
  varsList <- for indexes $ \ind -> fmap (ind,) $ do
    q <- sInteger ind
    constrain $ q .>= 0
    pure q

  let varsMap = Map.fromList varsList
      mappedEqs = over (traverse . _2 . traverse) (varsMap Map.!) eqs

  for mappedEqs $ \(target, vars) -> do
    constrain $ sum vars .== fromInteger (toInteger target)

  let sumVars = sum $ map snd varsList

  summ <- sInteger "sum"
  constrain $ summ .== sumVars

  minimize "sum" summ

solveB machines = do
  vals <- for machines $ \machine -> do
    res <- findSol (groupButtons machine)

    let smtRes = case res of
          LexicographicResult r -> r
          _ -> error ""

    let sumVal :: Integer
        Just sumVal = getModelValue "sum" smtRes
    pure sumVal

  pure $ sum vals

run :: String -> IO ()
run input = void $ do
  let parsed = parse input

  let resA = solveA parsed
  print resA
  resA @=? 375

  resB <- solveB parsed
  print resB
  resB @=? 15377
