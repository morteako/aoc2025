module Day.Day11 (run) where

import Control.Arrow ((>>>))
import Control.Lens
import Control.Monad (void)
import Data.Foldable (fold, toList)
import Data.Graph.Inductive (Graph (mkGraph), components, hasLoop, reachable, scc, topsort)
import Data.Graph.Inductive.Query
import Data.Graph.Inductive.Tree (Gr)
import Data.List
import Data.List qualified as List
import Data.List.Split (splitOn)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Tree qualified as Tree
import Map qualified
import Print
import Test.HUnit ((@=?))
import Text.RawString.QQ (r)
import Trace
import Utils

-- parse :: String -> Map.Map ([Char], Set.Set String) ([Char], Set.Set String)
parse = lines >>> map line >>> Map.fromList
 where
  line (splitOn ": " -> [k, vs]) = (k, Set.fromList $ words vs)

-- reverseMap = Map.foldMapWithKey (\k vs -> foldMap (List.singleton . fmap List.singleton . (k,)) vs) >>> Map.fromListWith (++) >>> Map.map Set.fromList

solveA :: Map.Map [Char] (Set String) -> _
solveA paths = runGo Set.empty _OUT paths "you" & length

-- go paths cur | traceShow cur False = undefined

runGo stoppers target paths start = go start
 where
  -- go :: [Char] -> [[String]]
  go cur | cur == target = [[]]
  go cur | Set.member cur stoppers = []
  go cur | Just s <- Map.lookup cur paths = do
    ps <- foldMap go s
    pure (cur : ps)
  go cur = []

runGoBasic = runGo Set.empty

-- go backwards, make multiplier?

-- goBack curs cur mult | cur == "you" = void
-- goBack curs cur mult | Set.member cur curs = void
-- goBack curs cur mult = do

hasEdges = any (not . null)

newtype Rev = Rev String deriving (Eq, Ord, Show)
type M = Map String (Set String)

topSort :: M -> M -> Set String -> [String] -> [String]
topSort graph revGraph s l | Set.null s = if hasEdges revGraph then error "NOP" else l
topSort !graph !revGraph !s !l = topSort graph' revGraph' s'' l'
 where
  -- remove all

  Just (n, s') = Set.minView s
  l' = n : l

  ms = Map.findWithDefault Set.empty n graph
  graph' = Map.delete n graph
  revGraph' = foldl' f revGraph ms
  f cur toDel = Map.adjust (Set.delete toDel) toDel cur

  s'' = s <> (ms & Set.filter (\x -> Map.findWithDefault Set.empty x revGraph' & null))

-- L ← Empty list that will contain the sorted elements
-- S ← Set of all nodes with no incoming edge

-- while S is not empty do
--     remove a node n from S
--     add n to L
--     for each node m with an edge e from n to m do
--         remove edge e from the graph
--         if m has no other incoming edges then
--             insert m into S

-- if graph has edges then
--     return error   (graph has at least one cycle)
-- else
--     return L   (a topologically sorted order)

_OUT = "out"

solveB :: Map.Map [Char] (Set String) -> _
solveB paths = do
  mprint $ paths
  print "bbbb"

  let allNodes = Set.toList $ (Map.keysSet paths <> fold paths)
  let nodesToInd = Map.fromList $ flip zip [0 ..] allNodes
  let indToNodes = Map.fromList $ zip [0 ..] allNodes

  let allEdges :: [(Int, Int, String)]
      allEdges =
        Map.foldMapWithKey (\k vs -> Set.toList vs & map (k,)) paths
          & nub
          & zipWith (\i (a, b) -> (nodesToInd Map.! a, nodesToInd Map.! b, a ++ "." ++ b)) [0 ..]

  let lnodes = Map.toList indToNodes
  -- print $ topSort paths (reverseMap paths) allNodes []

  let getNode s = nodesToInd Map.! s
  let getName s = indToNodes Map.! s

  -- mkGraph :: [LNode a] -> [LEdge b] -> gr a b
  let graph = mkGraph @Gr lnodes allEdges
  print $ topsort graph
  print $ hasLoop graph

  let isOk t = elem (getNode _FFT) t && elem (getNode _DAC) t
  -- print $ map Tree.levels $ map (fmap getName) $ dff [getNode _SRV] graph
  print $ toList $ take 10 $ filter isOk $ dff [getNode _SRV] graph
  -- print $ scc graph
  -- print $ reachable (nodesToInd Map.! "svr") graph
  -- print $ components graph
  print 1
 where
  -- let usedDacToOut = (Set.fromList $ concat dacToOut)

  -- let trimmedPaths = Map.map (\keys -> keys `Set.difference` usedDacToOut) paths

  -- print $
  --   length $
  --     -- s <- dacToOut
  --     runGo Set.empty _DAC trimmedPaths _FFT

  dacToFFT = runGoBasic _FFT paths _DAC & length -- 0, so the order must be, svr - fft - dac - out
  -- srvToDac = runGoBasic  _DAC paths _SRV & length
  dacToOut = runGoBasic _OUT paths _DAC -- fast, 6679
  fftToDac = runGoBasic _DAC paths _FFT & length
  svrToFFT = runGoBasic _FFT paths _SRV & length

-- fftToOut = runGo  _OUT paths _FFT & length

-- fftToOut = goNoLoop _STOP paths Set.empty _FFT & length

-- goNoLoop :: String -> Map.Map [Char] (Set [Char]) -> [Char] -> [[String]]
-- goNoLoop target paths _ cur | cur == target = [[]]
-- goNoLoop target paths prevs cur | Set.member cur prevs = [[]]
-- goNoLoop target paths prevs cur | Just s <- Map.lookup cur paths = do
--   ps <- foldMap (goNoLoop target paths (Set.insert cur prevs)) s
--   pure (cur : ps)
-- goNoLoop target paths _ cur = []

_SRV = "svr"
_DAC = "dac"
_FFT = "fft"

testInput =
  [r|aaa: you hhh
you: bbb ccc
bbb: ddd eee
ccc: ddd eee fff
ddd: ggg
eee: out
fff: out
ggg: out
hhh: ccc fff iii
iii: out|]
testInput2 =
  [r|svr: aaa bbb
aaa: fft
fft: ccc
bbb: tty
tty: ccc
ccc: ddd eee
ddd: hub
hub: fff
eee: dac
dac: fff
fff: ggg hhh
ggg: out
hhh: out|]

run :: String -> IO ()
run input = void $ do
  -- input <- putStrLn "#####    testInput   #####" >> pure testInput2
  -- print input
  let parsed = parse input
  mprint parsed
  print "############################"
  -- let resA = solveA parsed
  -- print resA

  -- resA @=? 1715
  let resB = solveB parsed
  resB

-- resB @=? 1739
