module AOC2019.Day6 where

import Control.Applicative
import Control.Monad
import Data.Bifunctor
import Data.Bits
import Data.Char
import Data.Foldable
import Data.Functor.WithIndex
import Data.List
import Data.List.Split
import Data.Maybe
import Data.Traversable
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Sequence as Seq
import Data.Sequence (Seq(..), (><))
import qualified Data.Vector as V
import qualified Text.Parsec as P

import AOC2019.Common
import qualified AOC2019.Intcode as IC

dataPath = "data/day6/"
runF = dataPath ++ "input.txt"
testF = dataPath ++ "test.txt"
test2F = dataPath ++ "test2.txt"

runI = load runF
testI = load testF
test2I = load test2F

run = runI >>= exec
test = testI >>= exec
test2 = test2I >>= exec

load :: FilePath -> IO OrbitMap
load filePath = do
  strs <- loadFile filePath
  pure (readOrbitMap strs)

exec :: OrbitMap -> IO ()
exec inputs = do
  --print inputs
  print (part1 inputs)
  print (part2 inputs)

part1 :: OrbitMap -> Int
part1 om = sum $ map (orbitCount om) $ Map.keys om

part2 :: OrbitMap -> Int
part2 om =
  let ocYou = orbitChain om "YOU"
      ocSan = orbitChain om "SAN"
      common = ocYou `intersect` ocSan
      maxCom = maximumBy (\x y -> compare (orbitCount om x) (orbitCount om y)) common
      comDepth = orbitCount om maxCom
      youDepth = orbitCount om "YOU"
      sanDepth = orbitCount om "SAN"
  in  youDepth - comDepth - 1 + sanDepth - comDepth - 1

orbitChain :: OrbitMap -> String -> [String]
orbitChain om o = case Map.lookup o om of
  Nothing -> []
  Just o' -> o' : orbitChain om o'

orbitCount :: OrbitMap -> String -> Int
orbitCount om o = case Map.lookup o om of
  Nothing -> 0
  Just o' -> 1 + orbitCount om o'

type OrbitMap = Map.Map String String

readOrbitMap :: [String] -> OrbitMap
readOrbitMap ss = Map.fromList $ map readOrbit ss

readOrbit :: String -> (String, String) -- (x, y) => x orbits y, y)x
readOrbit s =
  let [y, x] = splitOn ")" s
  in  (x, y)
