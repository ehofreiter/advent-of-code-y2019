module AOC2019.Day9 where

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
import qualified AOC2019.Intcode9 as IC

dataPath = "data/day9/"
runF = dataPath ++ "input.txt"
--testF = dataPath ++ "test.txt"

runI = load runF
--testI = load testF

run = runI >>= exec
--test = testI >>= exec

quine :: IC.Program
quine = [109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99]

load :: FilePath -> IO IC.Program
load filePath = do
  strs <- loadFile filePath
  pure (map read $ splitOn "," $ head strs)

exec :: IC.Program -> IO ()
exec inputs = do
  --print inputs
  print (part1 inputs)
  print (part2 inputs)

part1 :: IC.Program -> Integer
part1 intcode =
  let (_, ps) = IC.runProgram intcode [1]
      outputs = IC.psOutputs ps
  in  head outputs

part2 :: IC.Program -> Integer
part2 intcode =
  let (_, ps) = IC.runProgram intcode [2]
      outputs = IC.psOutputs ps
  in  head outputs
