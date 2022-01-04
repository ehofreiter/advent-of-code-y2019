module AOC2019.Day5 where

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

dataPath = "data/day5/"
runF = dataPath ++ "input.txt"

runI = load runF
testI = pure [1002,4,3,4,33]

run = runI >>= exec
test = testI >>= exec

load :: FilePath -> IO [Int]
load filePath = do
  strs <- loadFile filePath
  pure (map read $ splitOn "," $ head strs)

exec :: [Int] -> IO ()
exec inputs = do
  print (part1 inputs)
  print (part2 inputs)

part1 :: [Int] -> Int
part1 intcode =
  let (ending, ps) = IC.runProgram intcode [1]
  in  case (ending, IC.psOutputs ps) of
        (Left IC.InterruptHalt, code:outs)
          | all (== 0) outs -> code
          | otherwise -> error "invalid outputs"
        _ -> error "bad program"

-- First attempt was 12528776, which was incorrect (too high). This was because
-- I had the LessThan instruction doing less-than-or-equal-to. Took me a long
-- time to figure out. I ended up writing a "debugger" (see 'debugSteps') that
-- finally allowed me to see the issue.
part2 :: [Int] -> Int
part2 intcode =
  let (ending, ps) = IC.runProgram intcode [5]
  in  case (ending, IC.psOutputs ps) of
        (Left IC.InterruptHalt, [code]) -> code
        (Left IC.InterruptHalt, outs) -> error ("bad outputs " <> show outs)
        _ -> error "bad program"
