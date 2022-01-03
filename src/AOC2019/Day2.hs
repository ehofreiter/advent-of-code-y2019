module AOC2019.Day2 where

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

dataPath = "data/day2/"
runF = dataPath ++ "input.txt"
testF = dataPath ++ "test.txt"

runI = load runF
testI = load testF

run = runI >>= exec
test = testI >>= exec

load :: FilePath -> IO [Int]
load filePath = do
  strs <- loadFile filePath
  pure (map read $ splitOn "," $ head strs)

exec :: [Int] -> IO ()
exec inputs = do
  --print inputs
  print (part1 inputs)
  print (part2 inputs)

part1 :: [Int] -> Int
part1 ints =
  let v = V.fromList ints
      v' = v V.// [(1,12), (2,2)]
      (_, v'') = allOps v'
  in  v'' V.! 0

part2 :: [Int] -> Int
part2 ints =
  let results = [(x, y, runOpsWith ints x y) | x <- [0..99], y <- [0..99]]
      Just (x, y, _) = find (\(_,_,r) -> r == 19690720) results
  in  100*x + y

runOpsWith :: [Int] -> Int -> Int -> Int
runOpsWith ints x y =
  let v = V.fromList ints
      v' = v V.// [(1,x), (2,y)]
      (_, v'') = allOps v'
  in  v'' V.! 0


allOps :: V.Vector Int -> State
allOps v = loop (0, v)
  where
    loop s = case op s of
      Left s' -> s'
      Right s' -> loop s'

type State = (Int, V.Vector Int)

op :: State -> Either State State
op (pc, v) = case v V.! pc of
  99 -> Left (pc, v)
  1 ->
    let a = v V.! (pc + 1)
        b = v V.! (pc + 2)
        c = v V.! (pc + 3)
        x = v V.! a
        y = v V.! b
        v' = v V.// [(c, x + y)]
    in  Right (pc+4, v')
  2 ->
    let a = v V.! (pc + 1)
        b = v V.! (pc + 2)
        c = v V.! (pc + 3)
        x = v V.! a
        y = v V.! b
        v' = v V.// [(c, x * y)]
    in  Right (pc+4, v')
