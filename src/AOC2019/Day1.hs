module AOC2019.Day1 where

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
import qualified Text.Parsec as P

import AOC2019.Common

dataPath = "data/day1/"
runF = dataPath ++ "input.txt"
testF = dataPath ++ "test.txt"

runI = load runF
testI = load testF

run = runI >>= exec
test = testI >>= exec

load :: FilePath -> IO [Int]
load filePath = do
  strs <- loadFile filePath
  pure (map read strs)

exec :: [Int] -> IO ()
exec inputs = do
  print (part1 inputs)
  print (part2 inputs)

part1 :: [Int] -> Int
part1 = sum . map ((subtract 2) . (`div` 3))

part2 :: [Int] -> Int
part2 = sum . map modFuel

modFuel :: Int -> Int
modFuel mass = sum $ takeWhile (> 0) $ tail $ iterate' fuel mass

fuel :: Int -> Int
fuel m = m `div` 3 - 2
