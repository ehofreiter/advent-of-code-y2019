module AOC2019.Day4 where

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

runI :: (Int, Int)
runI = (246540, 787419)

part1 :: (Int, Int) -> Int
part1 (a, b) =
  let ints = map read $ filter hasDouble allNumbers
      inRange = filter (\x -> a <= x && x <= b) ints
  in  length inRange

part2 :: (Int, Int) -> Int
part2 (a, b) =
  let ints = map read $ filter hasDouble2 allNumbers
      inRange = filter (\x -> a <= x && x <= b) ints
  in  length inRange

hasDouble2 :: String -> Bool
hasDouble2 s = any ((== 2) . length) $ group s

hasDouble :: String -> Bool
hasDouble s = or $ zipWith (==) s (tail s)

allNumbers :: [String]
allNumbers = do
  d1 <- [2..9]
  d2 <- [d1..9]
  d3 <- [d2..9]
  d4 <- [d3..9]
  d5 <- [d4..9]
  d6 <- [d5..9]
  pure $ concatMap show [d1,d2,d3,d4,d5,d6]
