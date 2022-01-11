module AOC2019.Day8 where

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

dataPath = "data/day8/"
runF = dataPath ++ "input.txt"

runI = load runF

run = runI >>= exec

load :: FilePath -> IO [Layer]
load filePath = do
  strs <- loadFile filePath
  pure (chunksOf 6 $ chunksOf 25 $ map (read . (:[])) $ head strs)

exec :: [Layer] -> IO ()
exec inputs = do
  --print inputs
  --print (part1 inputs)
  part2 inputs

part1 :: [Layer] -> Int
part1 layers =
  let bestLayer = fewestZeros layers
  in  countDigits 1 bestLayer * countDigits 2 bestLayer

type Layer = [[Int]]

countDigits :: Int -> Layer -> Int
countDigits i = length . filter (== i) . concat

countZeros :: Layer -> Int
countZeros = countDigits 0

fewestZeros :: [Layer] -> Layer
fewestZeros = minimumBy (\x y -> compare (countZeros x) (countZeros y))

part2 :: [Layer] -> IO ()
part2 = printLayer . foldl1' combineLayers

printLayer :: Layer -> IO ()
printLayer = mapM_ (putStrLn . showRow)

showRow :: [Int] -> String
showRow = map showPixel

showPixel :: Int -> Char
showPixel p = case p of
  0 -> ' ' -- black
  1 -> '#' -- white
  2 -> '.' -- transparent

combineLayers :: Layer -> Layer -> Layer
combineLayers = zipWith combineRows

combineRows :: [Int] -> [Int] -> [Int]
combineRows = zipWith combinePixels

combinePixels :: Int -> Int -> Int
combinePixels p0 p1 = case p0 of
  2 -> p1
  _ -> p0
