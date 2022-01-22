{-# LANGUAGE TupleSections #-}
module AOC2019.Day13 where

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
import Data.Ratio
import Data.Traversable
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Sequence as Seq
import Data.Sequence (Seq(..), (><))
import qualified Data.Vector as V
import Linear.V2
import Linear.Vector
import qualified Text.Parsec as P

import AOC2019.Common
import qualified AOC2019.Intcode9 as IC

dataPath = "data/day13/"
runF = dataPath ++ "input.txt"
--testF = dataPath ++ "test.txt"

runI = load runF
--testI = load testF

run = runI >>= exec
--test = testI >>= exec

load :: FilePath -> IO IC.Program
load filePath = do
  strs <- loadFile filePath
  pure (map read $ splitOn "," $ head strs)

exec :: IC.Program -> IO ()
exec inputs = do
  --print inputs
  print (part1 inputs)
  --part2 inputs

-- part2 :: IC.Program -> IO ()
-- part2 intcode = mkFree intcode

mkFree :: IC.Program -> IC.Program
mkFree (i:p) = 2:p

part1 :: IC.Program -> Either (IC.Result ()) Int
part1 = fmap blockCount . extractScreen

blockCount :: Screen -> Int
blockCount = length . filter (== Tblock) . Map.elems

extractScreen :: IC.Program -> Either (IC.Result ()) Screen
extractScreen intcode =
  case IC.runProgram intcode [] of
    (Left IC.InterruptHalt, ps) ->
      let tiles = map mkTile . chunksOf 3 . reverse $ IC.psOutputs ps
          screen = foldl' drawTile Map.empty tiles
      in  Right screen
    result -> Left result
  where
    mkTile [x,y,t] =
      (V2 (fromIntegral x) (fromIntegral y), intToTid (fromIntegral t))

type Screen = Map.Map (V2 Int) Tid

showScreen :: Screen -> [String]
showScreen screen =
  [ [ showMTile (Map.lookup (V2 x y) screen) | x <- xs ] | y <- ys ]
  where
    xs = [0..maxX]
    ys = [0..maxY]
    coords = Map.keys screen
    maxX = maximum $ map (\(V2 x _) -> x) coords
    maxY = maximum $ map (\(V2 _ y) -> y) coords
    showMTile mt = case mt of
      Nothing -> '?'
      Just tid -> showTile tid

type Tile = (V2 Int, Tid)

data Tid
  = Tempty
  | Twall
  | Tblock
  | Tpaddle
  | Tball
  deriving (Eq, Ord, Show, Enum)

showTile :: Tid -> Char
showTile tid = case tid of
  Tempty -> ' '
  Twall -> '#'
  Tblock -> '='
  Tpaddle -> '_'
  Tball -> 'O'

tidToInt :: Tid -> Int
tidToInt = fromEnum

intToTid :: Int -> Tid
intToTid = toEnum

drawTile :: Screen -> Tile -> Screen
drawTile s (tpos, tid) = Map.insert tpos tid s
