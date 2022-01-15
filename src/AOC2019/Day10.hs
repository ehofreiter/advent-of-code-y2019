module AOC2019.Day10 where

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

dataPath = "data/day10/"
runF = dataPath ++ "input.txt"
testF = dataPath ++ "test.txt"

runI = load runF
testI = load testF

run = runI >>= exec
test = testI >>= exec

load :: FilePath -> IO Asteroids
load filePath = do
  strs <- loadFile filePath
  pure (mkAsteroids $ readRows strs)

exec :: Asteroids -> IO ()
exec inputs = do
  --print inputs
  print (part1 inputs)
  print (part2 inputs)

part1 :: Asteroids -> Int
part1 asts = maximum $ Map.elems $ fmap Set.size $ asteroidDirs asts

asteroidDirs :: Asteroids -> Map.Map (V2 Int) (Set.Set (V2 Int))
asteroidDirs asts = Map.fromSet diffs asts
  where
    diffs ast = Set.fromList $ mapMaybe (dir ast) (Set.toList asts)

dir :: V2 Int -> V2 Int -> Maybe (V2 Int)
dir base other = reduce $ other ^-^ base

reduce :: V2 Int -> Maybe (V2 Int)
reduce v@(V2 x y)
  | v == zero = Nothing
  | otherwise = Just $ fmap (`div` gcd x y) v

type Asteroids = Set.Set (V2 Int)

mkAsteroids :: [[Bool]] -> Asteroids
mkAsteroids bss = Set.fromList $ map fst $ filter snd css
  where
    css = [ (V2 x y, b) | (y, bs) <- zip [0..] bss, (x, b) <- zip [0..] bs ]

readRows :: [String] -> [[Bool]]
readRows = map readRow

readRow :: String -> [Bool]
readRow = map readSpot

readSpot :: Char -> Bool
readSpot '#' = True
readSpot '.' = False

part2 :: Asteroids -> Int
part2 asts =
  let best = bestAsteroid asts
      diffSeq = killSequence best asts
      (V2 x y) = diffSeq !! 200 ^+^ best
  in  100*x + y

bestAsteroid :: Asteroids -> V2 Int
bestAsteroid asts = fst $ maximumBy f $ Map.toList $ asteroidDirs asts
  where
    f (v0, s0) (v1, s1) = compare (Set.size s0) (Set.size s1)

killSequence :: V2 Int -> Asteroids -> [V2 Int]
killSequence best asts = concat $ transpose $ map Map.elems $ Map.elems adm
  where
    adm = angleDistMap best asts

angleDistMap ::
  V2 Int -> Asteroids -> Map.Map (Maybe Angle) (Map.Map Int (V2 Int))
angleDistMap best asts = foldl' insertAngle Map.empty diffs
  where
    insertAngle am diff =
      Map.insertWith
        Map.union
        (v2Angle diff)
        (Map.singleton (dist diff) diff)
        am
    diffs = map (^-^ best) $ Set.toList asts

dist :: V2 Int -> Int
dist (V2 x y) = abs x + abs y

-- |
-- >>> sortBy (\v u -> compare (v2Angle v) (v2Angle u)) [V2 x y | x <- [-2..2], y <- [-2..2]]
-- [V2 0 0,V2 0 (-2),V2 0 (-1),V2 1 (-2),V2 1 (-1),V2 2 (-2),V2 2 (-1),V2 1 0,V2 2 0,V2 2 1,V2 1 1,V2 2 2,V2 1 2,V2 0 1,V2 0 2,V2 (-1) 2,V2 (-2) 2,V2 (-1) 1,V2 (-2) 1,V2 (-2) 0,V2 (-1) 0,V2 (-2) (-1),V2 (-2) (-2),V2 (-1) (-1),V2 (-1) (-2)]
v2Angle :: V2 Int -> Maybe Angle
v2Angle (V2 x y)
  | x == 0, y == 0 = Nothing
  | x >= 0, y <  0 = Just $ Angle Q1 $ abs (x % y)
  | x >  0, y >= 0 = Just $ Angle Q2 $ abs (y % x)
  | x <= 0, y >  0 = Just $ Angle Q3 $ abs (x % y)
  | x <  0, y <= 0 = Just $ Angle Q4 $ abs (y % x)

data Angle = Angle Quad (Ratio Int)
  deriving (Eq, Ord, Show)

data Quad
  = Q1 -- +x, -y (upper-right)
  | Q2 -- +x, +y (lower-right)
  | Q3 -- -x, +y (lower-left)
  | Q4 -- -x, -y (upper-left)
  deriving (Eq, Ord, Show)
