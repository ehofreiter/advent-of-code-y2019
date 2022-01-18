{-# LANGUAGE TupleSections #-}
module AOC2019.Day12 where

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
import Linear.V3
import Linear.Vector
import qualified Text.Parsec as P

import AOC2019.Common
import qualified AOC2019.Intcode9 as IC

dataPath = "data/day12/"
runF = dataPath ++ "input.txt"
testF = dataPath ++ "test.txt"

runI = load runF
testI = load testF

run = runI >>= exec
test = testI >>= exec

load :: FilePath -> IO [Pos]
load filePath = do
  strs <- loadFile filePath
  pure (map readPos strs)

exec :: [Pos] -> IO ()
exec inputs = do
  print inputs
  print (part1 inputs)
  print (part2 inputs)

type Coord = (Int, Int) -- Pos, Vel

part2 :: [Pos] -> Integer
part2 ps0 =
  let V3 xs0 ys0 zs0 = initialCs ps0
      pdx = fromIntegral $ findPeriodC xs0 :: Integer
      pdy = fromIntegral $ findPeriodC ys0 :: Integer
      pdz = fromIntegral $ findPeriodC zs0 :: Integer
  in  lcm (lcm pdx pdy) pdz

initialCs :: [Pos] -> V3 [Coord]
initialCs ps = V3 xs ys zs
  where
    xs = map (\(V3 x _ _) -> (x, 0)) ps
    ys = map (\(V3 _ y _) -> (y, 0)) ps
    zs = map (\(V3 _ _ z) -> (z, 0)) ps

findPeriodC :: [Coord] -> Int
findPeriodC = loop Set.empty 0
  where
    loop prevs n cs =
      if Set.member cs prevs
      then n
      else loop (Set.insert cs prevs) (n+1) (stepC cs)

stepC :: [Coord] -> [Coord]
stepC = applyVelC . applyGravC

applyVelC :: [Coord] -> [Coord]
applyVelC = map velC

velC :: Coord -> Coord
velC (p,v) = (p+v, v)

applyGravC :: [Coord] -> [Coord]
applyGravC cs = map f cs
  where
    f c = second (+ sum (map (gravC c) cs)) c

gravC :: Coord -> Coord -> Int
gravC (x,v) (x',v') = case compare x x' of
  EQ -> 0
  LT -> 1
  GT -> -1

part1 :: [Pos] -> Int
part1 ps0 =
  let ms0 = initialMoons ps0
      ms1000 = iterate' step ms0 !! 1000
      e = sum $ map energy ms1000
  in  e

type Pos = V3 Int
type Vel = V3 Int
type Moon = (Pos, Vel)

energy :: Moon -> Int
energy (p,v) = l0Norm p * l0Norm v

l0Norm :: V3 Int -> Int
l0Norm (V3 x y z) = abs x + abs y + abs z

step :: [Moon] -> [Moon]
step = applyVelocity . applyGravity

applyGravity :: [Moon] -> [Moon]
applyGravity ms = map f ms
  where
    f m = second (^+^ sum (map (gravity m) ms)) m

gravity :: Moon -> Moon -> V3 Int
gravity (p,v) (p',v') = liftI2 g p p'
  where
    g x x' = case compare x x' of
      EQ -> 0
      LT -> 1
      GT -> -1

applyVelocity :: [Moon] -> [Moon]
applyVelocity = map velocity

velocity :: Moon -> Moon
velocity (p, v) = (p ^+^ v, v)

initialMoons :: [Pos] -> [Moon]
initialMoons = map (, zero)

readPos :: String -> Pos
readPos str = V3 x y z
  where
    [x,y,z] = map readAxis $ splitOn "," $ init $ tail str

readAxis :: String -> Int
readAxis s = read $ splitOn "=" s !! 1
