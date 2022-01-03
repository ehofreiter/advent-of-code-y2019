module AOC2019.Day3Part2 where

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

dataPath = "data/day3/"
runF = dataPath ++ "input.txt"
testF = dataPath ++ "test.txt"

runI = load runF
testI = load testF

run = runI >>= exec
test = testI >>= exec

load :: FilePath -> IO [Wire]
load filePath = do
  strs <- loadFile filePath
  pure (map readWire strs)

exec :: [Wire] -> IO ()
exec inputs = do
  --print inputs
  print (part2 inputs)

part2 :: [Wire] -> Int
part2 wires =
  let [swa, swb] = map mkSWire wires
      costs = catMaybes $ findCrossCost <$> Set.toList swa <*> Set.toList swb
  in  minimum $ filter (/= 0) costs

findCrossCost :: Seg -> Seg -> Maybe Int
findCrossCost (SH hy (hx0, hx1) hsteps) (SV vx (vy0, vy1) vsteps)
  | between vx (hx0, hx1) && between hy (vy0, vy1) =
    let hwireCost = hsteps + abs (vx - hx0)
        vwireCost = vsteps + abs (hy - vy0)
    in  Just $ hwireCost + vwireCost
  | otherwise = Nothing
findCrossCost sv@(SV _ _ _) sh@(SH _ _ _) = findCrossCost sh sv
findCrossCost _ _ = Nothing

between :: Int -> (Int, Int) -> Bool
between x (a, b) = min a b <= x && x <= max a b

mkSWire :: Wire -> SWire
mkSWire dirs =
  let (_, _, swire) = foldl' f ((0,0), 0, Set.empty) dirs
  in  swire
  where
    f (coord, steps, swire) dir =
      let (coord', steps', seg) = mkSeg coord steps dir
      in  (coord', steps', Set.insert seg swire)

mkSeg :: (Int, Int) -> Int -> Dir -> ((Int, Int), Int, Seg)
mkSeg (x, y) steps dir = case dir of
  DR n -> ((x+n, y), steps+n, SH y (x, x+n) steps)
  DL n -> ((x-n, y), steps+n, SH y (x, x-n) steps)
  DD n -> ((x, y+n), steps+n, SV x (y, y+n) steps)
  DU n -> ((x, y-n), steps+n, SV x (y, y-n) steps)

type SWire = Set.Set Seg
data Seg = SH Int (Int, Int) Int | SV Int (Int, Int) Int
  deriving (Eq, Ord, Show)

type Wire = [Dir]
data Dir = DR Int | DL Int | DU Int | DD Int
  deriving (Eq, Ord, Show)

readWire :: String -> Wire
readWire = map readDir . splitOn ","

readDir :: String -> Dir
readDir (c:cs) = case c of
  'R' -> DR (read cs)
  'L' -> DL (read cs)
  'U' -> DU (read cs)
  'D' -> DD (read cs)
