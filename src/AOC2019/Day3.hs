module AOC2019.Day3 where

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
  print (part1 inputs)

part1 :: [Wire] -> Int
part1 wires =
  let [swa, swb] = map mkSWire wires
      crosses = catMaybes $ findCross <$> Set.toList swa <*> Set.toList swb
      dists = filter (/= 0) [abs x + abs y | (x,y) <- crosses]
  in  minimum dists

findCross :: Seg -> Seg -> Maybe (Int, Int)
findCross (SH hy (hx0, hx1)) (SV vx (vy0, vy1))
  | hx0 <= vx && vx <= hx1 && vy0 <= hy && hy <= vy1 = Just (vx, hy)
  | otherwise = Nothing
findCross sv@(SV _ _) sh@(SH _ _) = findCross sh sv
findCross _ _ = Nothing

mkSWire :: Wire -> SWire
mkSWire dirs = snd $ foldl' f ((0,0), Set.empty) dirs
  where
    f (coord, swire) dir =
      let (coord', seg) = mkSeg coord dir
      in  (coord', Set.insert seg swire)

mkSeg :: (Int, Int) -> Dir -> ((Int, Int), Seg)
mkSeg (x, y) dir = case dir of
  DR n -> ((x+n, y), SH y (x, x+n))
  DL n -> ((x-n, y), SH y (x-n, x))
  DD n -> ((x, y+n), SV x (y, y+n))
  DU n -> ((x, y-n), SV x (y-n, y))

type SWire = Set.Set Seg
data Seg = SH Int (Int, Int) | SV Int (Int, Int)
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
