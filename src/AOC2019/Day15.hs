{-# LANGUAGE TupleSections #-}
module AOC2019.Day15 where

import Control.Applicative
import Control.Monad
import Control.Monad.Identity
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
import qualified AOC2019.Intcode15 as IC

dataPath = "data/day15/"
runF = dataPath ++ "input.txt"
--testF = dataPath ++ "test.txt"

runI = load runF
--testI = load testF

run = runI >>= exec
--test = testI >>= exec

load :: FilePath -> IO IC.Intcode
load filePath = do
  strs <- loadFile filePath
  pure (map read $ splitOn "," $ head strs)

exec :: IC.Intcode -> IO ()
exec inputs = do
  --print inputs
  print (part1 inputs)
  print (part2 inputs)

part2 :: IC.Intcode -> Int
part2 ic = time
  where
    Just (Left time) = find isLeft $ fillSteps ic

fillSteps :: IC.Intcode -> [Either Int Fill]
fillSteps ic = iterateM fillOxy (Right $ initialFill ic)

initialFill :: IC.Intcode -> Fill
initialFill ic = (osm, Set.singleton fs)
  where
    osm = fullOSM ic
    fs = FillSpot
      { fsTime = 0
      , fsV2 = head $ Map.keys $ Map.filter (== SOxy) osm
      }

fillOxy :: Fill -> Either Int Fill
fillOxy (osm, fq) =
  let (fs, fq') = Set.deleteFindMin fq
      adjEmpties = filter (isEmpty osm . fsV2) $ map (fill fs) allDirs
      fillOsm = Map.fromList [(fsV2 fs, SOxy) | fs <- adjEmpties]
      osm' = Map.union fillOsm osm
      fq'' = Set.union (Set.fromList adjEmpties) fq'
  in  if Set.null fq''
      then Left (fsTime fs)
      else Right (osm', fq'')

fill :: FillSpot -> Dir -> FillSpot
fill fs d = FillSpot
  { fsTime = fsTime fs + 1
  , fsV2 = fsV2 fs + dirToV2 d
  }

isEmpty :: OSM -> V2 Int -> Bool
isEmpty osm v2 = osm Map.! v2 == SEmpty

type Fill = (OSM, FQ)
type FQ = Set.Set FillSpot

data FillSpot = FillSpot
  { fsTime :: Int
  , fsV2 :: V2 Int
  }
  deriving (Eq, Ord, Show)

fullOSM :: IC.Intcode -> OSM
fullOSM ic = osm
  where
    Just (Left osm)
      = find isLeft
      $ iterateM expandAll
      $ (Right $ initialSearch ic)

expandAll :: Search -> Either OSM Search
expandAll search =
  let (_, (osm', sq')) = expand search
  in  if null sq'
      then Left osm'
      else Right (osm', sq')

part1 :: IC.Intcode -> Int
part1 ic = ssLength oxySpot
  where
    Just (Left oxySpot) = find isLeft $ searchSteps ic

isLeft :: Either a b -> Bool
isLeft = either (const True) (const False)

searchSteps :: IC.Intcode -> [Either SearchSpot Search]
searchSteps ic = iterateM expandUntilOxy (Right $ initialSearch ic)

initialSearch :: IC.Intcode -> (OSM, SQ)
initialSearch ic = (initialOSM, initialSQ ic)

initialOSM :: OSM
initialOSM = Map.singleton zero SEmpty

initialSQ :: IC.Intcode -> SQ
initialSQ ic = Set.singleton SearchSpot
  { ssLength = 0
  , ssV2 = zero
  , ssSpot = SEmpty
  , ssPs = IC.initProgram ic
  }

expandUntilOxy :: Search -> Either SearchSpot Search
expandUntilOxy search =
  let (unblockedSpots, search') = expand search
  in  case find ((== SOxy) . ssSpot) unblockedSpots of
        Nothing -> Right search'
        Just oxySpot -> Left oxySpot

expand :: Search -> ([SearchSpot], Search)
expand (osm, sq) =
  let (searchSpot, sq') = Set.deleteFindMin sq
      adjSpots = map (flip move searchSpot) allDirs
      newSpots = filter (flip Map.notMember osm . ssV2) adjSpots
      unblockedSpots = filter ((/= SWall) . ssSpot) newSpots
      sq'' = Set.union (Set.fromList unblockedSpots) sq'
      newV2Spots = [(ssV2 nss, ssSpot nss) | nss <- newSpots]
      osm' = Map.union (Map.fromList newV2Spots) osm
  in  (unblockedSpots, (osm', sq''))

move :: Dir -> SearchSpot -> SearchSpot
move d ss = SearchSpot
  { ssLength = ssLength ss + 1
  , ssV2 = ssV2 ss + dirToV2 d
  , ssSpot = intToSpot $ fromIntegral output
  , ssPs = ps
  }
  where
    (_, ps, [output])
      = runIdentity
      $ IC.runProgramT program readInput (ssPs ss)
    program = IC.executeThroughOp IC.Output
    readInput = pure (fromIntegral $ dirToInt d)

pathToV2 :: Path -> V2 Int
pathToV2 = sum . toList . fmap dirToV2

type Search = (OSM, SQ)

adjacents :: V2 Int -> [V2 Int]
adjacents v = map ((v +) . dirToV2) allDirs

allDirs :: [Dir]
allDirs = [North, South, West, East]

dirToV2 :: Dir -> V2 Int
dirToV2 d = case d of
  North -> V2 0 (-1)
  South -> V2 0 1
  West -> V2 (-1) 0
  East -> V2 1 0

-- Search Queue
-- (path length, spot info)
type SQ = Set.Set SearchSpot

data SearchSpot = SearchSpot
  { ssLength :: Int
  , ssV2 :: V2 Int
  , ssSpot :: Spot
  , ssPs :: IC.ProgramState
  }
  deriving (Eq, Ord, Show)

type Path = Seq.Seq Dir

data Dir
  = North
  | South
  | West
  | East
  deriving (Eq, Ord, Enum, Show)

intToDir :: Int -> Dir
intToDir i = case i of
  1 -> North
  2 -> South
  3 -> West
  4 -> East

dirToInt :: Dir -> Int
dirToInt d = case d of
  North -> 1
  South -> 2
  West -> 3
  East -> 4

type OSM = Map.Map (V2 Int) Spot

data Spot
  = SWall
  | SEmpty
  | SOxy
  deriving (Eq, Ord, Enum, Show)

intToSpot :: Int -> Spot
intToSpot i = case i of
  0 -> SWall
  1 -> SEmpty
  2 -> SOxy

spotToInt :: Spot -> Int
spotToInt s = case s of
  SWall -> 0
  SEmpty -> 1
  SOxy -> 2

printOSM :: OSM -> IO ()
printOSM = mapM_ putStrLn . showOSM

showOSM :: OSM -> [String]
showOSM osm =
  (map.map) (maybe ' ' showSpot . flip Map.lookup osm) coords
  where
    coords = [ [ V2 x y | x <- [minX..maxX] ] | y <- [minY..maxY] ]
    xs = [ x | V2 x y <- Map.keys osm ]
    ys = [ y | V2 x y <- Map.keys osm ]
    minX = minimum xs
    maxX = maximum xs
    minY = minimum ys
    maxY = maximum ys

showSpot :: Spot -> Char
showSpot s = case s of
  SWall -> '#'
  SEmpty -> '.'
  SOxy -> 'O'
