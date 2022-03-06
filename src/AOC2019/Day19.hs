{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
module AOC2019.Day19 where

import Control.Applicative
import Control.Monad
import Control.Monad.Identity
import Control.Monad.State
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

dataPath = "data/day19/"
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
  --print (part1 inputs)
  print (part2 inputs)

part2 :: IC.Intcode -> Integer
part2 ic = answer topLeft
  where
    topLeft = bottomLeft - V2 0 99
    bottomLeft = findMinShipFit ic

answer :: Coord -> Integer
answer (V2 x y) = x*10000 + y

findMinShipFit :: IC.Intcode -> Coord
findMinShipFit ic = minC
  where
    minC = findMinYC minXC
    findMinYC c
      | doesShipFit ic aboveC = findMinYC aboveC
      | otherwise             = c
      where
        aboveC = c - V2 0 1
    minXC = findMinXC initXC
    initXC = findShipFit ic
    findMinXC c = case maxShipFitCol ic (c - V2 1 0) of
      Just maxYC -> findMinXC maxYC
      Nothing    -> c

maxShipFitCol :: IC.Intcode -> Coord -> Maybe Coord
maxShipFitCol ic c
  | doesShipFit ic maxYC = Just maxYC
  | otherwise            = Nothing
  where
    maxYC = fromJust $ find (isBeamAt ic) col
    col = iterate (^-^ V2 0 1) c

findShipFit :: IC.Intcode -> Coord
findShipFit ic = fromJust $ find (doesShipFit ic) (zigZag ic)

doesShipFit :: IC.Intcode -> Coord -> Bool
doesShipFit ic bottomLeft = isBeamAt ic topRight
  where
    topRight = bottomLeft + V2 99 (-99)

zigZag :: IC.Intcode -> [Coord]
zigZag ic = unfoldr nextCoord (initRightEdge, True)
  where
    nextCoord (coord, isDown) = Just (newCoord, (newCoord, not isDown))
      where
        newCoord = findEdge ic coord
        findEdge = if isDown then findBottomEdge else findRightEdge

-- From inspection of part1 output, can see this is a right edge.
initRightEdge :: Coord
initRightEdge = V2 47 49

findRightEdge :: IC.Intcode -> Coord -> Coord
findRightEdge ic (V2 x y) = loopVoid x 1
  where
    loopVoid minX step
      | isBeamAt ic (V2 newX y) = loopVoid newX (step*2)
      | otherwise               = loop (minX, newX)
      where
        newX = minX + step
    loop (minX, maxX)
      | newX == minX            = V2 minX y
      | isBeamAt ic (V2 newX y) = loop (newX, maxX)
      | otherwise               = loop (minX, newX)
      where
        newX = (maxX + minX) `div` 2

findBottomEdge :: IC.Intcode -> Coord -> Coord
findBottomEdge ic (V2 x y) = loopVoid y 1
  where
    loopVoid minY step
      | isBeamAt ic (V2 x newY) = loopVoid newY (step*2)
      | otherwise               = loop (minY, newY)
      where
        newY = minY + step
    loop (minY, maxY)
      | newY == minY            = V2 x minY
      | isBeamAt ic (V2 x newY) = loop (newY, maxY)
      | otherwise               = loop (minY, newY)
      where
        newY = (maxY + minY) `div` 2

part1 :: IC.Intcode -> Int
part1 = beamCoordCount . mkBeamMap

beamCoordCount :: [[Bool]] -> Int
beamCoordCount = length . filter id . concat

showBeamMap :: [[Bool]] -> String
showBeamMap = unlines . (map.map) boolToChar
  where
    boolToChar b = if b then '#' else '.'

mkBeamMap :: IC.Intcode -> [[Bool]]
mkBeamMap ic = (map.map) (isBeamAt ic) areaCoords

areaCoords :: [[Coord]]
areaCoords = [[V2 x y | x <- [0..49]] | y <- [0..49]]

isBeamAt :: IC.Intcode -> Coord -> Bool
isBeamAt ic (V2 x y) = head outputs == 1
  where
    (_, _, outputs)
      = flip evalState initState
      $ IC.runProgramT IC.execute pop initPs
    initPs = IC.initProgram ic
    initState = [x, y]
    pop = do
      inputs <- get
      case inputs of
        [] -> error "no inputs"
        input:inputs' -> do
          put inputs'
          pure input

type Coord = V2 Integer

terminalWith :: IC.Intcode -> [Integer] -> IO ()
terminalWith ic initState = do
  (result, _)
    <- flip runStateT initState
    $  IC.runProgramT program (pure 0) initPs
  let (_, _, outputs) = result
  putStrLn ("O: " <> show outputs)
  print "done"
  where
    initPs = IC.initProgram ic
    program = IC.interactProgramT inFromOut

-- | Runs the Intcode program interactively in the terminal.
terminal :: IC.Intcode -> IO ()
terminal ic = terminalWith ic []

inFromOut :: (MonadState [Integer] m, MonadIO m) => [Integer] -> m Integer
inFromOut outputs = do
  liftIO $ putStrLn ("O: " <> show outputs)
  popInputs

popInputs :: (MonadState [Integer] m, MonadIO m) => m Integer
popInputs = do
  queuedInputs <- get
  case queuedInputs of
    [] -> do
      getInputs
      popInputs
    i:inputs -> do
      put inputs
      liftIO $ putStrLn ("I: " <> show i)
      pure i

getInputs :: (MonadState [Integer] m, MonadIO m) => m ()
getInputs = do
  input <- liftIO readLn
  put [input]
