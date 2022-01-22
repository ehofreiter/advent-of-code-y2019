{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}
module AOC2019.Day13Part2 where

import Control.Applicative
import Control.Monad
import Control.Monad.State
import Control.Monad.Writer
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
import qualified AOC2019.Intcode13 as IC

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
  --print (part1 inputs)
  part2 inputs

part2 :: IC.Program -> IO ()
part2 intcode = do
  putStrLn "part2"
  (_, finalGameState) <- flip runStateT Map.empty $ runWriterT
    $ IC.interactProgram (mkFree intcode) outputInput
  printGameState finalGameState
  putStrLn "done"

mkFree :: IC.Program -> IC.Program
mkFree (i:p) = 2:p

outputInput
  :: (MonadState GameState m, MonadIO m)
  => [Integer] -> m Integer
outputInput outputs = do
  modify' (Map.union newChunks)
  gameState <- get
  printGameState gameState
  liftIO readLn
  where
    newChunks = Map.fromList $ map f $ chunksOf 3 outputs
    f [x,y,t] = (V2 (fromIntegral x) (fromIntegral y), fromIntegral t)

showGameState :: GameState -> [String]
showGameState gs = "Score: " <> show score : showScreen screen
  where
    chunks = map mkChunk $ Map.toList gs
    screen = Map.fromList $ mapMaybe getTile chunks
    score = asum $ map getScore chunks

printGameState :: MonadIO m => GameState -> m ()
printGameState = liftIO . mapM_ putStrLn . showGameState

type GameState = Map.Map (V2 Int) Int

getTile :: Chunk -> Maybe Tile
getTile c = case c of
  Tile t -> Just t
  _ -> Nothing

getScore :: Chunk -> Maybe Int
getScore c = case c of
  Score s -> Just s
  _ -> Nothing

data Chunk
  = Tile Tile
  | Score Int

mkChunk :: (V2 Int, Int) -> Chunk
mkChunk (v, t) =
  case v of
    V2 (-1) 0 -> Score t
    _         -> Tile (v, intToTid t)

part1 :: IC.Program -> Either (IC.Result (Writer [Integer]) ()) Int
part1 = fmap blockCount . extractScreen

blockCount :: Screen -> Int
blockCount = length . filter (== Tblock) . Map.elems

extractScreen :: IC.Program -> Either (IC.Result (Writer [Integer]) ()) Screen
extractScreen intcode =
  case fst $ runWriter $ IC.interactProgram intcode (const $ pure 0) of
    ((Left IC.InterruptHalt, _), outputs) ->
      let tiles = map mkTile . chunksOf 3 $ outputs
          screen = foldl' drawTile Map.empty tiles
      in  Right screen
    (result, _) -> Left result
  where
    mkTile [x,y,t] =
      (V2 (fromIntegral x) (fromIntegral y), intToTid (fromIntegral t))

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
  Tpaddle -> 'T'
  Tball -> 'O'

type Screen = Map.Map (V2 Int) Tid

tidToInt :: Tid -> Int
tidToInt = fromEnum

intToTid :: Int -> Tid
intToTid = toEnum

drawTile :: Screen -> Tile -> Screen
drawTile s (tpos, tid) = Map.insert tpos tid s
