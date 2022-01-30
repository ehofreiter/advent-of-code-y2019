{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
module AOC2019.Day13Part2 where

import Control.Applicative
import Control.Concurrent
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
  print (part1 inputs)
  part2 inputs

part2 :: IC.Program -> IO ()
part2 intcode = breakout intcode solve

breakoutPrompt :: IC.Program -> IO ()
breakoutPrompt intcode = do
  putStrLn "Choose mode: a/p/s (a = autoplay, p = play, s = solve)"
  playType <- getInputLn "Invalid input." $ \case
    'a':_ -> Just autoPlay
    'p':_ -> Just play
    's':_ -> Just solve
    _ -> Nothing
  breakout intcode playType

breakout :: IC.Program -> ([Integer] -> StateT GameState IO Integer) -> IO ()
breakout intcode playType = do
  ((_, ps, outputs), gameState) <- runStateT (resultM playType) Map.empty
  printGameState (updateGameState outputs gameState)
  where
    resultM p = IC.runProgramT (IC.interactProgram p) (pure 0) ps0
    ps0 = IC.initProgram (mkFree intcode)

mkFree :: IC.Program -> IC.Program
mkFree (i:p) = 2:p

solve :: (MonadState GameState m, MonadIO m) => [Integer] -> m Integer
solve outputs = do
  modify' (updateGameState outputs)
  gets autoJoystick

autoPlay :: (MonadState GameState m, MonadIO m) => [Integer] -> m Integer
autoPlay outputs = do
  updatePrintGameState outputs
  gameState <- get
  let move = autoJoystick gameState
  liftIO $ putStrLn $ "Automove: " <> show move
  liftIO $ threadDelay (10^4)
  pure move

autoJoystick :: GameState -> Integer
autoJoystick gs = fromIntegral $ signum $ ballX - paddleX
  where
    V2 ballX _ = ballPos gs
    V2 paddleX _ = paddlePos gs

play :: (MonadState GameState m, MonadIO m) => [Integer] -> m Integer
play outputs = do
  updatePrintGameState outputs
  liftIO getJoystick

getJoystick :: IO Integer
getJoystick = do
  putStrLn "Tilt joystick: a/s/d (a = left, s = neutral, d = right)"
  getInputLn "Invalid input." $ \case
    "a" -> Just (-1)
    "s" -> Just 0
    "d" -> Just 1
    _ -> Nothing

updatePrintGameState
  :: (MonadState GameState m, MonadIO m)
  => [Integer] -> m ()
updatePrintGameState outputs = do
  modify' (updateGameState outputs)
  gameState <- get
  printGameState gameState

type GameState = Map.Map (V2 Int) Int

ballPos :: GameState -> V2 Int
ballPos = head . tilePositions Tball

paddlePos :: GameState -> V2 Int
paddlePos = head . tilePositions Tpaddle

tilePositions :: Tid -> GameState -> [V2 Int]
tilePositions tid = Map.keys . Map.filter (== tidToInt tid)

updateGameState :: [Integer] -> GameState -> GameState
updateGameState outputs = Map.union newChunks
  where
    newChunks = Map.fromList $ map f $ chunksOf 3 outputs
    f [x,y,t] = (V2 (fromIntegral x) (fromIntegral y), fromIntegral t)

showGameState :: GameState -> [String]
showGameState gs = "Score: " <> show score : showScreen screen
  where
    (score, screen) = bimap (fromMaybe 0) (fmap intToTid) $
      Map.updateLookupWithKey (\_ _ -> Nothing) (V2 (-1) 0) gs

printGameState :: MonadIO m => GameState -> m ()
printGameState = liftIO . mapM_ putStrLn . showGameState

part1 :: IC.Intcode -> Either (IC.Result ()) Int
part1 = fmap blockCount . extractScreen

blockCount :: Screen -> Int
blockCount = length . filter (== Tblock) . Map.elems

extractScreen :: IC.Intcode -> Either (IC.Result ()) Screen
extractScreen intcode =
  case IC.runProgramM program 0 intcode of
    (Left IC.InterruptHalt, _, outputs) ->
      let tiles = map mkTile . chunksOf 3 $ outputs
          screen = foldl' drawTile Map.empty tiles
      in  Right screen
    result -> Left result
  where
    program = IC.interactProgram (const $ pure 0)
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
