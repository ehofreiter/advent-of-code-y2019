{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
module AOC2019.Day17 where

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

dataPath = "data/day17/"
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
  -- print (part2 inputs)

tryRoutine :: IC.Intcode -> IO ()
tryRoutine ic = do
  (result, _) <- flip runStateT [] $ IC.runProgramT program (pure 0) initPs
  let (_, _, outputs) = result
  putStrLn $ showOutputs outputs
  where
    initPs = IC.initProgram $ wakeUp ic
    program = IC.interactProgramT inFromOut

inFromOut :: (MonadState [Integer] m, MonadIO m) => [Integer] -> m Integer
inFromOut outputs = do
  liftIO $ putStrLn $ showOutputs outputs
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
      pure i

getInputs :: (MonadState [Integer] m, MonadIO m) => m ()
getInputs = do
  rawInputs <- liftIO getLine
  put $ readInputs (rawInputs <> "\n")

showOutputs :: [Integer] -> String
showOutputs = map (toEnum . fromIntegral)

readInputs :: String -> [Integer]
readInputs = map (fromIntegral . fromEnum)

data MoveFunc = MFA | MFB | MFC
  deriving (Eq, Ord, Show)

showMoveFunc :: MoveFunc -> String
showMoveFunc mf = case mf of
  MFA -> "A"
  MFB -> "B"
  MFC -> "C"

data Action
  = Forward Int
  | Turn Turn
  deriving (Eq, Ord, Show)

showAction :: Action -> String
showAction a = case a of
  Forward d -> show d
  Turn TL -> "L"
  Turn TR -> "R"

data Turn = TL | TR
  deriving (Eq, Ord, Show)

wakeUp :: IC.Intcode -> IC.Intcode
wakeUp (i:ic) = 2:ic

part1 :: IC.Intcode -> Int
part1 ic = sum . map alignParam . filter (isIntersection sm) $ Map.keys sm
  where
    sm = mkSM $ initScaffold ic

alignParam :: V2 Int -> Int
alignParam (V2 x y) = x * y

isIntersection :: SM -> V2 Int -> Bool
isIntersection sm v = all (== Just SScaffold) spots
  where
    spots = map (flip Map.lookup sm) vs
    vs = [v, v + V2 0 1, v - V2 0 1, v + V2 1 0, v - V2 1 0]

type SM = Map.Map (V2 Int) Spot

data Spot
  = SEmpty
  | SScaffold
  | SRobot Dir
  | SRobotDead
  deriving (Eq, Ord, Show)

showSpot :: Spot -> Char
showSpot s = case s of
  SEmpty -> '.'
  SScaffold -> '#'
  SRobot DU -> '^'
  SRobot DR -> '>'
  SRobot DD -> 'v'
  SRobot DL -> '<'
  SRobotDead -> 'X'

readSpot :: Char -> Spot
readSpot c = case c of
  '.' -> SEmpty
  '#' -> SScaffold
  '^' -> SRobot DU
  '>' -> SRobot DR
  'v' -> SRobot DD
  '<' -> SRobot DL
  'X' -> SRobotDead

data Dir
  = DU
  | DR
  | DD
  | DL
  deriving (Eq, Ord, Enum, Show)

mkSM :: String -> SM
mkSM s = Map.fromList
  [ (V2 x y, readSpot c)
  | (r, y) <- zip (lines s) [0..]
  , (c, x) <- zip r [0..]
  ]

initScaffold :: IC.Intcode -> String
initScaffold ic = map (toEnum . fromIntegral) outputs
  where
    (_, _, outputs) = result
    result = runIdentity $ IC.runProgramT IC.execute (pure 0) initPs
    initPs = IC.initProgram ic
