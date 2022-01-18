module AOC2019.Day11 where

import Control.Applicative
import Control.Monad
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
import qualified AOC2019.Intcode9 as IC

dataPath = "data/day11/"
runF = dataPath ++ "input.txt"
testF = dataPath ++ "test.txt"

runI = load runF
testI = load testF

run = runI >>= exec
test = testI >>= exec

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
part2 = mapM_ putStrLn . showPanels . ssPanels . runPaint initShipState2

showPanels :: PanelState -> [String]
showPanels panels =
  [ [ showColor (lookupPanels (V2 x y) panels)
    | x <- [minX..maxX]
    ]
  | y <- [minY..maxY]
  ]
  where
    minX = minimum $ map getX $ Map.keys panels
    maxX = maximum $ map getX $ Map.keys panels
    minY = minimum $ map getY $ Map.keys panels
    maxY = maximum $ map getY $ Map.keys panels
    getX (V2 x _) = x
    getY (V2 _ y) = y

lookupPanels :: V2 Int -> PanelState -> Color
lookupPanels v ps = fromMaybe Cb $ Map.lookup v ps

showColor :: Color -> Char
showColor c = case c of
  Cb -> '.'
  Cw -> '#'

initShipState2 :: ShipState
initShipState2 = ShipState
  { ssRobot = initRobotState
  , ssPanels = Map.singleton zero Cw
  }

part1 :: IC.Program -> Int
part1 = Map.size . ssPanels . runPaint initShipState

runPaint :: ShipState -> IC.Program -> ShipState
runPaint ss0 intcode = loop (IC.initProgram intcode []) ss0
  where
    loop ps ss = case IC.runProgramM (stepM (currentColor ss)) ps of
      (Left (IC.InterruptError err), _) -> error err
      (Left IC.InterruptHalt, _) -> ss
      (Right (color', turn), ps') ->
        let ss' = overRobot (moveRobot . turnRobot turn)
                $ paintCurrent color' ss
        in  loop ps' ss'

stepM :: Color -> IC.ProgramM (Color, Turn)
stepM color = do
  IC.pmQueueInput (fromIntegral $ colorToInt color)
  color' <- intToColor <$> IC.pmExecUntilOutput
  turn <- intToTurn <$> IC.pmExecUntilOutput
  pure (color', turn)

paintCurrent :: Color -> ShipState -> ShipState
paintCurrent color ss = overPanels (Map.insert pos color) ss
  where
    pos = rsPos (ssRobot ss)

turnRobot :: Turn -> RobotState -> RobotState
turnRobot turn = overDir (applyTurn turn)

moveRobot :: RobotState -> RobotState
moveRobot rs = overPos (^+^ dirToV dir) rs
  where
    dir = rsDir rs

currentColor :: ShipState -> Color
currentColor ss = fromMaybe Cb $ Map.lookup pos panels
  where
    pos = rsPos (ssRobot ss)
    panels = ssPanels ss

initShipState :: ShipState
initShipState = ShipState
  { ssRobot = initRobotState
  , ssPanels = Map.empty
  }

initRobotState :: RobotState
initRobotState = RobotState
  { rsPos = zero
  , rsDir = Du
  }

-- type PaintM = IC.ProgramT (State ShipState)

-- runPaintM
--   :: PaintM a -> IC.ProgramState -> ShipState
--   -> (IC.Result a, ShipState)
-- runPaintM pm ps = runState (IC.runProgramT pm ps)

data ShipState = ShipState
  { ssRobot :: RobotState
  , ssPanels :: PanelState
  }
  deriving (Eq, Ord, Show)

overRobot :: (RobotState -> RobotState) -> ShipState -> ShipState
overRobot f ss = ss { ssRobot = f $ ssRobot ss }

overPanels :: (PanelState -> PanelState) -> ShipState -> ShipState
overPanels f ss = ss { ssPanels = f $ ssPanels ss }

data RobotState = RobotState
  { rsPos :: V2 Int
  , rsDir :: Dir
  }
  deriving (Eq, Ord, Show)

overPos :: (V2 Int -> V2 Int) -> RobotState -> RobotState
overPos f rs = rs { rsPos = f $ rsPos rs }

overDir :: (Dir -> Dir) -> RobotState -> RobotState
overDir f rs = rs { rsDir = f $ rsDir rs }

type PanelState = Map.Map (V2 Int) Color

data Color = Cb | Cw
  deriving (Eq, Ord, Show)

colorToInt :: Color -> Int
colorToInt c = case c of
  Cb -> 0
  Cw -> 1

intToColor :: Integer -> Color
intToColor i = case i of
  0 -> Cb
  1 -> Cw

data Dir = Du | Dr | Dd | Dl
  deriving (Eq, Ord, Show)

data Turn = Tl | Tr
  deriving (Eq, Ord, Show)

intToTurn :: Integer -> Turn
intToTurn i = case i of
  0 -> Tl
  1 -> Tr

applyTurn :: Turn -> Dir -> Dir
applyTurn t = case t of
  Tl -> turnLeft
  Tr -> turnRight

turnLeft :: Dir -> Dir
turnLeft dir = case dir of
  Du -> Dl
  Dr -> Du
  Dd -> Dr
  Dl -> Dd

turnRight :: Dir -> Dir
turnRight dir = case dir of
  Du -> Dr
  Dr -> Dd
  Dd -> Dl
  Dl -> Du

dirToV :: Dir -> V2 Int
dirToV dir = case dir of
  Du -> V2 0    (-1)
  Dr -> V2 1    0
  Dd -> V2 0    1
  Dl -> V2 (-1) 0
