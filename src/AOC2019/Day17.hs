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
  --print (part1 inputs)
  part2 inputs

part2 :: IC.Intcode -> IO ()
part2 ic = do
  let fp = head $ fullPrograms $ initActions ic
  tryRoutineFullProg ic fp

initActions :: IC.Intcode -> [Action]
initActions = walkScaffold . mkSM . initScaffold

fullPrograms :: [Action] -> [FullProg]
fullPrograms actions = do
  let fActions0 = map Right actions
  defA <- nextDefs fActions0
  let fActions1 = condenseFunc (MFA, defA) fActions0
  defB <- nextDefs fActions1
  let fActions2 = condenseFunc (MFB, defB) fActions1
  defC <- nextDefs fActions2
  let fActions3 = condenseFunc (MFC, defC) fActions2
      moveFuncs = map (either Just (const Nothing)) fActions3
  guard (all isJust moveFuncs)
  Just mainDef <- [mkDef $ catMaybes moveFuncs]
  [FullProg mainDef defA defB defC]

showFullProg :: FullProg -> [String]
showFullProg fp
  = intShowStr (fpMain fp)
  : map intShowStr [fpA fp, fpB fp, fpC fp]

type FAction = Either MoveFunc Action

nextDefs :: [FAction] -> [Def Action]
nextDefs fActions = mapMaybe mkDef . reverse . drop 1 $ inits actions
  where
    actions = catMaybes
            . takeWhile isJust
            . dropWhile isNothing
            $ map (either (const Nothing) Just) fActions

condenseFunc :: (MoveFunc, Def Action) -> [FAction] -> [FAction]
condenseFunc (mf, def@(Def defActions)) fActions =
  case (defActions, fActions) of
    ([], _) -> fActions
    (_, []) -> []
    (_, fAct:fActs) ->
      case stripPrefix (map Right defActions) fActions of
        Nothing -> fAct : condenseFunc (mf, def) fActs
        Just fActs' -> Left mf : condenseFunc (mf, def) fActs'

subseqIndexes :: Eq a => [a] -> [a] -> [Int]
subseqIndexes sub list = catMaybes $ imap f (tails list)
  where
    f i list = if sub `isPrefixOf` list then Just i else Nothing

type WalkState = (V2 Int, Dir)

walkScaffold :: SM -> [Action]
walkScaffold sm = case startRobot sm of
  Nothing -> []
  Just ws0 -> reverse $ loop ws0 []
  where
    loop ws actions =
      case getNextStep sm ws of
        Nothing -> actions
        Just (action, ws') -> loop ws' $ prependAction action actions

prependAction :: Action -> [Action] -> [Action]
prependAction (Forward d) (Forward d' : acts) = Forward (d + d') : acts
prependAction act acts = act : acts

getNextStep :: SM -> WalkState -> Maybe (Action, WalkState)
getNextStep sm ws@(pos, dir) = do
  na <- mNextAction
  Just (na, applyAction na ws)
  where
    mNextAction = asum (map turnToAction turns)
    turnToAction mTurn = do
      let dir' = maybe id applyTurn mTurn dir
          pos' = pos + dirToV dir'
      SScaffold <- Map.lookup pos' sm
      pure $ maybe (Forward 1) Turn mTurn
    turns = [Nothing, Just TL, Just TR]

applyAction :: Action -> WalkState -> WalkState
applyAction action (pos, dir) = case action of
  Forward d -> (pos + dirToV dir ^* d, dir)
  Turn turn -> (pos, applyTurn turn dir)

applyTurn :: Turn -> Dir -> Dir
applyTurn turn dir =
  let iDir = fromEnum dir
      iTurn = turnToInt turn
  in  toEnum $ (iDir + iTurn) `mod` 4

turnToInt :: Turn -> Int
turnToInt TL = -1
turnToInt TR = 1

dirToV :: Dir -> V2 Int
dirToV dir = case dir of
  DU -> V2 0 (-1)
  DR -> V2 1 0
  DD -> V2 0 1
  DL -> V2 (-1) 0

startPos :: SM -> Maybe (V2 Int)
startPos = fmap fst . startRobot

startRobot :: SM -> Maybe (V2 Int, Dir)
startRobot = listToMaybe . mapMaybe robotDir . Map.toList
  where
    robotDir (pos, spot) = case spot of
      SRobot dir -> Just (pos, dir)
      _ -> Nothing

tryRoutineFullProg :: IC.Intcode -> FullProg -> IO ()
tryRoutineFullProg ic fp = tryRoutineWith ic initState
  where
    initState
      = concat
      $ intShowLn (fpMain fp)
      : map intShowLn [fpA fp, fpB fp, fpC fp]
      ++ [toAscii "n\n"]

tryRoutineWith :: IC.Intcode -> [Integer] -> IO ()
tryRoutineWith ic initState = do
  (result, _)
    <- flip runStateT initState
    $  IC.runProgramT program (pure 0) initPs
  let (_, _, outputs) = result
  putStrLn $ fromAscii outputs
  print (last outputs)
  where
    initPs = IC.initProgram $ wakeUp ic
    program = IC.interactProgramT inFromOut

-- | Runs the Intcode program interactively in the terminal.
tryRoutine :: IC.Intcode -> IO ()
tryRoutine ic = tryRoutineWith ic []

inFromOut :: (MonadState [Integer] m, MonadIO m) => [Integer] -> m Integer
inFromOut outputs = do
  liftIO $ putStr $ fromAscii outputs
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
      liftIO $ putChar (toEnum $ fromIntegral i)
      pure i

getInputs :: (MonadState [Integer] m, MonadIO m) => m ()
getInputs = do
  rawInputs <- liftIO getLine
  put $ toAscii (rawInputs <> "\n")

fromAscii :: [Integer] -> String
fromAscii = map (toEnum . fromIntegral)

toAscii :: String -> [Integer]
toAscii = map (fromIntegral . fromEnum)

data FullProg = FullProg
  { fpMain :: Def MoveFunc
  , fpA :: Def Action
  , fpB :: Def Action
  , fpC :: Def Action
  } deriving (Eq, Ord, Show)

fpFunc :: FullProg -> MoveFunc -> Def Action
fpFunc fp mf = case mf of
  MFA -> fpA fp
  MFB -> fpB fp
  MFC -> fpC fp

expandFull :: FullProg -> [Action]
expandFull fp = concatMap (expandFunc fp) (unDef $ fpMain fp)

expandFunc :: FullProg -> MoveFunc -> [Action]
expandFunc fp mf = unDef $ fpFunc fp mf

newtype Def a = Def { unDef :: [a] }
  deriving (Eq, Ord, Show)

mkDef :: IntShow a => [a] -> Maybe (Def a)
mkDef xs = if defLength > maxDefLength then Nothing else Just def
  where
    def = Def xs
    defLength = length (intShowStr def)

maxDefLength :: Int
maxDefLength = 20

instance IntShow a => IntShow (Def a) where
  intShowStr = intercalate "," . map intShowStr . unDef

data MoveFunc = MFA | MFB | MFC
  deriving (Eq, Ord, Show)

instance IntShow MoveFunc where
  intShowStr mf = case mf of
    MFA -> "A"
    MFB -> "B"
    MFC -> "C"

data Action
  = Forward Int
  | Turn Turn
  deriving (Eq, Ord, Show)

instance IntShow Action where
  intShowStr a = case a of
    Forward d -> show d
    Turn TL -> "L"
    Turn TR -> "R"

class IntShow a where
  intShowStr :: a -> String

intShow :: IntShow a => a -> [Integer]
intShow = toAscii . intShowStr

intShowLn :: IntShow a => a -> [Integer]
intShowLn = toAscii . (++ "\n") . intShowStr

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
    vs = v : adjVs v

adjVs :: V2 Int -> [V2 Int]
adjVs v = [v + V2 0 1, v - V2 0 1, v + V2 1 0, v - V2 1 0]

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
