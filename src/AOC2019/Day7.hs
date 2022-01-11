module AOC2019.Day7 where

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
import qualified AOC2019.Intcode as IC

dataPath = "data/day7/"
runF = dataPath ++ "input.txt"
testF = dataPath ++ "test.txt"
test2F = dataPath ++ "test2.txt"

runI = load runF
testI = load testF
test2I = load test2F

run = runI >>= exec
test = testI >>= exec
test2 = test2I >>= exec

load :: FilePath -> IO IC.Program
load filePath = do
  strs <- loadFile filePath
  pure (map read $ splitOn "," $ head strs)

exec :: IC.Program -> IO ()
exec inputs = do
  --print inputs
  --print (part1 inputs)
  print (part2 inputs)

part1 :: IC.Program -> Int
part1 intcode =
  maximum $ map (runAmpSeq intcode) allPhaseSeqs

allPhaseSeqs :: [PhaseSeq]
allPhaseSeqs = permutations [0..4]

runAmpSeq :: IC.Program -> PhaseSeq -> Int
runAmpSeq intcode phases =
  foldl' f 0 phases
  where
    f input phase = runAmp intcode phase input

type PhaseSeq = [Phase]

type Phase = Int

runAmp :: IC.Program -> Phase -> Int -> Int
runAmp intcode phase input =
  let result = IC.runProgram intcode [phase, input]
  in case result of
    (Left IC.InterruptHalt, ps) -> head $ IC.psOutputs ps
    _ -> error $ "phase: " <> show phase <> "error: " <> show result

part2 :: IC.Program -> Int
part2 intcode =
  maximum $ map f allPhaseSeqs2
  where
    f phaseSeq =
      extractFinalOutput $ runAmps $ initialAmpState intcode phaseSeq

extractFinalOutput :: AmpState -> Int
extractFinalOutput (_, ampMap) = head $ IC.psOutputs $ ampMap Map.! AE

allPhaseSeqs2 :: [PhaseSeq]
allPhaseSeqs2 = permutations [5..9]

initialAmpState :: IC.Program -> PhaseSeq -> AmpState
initialAmpState intcode phases = (AA, ampMap)
  where
    ampMap = Map.fromList $ zip [AA .. AE] (map initAmp phases)
    initAmp phase = IC.initProgram intcode [phase]

runAmps :: AmpState -> AmpState
runAmps = loop 0
  where
    loop input ampState0 = case stepAmpState (input, ampState0) of
      (Left IC.InterruptHalt, ampState1) -> ampState1
      (Right output, ampState1) -> loop output ampState1

stepAmpState :: (Int, AmpState) -> (Either IC.Interrupt Int, AmpState)
stepAmpState (input, (aid, am)) =
  let (eOutput, ps) = IC.runProgramM (pmInOut input) (am Map.! aid)
  in  (eOutput, (nextAmpId aid, Map.insert aid ps am))

pmInOut :: Int -> IC.ProgramM Int
pmInOut input = do
  IC.pmQueueInput input
  IC.pmExecUntilOutput

data AmpId = AA | AB | AC | AD | AE
  deriving (Eq, Ord, Show, Enum)

nextAmpId :: AmpId -> AmpId
nextAmpId a = case a of
  AA -> AB
  AB -> AC
  AC -> AD
  AD -> AE
  AE -> AA

type AmpMap = Map.Map AmpId IC.ProgramState

type AmpState = (AmpId, AmpMap) -- current amp, state

