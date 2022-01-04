{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ParallelListComp #-}
module AOC2019.Intcode where

import Control.Monad.Except
import Control.Monad.Identity
import Control.Monad.State
import Control.Monad.Writer
import Data.List
import Data.Maybe
import qualified Data.Vector as V

type Program = [Int]

newtype Address = A Int
  deriving (Eq, Ord, Num, Enum, Show)

unA :: Address -> Int
unA (A n) = n

newtype Memory = Memory { memoryToVector :: V.Vector Int }
  deriving (Eq, Ord, Show)

(!) :: Memory -> Address -> Int
(!) mem addr = memoryToVector mem V.! unA addr

memSet :: Address -> Int -> Memory -> Memory
memSet addr i mem = Memory $ memoryToVector mem V.// [(unA addr, i)]

initMemory :: Program -> Memory
initMemory = Memory . V.fromList

data Instr = Instr
  { iOp :: Op
  , iParams :: [Param]
  }
  deriving (Eq, Ord, Show)

data Op
  = Add
  | Multiply
  | Input
  | Output
  | JumpIfTrue
  | JumpIfFalse
  | LessThan
  | Equals
  | Halt
  deriving (Eq, Ord, Show)

readOp :: Int -> Op
readOp i = case i `mod` 100 of
  1 -> Add
  2 -> Multiply
  3 -> Input
  4 -> Output
  5 -> JumpIfTrue
  6 -> JumpIfFalse
  7 -> LessThan
  8 -> Equals
  99 -> Halt
  _ -> error $ "readOp: unknown op " <> show i

opParamCount :: Op -> Int
opParamCount op = case op of
  Add -> 3
  Multiply -> 3
  Input -> 1
  Output -> 1
  JumpIfTrue -> 2
  JumpIfFalse -> 2
  LessThan -> 3
  Equals -> 3
  Halt -> 0

data ParamMode
  = Position
  | Immediate
  deriving (Eq, Ord, Show)

digitToParamMode :: Int -> ParamMode
digitToParamMode i = case i of
  0 -> Position
  1 -> Immediate

-- >>> readParamModes 1002
-- [Position,Immediate]
readParamModes :: Int -> [ParamMode]
readParamModes opcode = loop (opcode `div` 100)
  where
    loop n
      | n <= 0 = []
      | otherwise = digitToParamMode (n `mod` 10) : loop (n `div` 10)

data Param = Param
  { pMode :: ParamMode
  , pValue :: Int
  }
  deriving (Eq, Ord, Show)

evalP :: Param -> Memory -> Int
evalP param mem = case pMode param of
  Position -> mem ! (A $ pValue param)
  Immediate -> pValue param

-- >>> readInstr 0 (initMemory [1002,4,3,4,33])
-- Instr {iOp = Multiply, iParams = [Param {pMode = Position, pValue = 4},Param {pMode = Immediate, pValue = 3},Param {pMode = Position, pValue = 4}]}
readInstr :: Address -> Memory -> Instr
readInstr ip mem = Instr
  { iOp = op
  , iParams = params
  }
  where
    opcode = mem ! ip
    op = readOp opcode
    paramCount = opParamCount op
    paramModes = readParamModes opcode
    paramInts = [mem ! (ip + offset) | offset <- [1..A paramCount]]
    params = zipWith Param (paramModes ++ repeat Position) paramInts

getOffset :: Instr -> Int
getOffset instr = 1 + length (iParams instr)

data ProgramState = ProgramState
  { psMemory :: Memory
  , psInstrPtr :: Address
  , psInputs :: [Int]
  , psOutputs :: [Int]
  }
  deriving (Eq, Show)

overMemory :: (Memory -> Memory) -> ProgramState -> ProgramState
overMemory f ps = ps { psMemory = f $ psMemory ps }

overInstrPtr :: (Address -> Address) -> ProgramState -> ProgramState
overInstrPtr f ps = ps { psInstrPtr = f $ psInstrPtr ps }

overInputs :: ([Int] -> [Int]) -> ProgramState -> ProgramState
overInputs f ps = ps { psInputs = f $ psInputs ps }

overOutputs :: ([Int] -> [Int]) -> ProgramState -> ProgramState
overOutputs f ps = ps { psOutputs = f $ psOutputs ps }

initProgram :: Program -> [Int] -> ProgramState
initProgram intcode inputs = ProgramState
  { psMemory = initMemory intcode
  , psInstrPtr = 0
  , psInputs = inputs
  , psOutputs = []
  }

type ProgramT m = ExceptT Interrupt (StateT ProgramState m)
type ProgramM = ProgramT Identity
type DebugM = ProgramT (Writer [ProgramState])

type Result a = (Either Interrupt a, ProgramState)
type DebugResult a = (Result a, [ProgramState])

data Interrupt
  = InterruptHalt
  | InterruptError String
  deriving (Eq, Show)

runProgramT :: ProgramT m a -> ProgramState -> m (Result a)
runProgramT ptm = runStateT (runExceptT ptm)

mapProgramT :: (m (Result a) -> n (Result b)) -> ProgramT m a -> ProgramT n b
mapProgramT f = mapExceptT (mapStateT f)

runProgramM :: ProgramM a -> ProgramState -> Result a
runProgramM pm = runIdentity . runProgramT pm

runDebugM :: DebugM a -> ProgramState -> DebugResult a
runDebugM dm = runWriter . runProgramT dm

-- |
-- Just halt
-- >>> runProgram [99] []
-- (Left InterruptHalt,ProgramState {psMemory = Memory {memoryToVector = [99]}, psInstrPtr = A 0, psInputs = [], psOutputs = []})
--
-- Multiply address 4 by 3, yielding 99 which then halts
-- >>> runProgram [1002,4,3,4,33] []
-- (Left InterruptHalt,ProgramState {psMemory = Memory {memoryToVector = [1002,4,3,4,99]}, psInstrPtr = A 4, psInputs = [], psOutputs = []})
--
-- Write the input to address 0 and output it
-- >>> runProgram [3,0,4,0,99] [55]
-- (Left InterruptHalt,ProgramState {psMemory = Memory {memoryToVector = [55,0,4,0,99]}, psInstrPtr = A 4, psInputs = [], psOutputs = [55]})
--
-- Output whether input equals 8
-- >>> runProgram [3,3,1108,-1,8,3,4,3,99] [5]
-- (Left InterruptHalt,ProgramState {psMemory = Memory {memoryToVector = [3,3,1108,0,8,3,4,3,99]}, psInstrPtr = A 8, psInputs = [], psOutputs = [0]})
--
-- Output whether input is non-zero
-- >>> runProgram [3,12,6,12,15,1,13,14,13,4,13,99,-1,0,1,9] [0]
-- (Left InterruptHalt,ProgramState {psMemory = Memory {memoryToVector = [3,12,6,12,15,1,13,14,13,4,13,99,0,0,1,9]}, psInstrPtr = A 11, psInputs = [], psOutputs = [0]})
--
-- Output 999 if input is less than 8, 1000 if equal, 1001 if greater
-- >>> runProgram [3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31,1106,0,36,98,0,0,1002,21,125,20,4,20,1105,1,46,104,999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99] [11]
-- (Left InterruptHalt,ProgramState {psMemory = Memory {memoryToVector = [3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31,1106,0,36,98,1001,11,1002,21,125,20,4,20,1105,1,46,104,999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99]}, psInstrPtr = A 46, psInputs = [], psOutputs = [1001]})
runProgram :: Program -> [Int] -> Result ()
runProgram intcode inputs =
  let state0 = initProgram intcode inputs
  in  runProgramM pmExec state0

debugProgram :: Program -> [Int] -> DebugResult ()
debugProgram intcode inputs =
  let state0 = initProgram intcode inputs
  in  runDebugM dmDebugExec state0

debugSteps :: Program -> [Int] -> IO (DebugResult ())
debugSteps intcode inputs = do
  let (result, states) = debugProgram intcode inputs
      prints =
        [ putStrLn $ showStep i t p
        | i <- [1..]
        | t <- map Just (tail states) ++ [Nothing]
        | p <- states
        ]
      printWaits = intersperse (void getLine) prints
  sequence_ printWaits
  pure (result, states)
  where
    showStep i mNextState thisState = unlines
      $  showThisStateLines i thisState
      ++ showDiffLines i mNextState thisState
    showThisStateLines i thisState =
      let thisInstr = readInstr (psInstrPtr thisState) (psMemory thisState)
          thisNextInput = listToMaybe $ psInputs thisState
      in [ intercalate ", "
           [ "Step " ++ show i
           , "IP: " ++ show (psInstrPtr thisState)
           , "Next input: " ++ show thisNextInput
           ]
         , showInstr (psMemory thisState) thisInstr
         ]
    showDiffLines i mNextState thisState = case mNextState of
      Nothing -> []
      Just nextState ->
        let memDiffs = memoryDiffs (psMemory thisState) (psMemory nextState)
        in [ "Outputs: " ++ show (psOutputs nextState)
           , "MemDiff: " ++ showMemDiffs memDiffs
           ]
    showMemDiff (i, old, new) =
      "m[" ++ show i ++ "]: " ++ show old ++ " -> " ++ show new
    showMemDiffs mds =
      "[" ++ intercalate ", " (map showMemDiff mds) ++ "]"

showInstr :: Memory -> Instr -> String
showInstr mem instr = unwords
  $ ("{" ++ show (iOp instr) ++ "}")
  : map (showParam mem) (iParams instr)

showParam :: Memory -> Param -> String
showParam mem param =
  let value = pValue param
  in  case pMode param of
    Position -> "m[" ++ show value ++ "]=" ++ show (mem ! A value)
    Immediate -> show value

memoryDiffs :: Memory -> Memory -> [(Int, Int, Int)]
memoryDiffs oldMem newMem =
  filter different $ zip3 [0..] olds news
  where
    different (_, old, new) = old /= new
    olds = V.toList $ memoryToVector oldMem
    news = V.toList $ memoryToVector newMem

dmDebugExec :: DebugM ()
dmDebugExec = sequence_ (repeat dmDebugExecCurrent)

dmDebugExecCurrent :: DebugM ()
dmDebugExecCurrent = do
  ps <- get
  tell [ps]
  mapProgramT lift pmExecCurrent

pmSet :: Address -> Int -> ProgramM ()
pmSet addr i = modify' $ overMemory $ memSet addr i

pmGet :: Address -> ProgramM Int
pmGet addr = gets ((! addr) . psMemory)

pmEvalP :: Param -> ProgramM Int
pmEvalP param = gets (evalP param . psMemory)

pmPeekInput :: ProgramM Int
pmPeekInput = gets (head . psInputs)

pmPopInput :: ProgramM Int
pmPopInput = do
  i <- pmPeekInput
  modify' $ overInputs tail
  pure i

pmPushOutput :: Int -> ProgramM ()
pmPushOutput i = modify' $ overOutputs (i :)

pmHalt :: ProgramM ()
pmHalt = throwError InterruptHalt

pmInstr :: ProgramM Instr
pmInstr = do
  ip <- gets psInstrPtr
  mem <- gets psMemory
  pure (readInstr ip mem)

pmAdvance :: Int -> ProgramM ()
pmAdvance offset = modify' $ overInstrPtr (+ A offset)

pmJump :: Address -> ProgramM ()
pmJump addr = modify' $ overInstrPtr (const addr)

pmExec :: ProgramM ()
pmExec = sequence_ (repeat pmExecCurrent)

pmExecN :: Int -> ProgramM ()
pmExecN n = replicateM_ n pmExecCurrent

pmExecCurrent :: ProgramM ()
pmExecCurrent = do
  instr <- pmInstr
  didJump <- pmApplyInstr instr
  when (not didJump) $
    pmAdvance (getOffset instr)

pmApplyInstr :: Instr -> ProgramM Bool -- returns whether a jump occurred
pmApplyInstr instr =
  case (iOp instr, iParams instr) of
    (Add, [pi0, pi1, pout]) -> do
      i0 <- pmEvalP pi0
      i1 <- pmEvalP pi1
      let addr = A $ pValue pout
      pmSet addr (i0 + i1)
      pure False
    (Multiply, [pi0, pi1, pout]) -> do
      i0 <- pmEvalP pi0
      i1 <- pmEvalP pi1
      let addr = A $ pValue pout
      pmSet addr (i0 * i1)
      pure False
    (Input, [pout]) -> do
      i <- pmPopInput
      let addr = A $ pValue pout
      pmSet addr i
      pure False
    (Output, [pi0]) -> do
      i0 <- pmEvalP pi0
      pmPushOutput i0
      pure False
    (JumpIfTrue, [pi0, pi1]) -> do
      i0 <- pmEvalP pi0
      i1 <- pmEvalP pi1
      let jump = i0 /= 0
      when jump $
        pmJump (A i1)
      pure jump
    (JumpIfFalse, [pi0, pi1]) -> do
      i0 <- pmEvalP pi0
      i1 <- pmEvalP pi1
      let jump = i0 == 0
      when jump $
        pmJump (A i1)
      pure jump
    (LessThan, [pi0, pi1, pout]) -> do
      i0 <- pmEvalP pi0
      i1 <- pmEvalP pi1
      let addr = A $ pValue pout
          value = if i0 < i1 then 1 else 0
      pmSet addr value
      pure False
    (Equals, [pi0, pi1, pout]) -> do
      i0 <- pmEvalP pi0
      i1 <- pmEvalP pi1
      let addr = A $ pValue pout
          value = if i0 == i1 then 1 else 0
      pmSet addr value
      pure False
    (Halt, []) -> do
      pmHalt
      pure False
