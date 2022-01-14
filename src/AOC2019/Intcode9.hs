{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ParallelListComp #-}
{-# LANGUAGE TupleSections #-}
module AOC2019.Intcode9 where

import Control.Monad.Except
import Control.Monad.Identity
import Control.Monad.State
import Control.Monad.Writer
import Data.Bifunctor
import Data.List
import Data.Maybe
import qualified Data.Map.Merge.Strict as Map
import qualified Data.Map.Strict as Map
import qualified Data.Vector as V

type Program = [Integer]

newtype Address = A Integer
  deriving (Eq, Ord, Num, Enum, Show)

unA :: Address -> Integer
unA (A n) = n

data Memory = Memory
  { programMemory :: V.Vector Integer
  , extendedMemory :: Map.Map Integer Integer
  }
  deriving (Eq, Ord, Show)

overProgramMemory
  :: (V.Vector Integer -> V.Vector Integer)
  -> Memory -> Memory
overProgramMemory f mem = mem { programMemory = f (programMemory mem) }

overExtendedMemory
  :: (Map.Map Integer Integer -> Map.Map Integer Integer)
  -> Memory -> Memory
overExtendedMemory f mem = mem { extendedMemory = f (extendedMemory mem) }

(!) :: Memory -> Address -> Either String Integer
(!) mem (A n)
  | n < 0 =
    Left $ "(!): negative address " <> show (A n)
  | n < fromIntegral (V.length (programMemory mem)) =
    Right $ programMemory mem V.! (fromIntegral n)
  | otherwise =
    Right $ fromMaybe 0 $ Map.lookup n (extendedMemory mem)

memSet :: Address -> Integer -> Memory -> Either String Memory
memSet (A n) i mem
  | n < 0 =
    Left $ "memSet: negative address " <> show (A n)
  | n < fromIntegral (V.length (programMemory mem)) =
    Right $ overProgramMemory (V.// [(fromIntegral n, i)]) mem
  | otherwise =
    Right $ overExtendedMemory (Map.insert n i) mem

initMemory :: Program -> Memory
initMemory intcode = Memory
  { programMemory = V.fromList intcode
  , extendedMemory = Map.empty
  }

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
  | RelBaseOffset
  | Halt
  deriving (Eq, Ord, Show)

readOp :: Integer -> Either String Op
readOp i = case i `mod` 100 of
  1 -> Right Add
  2 -> Right Multiply
  3 -> Right Input
  4 -> Right Output
  5 -> Right JumpIfTrue
  6 -> Right JumpIfFalse
  7 -> Right LessThan
  8 -> Right Equals
  9 -> Right RelBaseOffset
  99 -> Right Halt
  _ -> Left $ "readOp: unknown op " <> show i

opParamCount :: Op -> Integer
opParamCount op = case op of
  Add -> 3
  Multiply -> 3
  Input -> 1
  Output -> 1
  JumpIfTrue -> 2
  JumpIfFalse -> 2
  LessThan -> 3
  Equals -> 3
  RelBaseOffset -> 1
  Halt -> 0

data ParamMode
  = Position
  | Immediate
  | Relative
  deriving (Eq, Ord, Show)

digitToParamMode :: Integer -> Either String ParamMode
digitToParamMode i = case i of
  0 -> Right Position
  1 -> Right Immediate
  2 -> Right Relative
  _ -> Left $ "digitToParamMode: unknown param mode " <> show i

-- |
-- >>> readParamModes 1002
-- [Position,Immediate]
readParamModes :: Integer -> Either String [ParamMode]
readParamModes opcode = loop (opcode `div` 100)
  where
    loop n
      | n <= 0 = Right []
      | otherwise = do
          pm <- digitToParamMode (n `mod` 10)
          pms <- loop (n `div` 10)
          pure $ pm : pms

data Param = Param
  { pMode :: ParamMode
  , pValue :: Integer
  }
  deriving (Eq, Ord, Show)

evalP :: Param -> Memory -> Address -> Either String Integer
evalP param mem relBase = case pMode param of
  Position -> mem ! (A $ pValue param)
  Immediate -> Right $ pValue param
  Relative -> mem ! (relBase + A (pValue param))

addressP :: Param -> Memory -> Address -> Maybe Address
addressP param mem relBase = case pMode param of
  Position -> Just $ A $ pValue param
  Immediate -> Nothing
  Relative -> Just $ relBase + A (pValue param)

-- |
-- >>> readInstr 0 (initMemory [1002,4,3,4,33])
-- Instr {iOp = Multiply, iParams = [Param {pMode = Position, pValue = 4},Param {pMode = Immediate, pValue = 3},Param {pMode = Position, pValue = 4}]}
readInstr :: Address -> Memory -> Either String Instr
readInstr ip mem = do
  opcode <- mem ! ip
  op <- readOp opcode
  let paramCount = opParamCount op
  paramModes <- readParamModes opcode
  paramInts <- sequenceA [mem ! (ip + offset) | offset <- [1..A paramCount]]
  let params = zipWith Param (paramModes ++ repeat Position) paramInts
  pure Instr
    { iOp = op
    , iParams = params
    }

getOffset :: Instr -> Integer
getOffset instr = 1 + fromIntegral (length (iParams instr))

data ProgramState = ProgramState
  { psMemory :: Memory
  , psInstrPtr :: Address -- ^ instruction pointer
  , psRelBase :: Address -- ^ relative base
  , psInputs :: [Integer]
  , psOutputs :: [Integer]
  }
  deriving (Eq, Show)

overMemory :: (Memory -> Memory) -> ProgramState -> ProgramState
overMemory f ps = ps { psMemory = f $ psMemory ps }

overInstrPtr :: (Address -> Address) -> ProgramState -> ProgramState
overInstrPtr f ps = ps { psInstrPtr = f $ psInstrPtr ps }

overRelBase :: (Address -> Address) -> ProgramState -> ProgramState
overRelBase f ps = ps { psRelBase = f $ psRelBase ps }

overInputs :: ([Integer] -> [Integer]) -> ProgramState -> ProgramState
overInputs f ps = ps { psInputs = f $ psInputs ps }

overOutputs :: ([Integer] -> [Integer]) -> ProgramState -> ProgramState
overOutputs f ps = ps { psOutputs = f $ psOutputs ps }

initProgram :: Program -> [Integer] -> ProgramState
initProgram intcode inputs = ProgramState
  { psMemory = initMemory intcode
  , psInstrPtr = 0
  , psRelBase = 0
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
runProgram :: Program -> [Integer] -> Result ()
runProgram intcode inputs =
  let state0 = initProgram intcode inputs
  in  runProgramM pmExec state0

debugProgram :: Program -> [Integer] -> DebugResult ()
debugProgram intcode inputs =
  let state0 = initProgram intcode inputs
  in  runDebugM dmDebugExec state0

debugSteps :: Program -> [Integer] -> IO (DebugResult ())
debugSteps intcode inputs = do
  let (result, states) = debugProgram intcode inputs
      prints =
        [ putStrLn $ showStepError i t p
        | i <- [1..]
        | t <- map Just (tail states) ++ [Nothing]
        | p <- states
        ]
      printWaits = intersperse (void getLine) prints
  sequence_ printWaits
  pure (result, states)
  where
    showStepError i mNextState thisState =
      case showStep i mNextState thisState of
        Left err -> show err
        Right str -> str
    showStep i mNextState thisState = do
      stateLinesStr <- showThisStateLines i thisState
      pure $ unlines (stateLinesStr ++ showDiffLines i mNextState thisState)
    showThisStateLines i thisState = do
      thisInstr <- readInstr (psInstrPtr thisState) (psMemory thisState)
      instrStr <- showInstr
        (psMemory thisState)
        (psRelBase thisState)
        thisInstr
      let thisNextInput = listToMaybe $ psInputs thisState
      pure [ intercalate ", "
             [ "Step " ++ show i
             , "IP: " ++ show (psInstrPtr thisState)
             , "RB: " ++ show (psRelBase thisState)
             , "Next input: " ++ show thisNextInput
             ]
           , instrStr
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

showInstr :: Memory -> Address -> Instr -> Either String String
showInstr mem relBase instr = fmap unwords . sequenceA
  $ (Right $ "{" ++ show (iOp instr) ++ "}")
  : map (showParam mem relBase) (iParams instr)

showParam :: Memory -> Address -> Param -> Either String String
showParam mem relBase param =
  let value = pValue param
  in  case pMode param of
    Position -> do
      stored <- mem ! A value
      pure $ "m[" ++ show value ++ "]=" ++ show stored
    Immediate -> Right $ show value
    Relative -> do
      stored <- mem ! (relBase + A value)
      pure $ "m[" ++ show relBase ++ "+" ++ show value ++ "]=" ++ show stored

memoryDiffs :: Memory -> Memory -> [(Integer, Integer, Integer)]
memoryDiffs oldMem newMem
  =  programMemoryDiffs (programMemory oldMem) (programMemory newMem)
  ++ extendedMemoryDiffs (extendedMemory oldMem) (extendedMemory newMem)

programMemoryDiffs
  :: V.Vector Integer
  -> V.Vector Integer
  -> [(Integer, Integer, Integer)]
programMemoryDiffs oldMem newMem =
  filter different $ zip3 [0..] olds news
  where
    different (_, old, new) = old /= new
    olds = V.toList oldMem
    news = V.toList newMem

extendedMemoryDiffs
  :: Map.Map Integer Integer
  -> Map.Map Integer Integer
  -> [(Integer, Integer, Integer)]
extendedMemoryDiffs olds news =
  Map.elems . Map.filter different $ Map.merge
    (Map.mapMissing $ \k a -> (k, a, 0))
    (Map.mapMissing $ \k b -> (k, 0, b))
    (Map.zipWithMatched $ \k a b -> (k, a, b))
    olds news
  where
    different (_, old, new) = old /= new

dmDebugExec :: DebugM ()
dmDebugExec = sequence_ (repeat dmDebugExecCurrent)

dmDebugExecCurrent :: DebugM ()
dmDebugExecCurrent = do
  ps <- get
  tell [ps]
  mapProgramT lift pmExecCurrent

liftInterrupt :: String -> Either String a -> ProgramM a
liftInterrupt prefix = liftEither . first (InterruptError . (prefix <>))

pmSet :: Address -> Integer -> ProgramM ()
pmSet addr i = do
  mem <- gets psMemory
  mem' <- liftInterrupt "pmSet: " $ memSet addr i mem
  modify' $ overMemory (const mem')

pmGet :: Address -> ProgramM Integer
pmGet addr = do
  ei <- gets ((! addr) . psMemory)
  liftInterrupt "pmGet: " ei

pmEvalP :: Param -> ProgramM Integer
pmEvalP param = do
  ei <- gets (\ps -> evalP param (psMemory ps) (psRelBase ps))
  liftInterrupt "pmEvalP: " ei

pmAddressP :: Param -> ProgramM Address
pmAddressP param = do
  mAddr <- gets (\ps -> addressP param (psMemory ps) (psRelBase ps))
  case mAddr of
    Nothing -> throwError (InterruptError "pmAddressP: no address")
    Just addr -> pure addr

pmPeekInput :: ProgramM (Maybe Integer)
pmPeekInput = gets (listToMaybe . psInputs)

pmPopInput :: ProgramM Integer
pmPopInput = do
  inputs <- gets psInputs
  case inputs of
    [] -> throwError (InterruptError "pmPopInput: input unavailable")
    i:inputs' -> do
      modify' $ overInputs (const inputs')
      pure i

-- | Adds an input to the end of the input queue, to be consumed after all
-- current inputs are consumed.
pmQueueInput :: Integer -> ProgramM ()
pmQueueInput i = modify' $ overInputs (++ [i])

pmPushOutput :: Integer -> ProgramM ()
pmPushOutput i = modify' $ overOutputs (i :)

pmHalt :: ProgramM ()
pmHalt = throwError InterruptHalt

pmInstr :: ProgramM Instr
pmInstr = do
  ip <- gets psInstrPtr
  mem <- gets psMemory
  liftInterrupt "pmInstr: " $ readInstr ip mem

pmAdvance :: Integer -> ProgramM ()
pmAdvance offset = modify' $ overInstrPtr (+ A offset)

pmJump :: Address -> ProgramM ()
pmJump addr = modify' $ overInstrPtr (const addr)

pmOffsetRelBase :: Integer -> ProgramM ()
pmOffsetRelBase offset = modify' $ overRelBase (+ A offset)

pmExecUntilOutput :: ProgramM Integer
pmExecUntilOutput = do
  instr <- pmInstr
  pmExecCurrent
  case iOp instr of
    Output -> gets (head . psOutputs)
    _ -> pmExecUntilOutput

pmExec :: ProgramM ()
pmExec = sequence_ (repeat pmExecCurrent)

pmExecN :: Int -> ProgramM ()
pmExecN n = replicateM_ n pmExecCurrent

pmExecCurrent :: ProgramM ()
pmExecCurrent = do
  instr <- pmInstr
  didJump <- pmApplyInstr instr
  unless didJump $
    pmAdvance (getOffset instr)

pmApplyInstr :: Instr -> ProgramM Bool -- returns whether a jump occurred
pmApplyInstr instr =
  case (iOp instr, iParams instr) of
    (Add, [pi0, pi1, pout]) -> do
      i0 <- pmEvalP pi0
      i1 <- pmEvalP pi1
      addr <- pmAddressP pout
      pmSet addr (i0 + i1)
      pure False
    (Multiply, [pi0, pi1, pout]) -> do
      i0 <- pmEvalP pi0
      i1 <- pmEvalP pi1
      addr <- pmAddressP pout
      pmSet addr (i0 * i1)
      pure False
    (Input, [pout]) -> do
      i <- pmPopInput
      addr <- pmAddressP pout
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
      addr <- pmAddressP pout
      let value = if i0 < i1 then 1 else 0
      pmSet addr value
      pure False
    (Equals, [pi0, pi1, pout]) -> do
      i0 <- pmEvalP pi0
      i1 <- pmEvalP pi1
      addr <- pmAddressP pout
      let value = if i0 == i1 then 1 else 0
      pmSet addr value
      pure False
    (RelBaseOffset, [pi0]) -> do
      i0 <- pmEvalP pi0
      pmOffsetRelBase i0
      pure False
    (Halt, []) -> do
      pmHalt
      pure False
