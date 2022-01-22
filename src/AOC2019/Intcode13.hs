{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module AOC2019.Intcode13
  ( module AOC2019.Intcode13
  , module AOC2019.IntcodeProgram
  ) where

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

import AOC2019.IntcodeProgram

data ProgramState m = ProgramState
  { psInternal :: InternalState
  , psReadInput :: m Integer
  , psWriteOutput :: Integer -> m ()
  }

instance Show (ProgramState m) where
  show ps
    = "ProgramState "
    <> "{ psInternal = " <> show (psInternal ps)
    <> ", psReadInput = <m Integer>"
    <> ", psWriteOutput = <Integer -> m ()>"

overInternal
  :: (InternalState -> InternalState)
  -> ProgramState m -> ProgramState m
overInternal f ps = ps { psInternal = f $ psInternal ps }

overReadInput
  :: (m Integer -> m Integer)
  -> ProgramState m -> ProgramState m
overReadInput f ps = ps { psReadInput = f $ psReadInput ps }

data InternalState = InternalState
  { inMemory :: Memory
  , inInstrPtr :: Address -- ^ instruction pointer
  , inRelBase :: Address -- ^ relative base
  }
  deriving (Eq, Show)

overMemory :: (Memory -> Memory) -> InternalState -> InternalState
overMemory f ins = ins { inMemory = f $ inMemory ins }

overInstrPtr :: (Address -> Address) -> InternalState -> InternalState
overInstrPtr f ins = ins { inInstrPtr = f $ inInstrPtr ins }

overRelBase :: (Address -> Address) -> InternalState -> InternalState
overRelBase f ins = ins { inRelBase = f $ inRelBase ins }

psMemory :: ProgramState m -> Memory
psMemory = inMemory . psInternal

psInstrPtr :: ProgramState m -> Address
psInstrPtr = inInstrPtr . psInternal

psRelBase :: ProgramState m -> Address
psRelBase = inRelBase . psInternal

initProgram :: Program -> m Integer -> (Integer -> m ()) -> ProgramState m
initProgram intcode readInput writeOutput = ProgramState
  { psInternal = InternalState
    { inMemory = initMemory intcode
    , inInstrPtr = 0
    , inRelBase = 0
    }
  , psReadInput = readInput
  , psWriteOutput = writeOutput
  }

data Interrupt
  = InterruptHalt
  | InterruptError String
  deriving (Eq, Show)

liftInterrupt :: Monad m => String -> Either String a -> ProgramT m a
liftInterrupt prefix = liftEither . first (InterruptError . (prefix <>))

newtype ProgramT m a = ProgramT
  { unProgramT :: ExceptT Interrupt (StateT (ProgramState m) m) a
  }
  deriving
    ( Functor, Applicative, Monad
    , MonadState (ProgramState m)
    , MonadError Interrupt
    )

instance MonadTrans ProgramT where
  lift = ProgramT . lift . lift

runProgramT :: ProgramT m a -> ProgramState m -> m (Result m a)
runProgramT pt = runStateT (runExceptT (unProgramT pt))

interactProgram
  :: MonadWriter [Integer] m
  => Program -> ([Integer] -> m Integer)
  -> m (Result m (), [Integer])
interactProgram intcode outputInput = loop ps0
  where
    ps0 = initProgram intcode (outputInput []) (\i -> tell [i])
    loop ps = do
      let program = executeCurrent >> executeUntilInput
      ((value, ps'), outputs) <- listen $ runProgramT program ps
      case value of
        Left interrupt -> pure ((Left interrupt, ps'), outputs)
        Right _ -> loop ps' { psReadInput = outputInput outputs }

type Result m a = (Either Interrupt a, ProgramState m)

class Monad m => MonadProgram m where
  executeCurrent :: m (Maybe Integer)
  executeUntilInput :: m [Integer]
  execute :: m ()
  execute = sequence_ (repeat executeCurrent)

instance Monad m => MonadProgram (ProgramT m) where
  executeCurrent = pmExecCurrent
  executeUntilInput = pmExecUntilOp Input

executeUntilOutput :: MonadProgram m => m Integer
executeUntilOutput = do
  mOutput <- executeCurrent
  case mOutput of
    Nothing -> executeUntilOutput
    Just output -> pure output

executeN :: MonadProgram m => Int -> m ()
executeN n = replicateM_ n executeCurrent

pmExecCurrent :: Monad m => ProgramT m (Maybe Integer)
pmExecCurrent = do
  instr <- pmInstr
  result <- pmApplyInstr instr
  unless (didJump result) $
    pmAdvance (getOffset instr)
  pure (getOutput result)

pmExecUntilOp :: Monad m => Op -> ProgramT m [Integer]
pmExecUntilOp op = loop []
  where
    loop outputs = do
      instr <- pmInstr
      if iOp instr == op
        then pure (reverse $ catMaybes outputs)
        else do
          output <- pmExecCurrent
          loop (output : outputs)

pmSet :: Monad m => Address -> Integer -> ProgramT m ()
pmSet addr i = do
  mem <- gets psMemory
  mem' <- liftInterrupt "pmSet: " $ memSet addr i mem
  modify' $ overInternal $ overMemory (const mem')

pmGet :: Monad m => Address -> ProgramT m Integer
pmGet addr = do
  ei <- gets ((! addr) . psMemory)
  liftInterrupt "pmGet: " ei

pmEvalP :: Monad m => Param -> ProgramT m Integer
pmEvalP param = do
  ei <- gets (\ps -> evalP param (psMemory ps) (psRelBase ps))
  liftInterrupt "pmEvalP: " ei

pmAddressP :: Monad m => Param -> ProgramT m Address
pmAddressP param = do
  mAddr <- gets (\ps -> addressP param (psMemory ps) (psRelBase ps))
  case mAddr of
    Nothing -> throwError (InterruptError "pmAddressP: no address")
    Just addr -> pure addr

pmHalt :: Monad m => ProgramT m ()
pmHalt = throwError InterruptHalt

pmInstr :: Monad m => ProgramT m Instr
pmInstr = do
  ip <- gets (inInstrPtr . psInternal)
  mem <- gets (inMemory . psInternal)
  liftInterrupt "pmInstr: " $ readInstr ip mem

pmAdvance :: Monad m => Integer -> ProgramT m ()
pmAdvance offset = modify' $ overInternal $ overInstrPtr (+ A offset)

pmJump :: Monad m => Address -> ProgramT m ()
pmJump addr = modify' $ overInternal $ overInstrPtr (const addr)

pmOffsetRelBase :: Monad m => Integer -> ProgramT m ()
pmOffsetRelBase offset = modify' $ overInternal $ overRelBase (+ A offset)

pmReadInput :: Monad m => ProgramT m Integer
pmReadInput = do
  readInput <- gets psReadInput
  lift readInput

pmWriteOutput :: Monad m => Integer -> ProgramT m ()
pmWriteOutput i = do
  writeOutput <- gets psWriteOutput
  lift (writeOutput i)

data InstrResult
  = Jumped
  | Out Integer
  | Empty

didJump :: InstrResult -> Bool
didJump ir = case ir of
  Jumped -> True
  _ -> False

getOutput :: InstrResult -> Maybe Integer
getOutput ir = case ir of
  Out i -> Just i
  _ -> Nothing

pmApplyInstr :: Monad m => Instr -> ProgramT m InstrResult
pmApplyInstr instr =
  case (iOp instr, iParams instr) of
    (Add, [pi0, pi1, pout]) -> do
      i0 <- pmEvalP pi0
      i1 <- pmEvalP pi1
      addr <- pmAddressP pout
      pmSet addr (i0 + i1)
      pure Empty
    (Multiply, [pi0, pi1, pout]) -> do
      i0 <- pmEvalP pi0
      i1 <- pmEvalP pi1
      addr <- pmAddressP pout
      pmSet addr (i0 * i1)
      pure Empty
    (Input, [pout]) -> do
      i <- pmReadInput
      addr <- pmAddressP pout
      pmSet addr i
      pure Empty
    (Output, [pi0]) -> do
      i0 <- pmEvalP pi0
      pmWriteOutput i0
      pure (Out i0)
    (JumpIfTrue, [pi0, pi1]) -> do
      i0 <- pmEvalP pi0
      i1 <- pmEvalP pi1
      let jump = i0 /= 0
      when jump $
        pmJump (A i1)
      pure (if jump then Jumped else Empty)
    (JumpIfFalse, [pi0, pi1]) -> do
      i0 <- pmEvalP pi0
      i1 <- pmEvalP pi1
      let jump = i0 == 0
      when jump $
        pmJump (A i1)
      pure (if jump then Jumped else Empty)
    (LessThan, [pi0, pi1, pout]) -> do
      i0 <- pmEvalP pi0
      i1 <- pmEvalP pi1
      addr <- pmAddressP pout
      let value = if i0 < i1 then 1 else 0
      pmSet addr value
      pure Empty
    (Equals, [pi0, pi1, pout]) -> do
      i0 <- pmEvalP pi0
      i1 <- pmEvalP pi1
      addr <- pmAddressP pout
      let value = if i0 == i1 then 1 else 0
      pmSet addr value
      pure Empty
    (RelBaseOffset, [pi0]) -> do
      i0 <- pmEvalP pi0
      pmOffsetRelBase i0
      pure Empty
    (Halt, []) -> do
      pmHalt
      pure Empty
