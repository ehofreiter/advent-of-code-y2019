{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module AOC2019.Intcode13
  ( module AOC2019.Intcode13
  , module AOC2019.IntcodeProgram
  ) where

import Control.Monad.Except
import Control.Monad.Identity
import Control.Monad.RWS
import Data.Bifunctor
import Data.List
import Data.Maybe
import qualified Data.Map.Merge.Strict as Map
import qualified Data.Map.Strict as Map
import qualified Data.Vector as V

import AOC2019.IntcodeProgram

type Intcode = Program

type Env m = m Integer

type Outputs = [Integer]

data ProgramState = ProgramState
  { psMemory :: Memory
  , psInstrPtr :: Address -- ^ instruction pointer
  , psRelBase :: Address -- ^ relative base
  }
  deriving (Eq, Show)

overMemory :: (Memory -> Memory) -> ProgramState -> ProgramState
overMemory f ins = ins { psMemory = f $ psMemory ins }

overInstrPtr :: (Address -> Address) -> ProgramState -> ProgramState
overInstrPtr f ins = ins { psInstrPtr = f $ psInstrPtr ins }

overRelBase :: (Address -> Address) -> ProgramState -> ProgramState
overRelBase f ins = ins { psRelBase = f $ psRelBase ins }

initProgram :: Intcode -> ProgramState
initProgram intcode = ProgramState
  { psMemory = initMemory intcode
  , psInstrPtr = 0
  , psRelBase = 0
  }

data Interrupt
  = InterruptHalt
  | InterruptError String
  deriving (Eq, Show)

liftInterrupt :: Monad m => String -> Either String a -> ProgramT m a
liftInterrupt prefix = liftEither . first (InterruptError . (prefix <>))

newtype ProgramT m a = ProgramT
  { unProgramT
    :: ExceptT Interrupt (RWST (Env m) Outputs ProgramState m) a
  }
  deriving
    ( Functor, Applicative, Monad
    , MonadError Interrupt
    , MonadReader (Env m)
    , MonadWriter Outputs
    , MonadState ProgramState
    , MonadRWS (Env m) Outputs ProgramState
    )

instance MonadTrans ProgramT where
  lift = ProgramT . lift . lift

runProgramT :: ProgramT m a -> Env m -> ProgramState -> m (Result a)
runProgramT pt = runRWST (runExceptT (unProgramT pt))

type Result a = (Either Interrupt a, ProgramState, Outputs)

type ProgramM = ProgramT Identity

runProgramM :: ProgramM a -> Integer -> Intcode -> Result a
runProgramM pt i ic = runIdentity $ runProgramT pt (pure i) (initProgram ic)

class Monad m => MonadProgram m where
  executeCurrent :: m ()
  executeUntilOp :: Op -> m ()
  execute :: m ()
  execute = sequence_ (repeat executeCurrent)

instance Monad m => MonadProgram (ProgramT m) where
  executeCurrent = pmExecCurrent
  executeUntilOp = pmExecUntilOp

executeThroughOp :: MonadProgram m => Op -> m ()
executeThroughOp op = do
  executeUntilOp op
  executeCurrent

executeN :: MonadProgram m => Int -> m ()
executeN n = replicateM_ n executeCurrent

-- | Runs the program interactively by collecting outputs until the next
-- input instruction, then passing the outputs to the given function to
-- provide the input value. This is repeated until the program halts. Each
-- input instruction only gets the outputs since the last input instruction.
-- The final output from the Writer is the outputs since the last input
-- instruction.
interactProgram
  :: (MonadReader (Env n) m, MonadWriter Outputs m, MonadProgram m)
  => ([Integer] -> n Integer)
  -> m ()
interactProgram inFromOut = do
  (_, outputs) <- censor (const []) $ listen $ executeUntilOp Input
  local (const $ inFromOut outputs) executeCurrent
  interactProgram inFromOut

pmExecCurrent :: Monad m => ProgramT m ()
pmExecCurrent = do
  instr <- pmInstr
  didJump <- pmApplyInstr instr
  unless didJump $
    pmAdvance (getOffset instr)

pmExecUntilOp :: Monad m => Op -> ProgramT m ()
pmExecUntilOp op = do
  instr <- pmInstr
  if iOp instr == op
    then pure ()
    else do
      pmExecCurrent
      pmExecUntilOp op

pmSet :: Monad m => Address -> Integer -> ProgramT m ()
pmSet addr i = do
  mem <- gets psMemory
  mem' <- liftInterrupt "pmSet: " $ memSet addr i mem
  modify' $ overMemory (const mem')

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
  ip <- gets psInstrPtr
  mem <- gets psMemory
  liftInterrupt "pmInstr: " $ readInstr ip mem

pmAdvance :: Monad m => Integer -> ProgramT m ()
pmAdvance offset = modify' $ overInstrPtr (+ A offset)

pmJump :: Monad m => Address -> ProgramT m ()
pmJump addr = modify' $ overInstrPtr (const addr)

pmOffsetRelBase :: Monad m => Integer -> ProgramT m ()
pmOffsetRelBase offset = modify' $ overRelBase (+ A offset)

pmReadInput :: Monad m => ProgramT m Integer
pmReadInput = do
  readInput <- ask
  lift readInput

pmWriteOutput :: Monad m => Integer -> ProgramT m ()
pmWriteOutput i = tell [i]

pmApplyInstr :: Monad m => Instr -> ProgramT m Bool -- whether IP jumped
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
      i <- pmReadInput
      addr <- pmAddressP pout
      pmSet addr i
      pure False
    (Output, [pi0]) -> do
      i0 <- pmEvalP pi0
      pmWriteOutput i0
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
