{-# LANGUAGE ScopedTypeVariables #-}
module AOC2019.InOut where

import Prelude hiding (interact)
import qualified Control.Monad.Free as MF
import Control.Monad.State

-- | Attempt to make a better interface for the Intcode computer by
-- translating an Intcode program into a sequence of input/output operations.
-- These operations will be filled in by the caller. Here the concept is
-- is to construct the sequence of operations as a free monad.
-- Another concept to be considered is using stream processing libraries
-- like conduit or pipes.
data InOutF k
  = In (Int -> k)
  | Out Int k

-- Could use DeriveFunctor extension instead.
instance Functor InOutF where
  fmap f (In g) = In (f . g)
  fmap f (Out i k) = Out i (f k)

type InOut = MF.Free InOutF

readIn :: InOut Int
readIn = MF.liftF $ In id

writeOut :: Int -> InOut ()
writeOut i = MF.liftF $ Out i ()

prog :: InOut ()
prog = do
  mapM_ writeOut [1..10]
  i <- readIn
  writeOut i

interpIO :: InOut a -> IO a
interpIO inOut = case inOut of
  MF.Pure x -> pure x
  MF.Free (In g) -> readLn >>= interpIO . g
  MF.Free (Out i k) -> print i >> interpIO k

interact
  :: forall a m. Monad m
  => ([Int] -> m Int) -> InOut a -> m (a, [Int])
interact f inOut = runStateT (loop inOut) []
  where
    loop :: InOut a -> StateT [Int] m a
    loop inOut = case inOut of
      MF.Pure x -> pure x
      MF.Free (In g) -> do
        outs <- get
        put []
        input <- lift $ f outs
        loop $ g input
      MF.Free (Out i k) -> do
        modify (++ [i])
        loop k

printSum :: [Int] -> IO Int
printSum ints = do
  putStrLn $ "sum of " <> show ints <> ": " <> show (sum ints)
  pure (sum ints)
