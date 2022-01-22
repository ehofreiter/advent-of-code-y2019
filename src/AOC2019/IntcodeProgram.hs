{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module AOC2019.IntcodeProgram where

import Data.Maybe
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
    Right $ programMemory mem V.! fromIntegral n
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
-- Right [Position,Immediate]
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
-- Right (Instr {iOp = Multiply, iParams = [Param {pMode = Position, pValue = 4},Param {pMode = Immediate, pValue = 3},Param {pMode = Position, pValue = 4}]})
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
