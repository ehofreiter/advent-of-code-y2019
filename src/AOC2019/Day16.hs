{-# LANGUAGE TupleSections #-}
module AOC2019.Day16 where

import Control.Applicative
import Control.Monad
import Control.Monad.Identity
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
import qualified Data.Vector.Mutable as MV
import Linear.V2
import Linear.Vector
import qualified Text.Parsec as P

import AOC2019.Common

dataPath = "data/day16/"
runF = dataPath ++ "input.txt"
--testF = dataPath ++ "test.txt"

runI = load runF
--testI = load testF

run = runI >>= exec
--test = testI >>= exec

load :: FilePath -> IO (V.Vector Int)
load filePath = do
  strs <- loadFile filePath
  pure (parseInput $ head strs)

parseInput :: String -> V.Vector Int
parseInput = V.fromList . map (read . (: []))

exec :: V.Vector Int -> IO ()
exec inputs = do
  --print inputs
  --print (part1 inputs)
  s <- part2 inputs
  print s

-- Part 2
-- Each digit only depends on the digits after it every iteration.
-- We care about an 8-digit message that is offset several million,
-- ie towards the end of the message. Past the halfway point, the
-- computation is just add all the numbers to the end of the list.
-- Seems that in the input the message past the halfway point.

part2 :: V.Vector Int -> IO String
part2 v = do
  mv <- V.thaw (reverseMessageTail v)
  fft2MN mv 100
  v' <- V.freeze mv
  pure (extractFinalMessage v')

extractFinalMessage :: V.Vector Int -> String
extractFinalMessage v
  = concatMap show
  . reverse
  . V.toList
  $ V.slice (V.length v - 8) 8 v

fft2MN :: MV.IOVector Int -> Int -> IO ()
fft2MN v n = replicateM_ n (fft2M v)

fft2M :: MV.IOVector Int -> IO ()
fft2M v = MV.iforM_ v $ \ i x ->
  if i == 0
  then pure ()
  else do
    prev <- MV.read v (i-1)
    MV.write v i $ (prev + x) `mod` 10

-- Assuming we got message from reverseMessageTail, and offset was
-- more than halfway through the message.
fft2 :: V.Vector Int -> V.Vector Int
fft2 v = V.unfoldrExactN (V.length v) f (0, 0)
  where
    f (i, acc) =
      let acc' = (v V.! i + acc) `mod` 10
      in  (acc', (i + 1, acc'))

-- Repeat given message 10,000 times and drop the offset, then reverse.
reverseMessageTail :: V.Vector Int -> V.Vector Int
reverseMessageTail v = V.generate tailLength f
  where
    tailLength = V.length v * 10000 - offset v
    f i = v V.! (V.length v - 1 - i `mod` V.length v)

offset :: V.Vector Int -> Int
offset v = sum $ zipWith f [0..] $ reverse $ V.toList $ V.take 7 v
  where
    f n d = d*10^n

part1 :: V.Vector Int -> String
part1
  = concatMap show
  . V.toList
  . V.take 8
  . (!! 100)
  . iterate' fft

fft :: V.Vector Int -> V.Vector Int
fft v = V.generate (V.length v) (fftDigit v)

fftDigit :: V.Vector Int -> Int -> Int
fftDigit v i = abs (pos - neg) `mod` 10
  where
    vlength = V.length v
    pos = sums posSlices
    neg = sums negSlices
    sums slices = sum $ map (V.sum . flip safeSlice v) $ slices i vlength

-- >>> safeSlice (1, 3) $ V.fromList [0,1,2]
-- [1,2]
safeSlice :: (Int, Int) -> V.Vector a -> V.Vector a
safeSlice (i, n) v
  | i > V.length v - 1 = error "safeSlice: start too big"
  | otherwise = V.slice i (min n (V.length v - i)) v

-- >>> take 10 $ posSlices 1 73
-- [(1,2),(9,2),(17,2),(25,2),(33,2),(41,2),(49,2),(57,2),(65,2)]
posSlices :: Int -> Int -> [(Int,Int)] -- [(start, len)]
posSlices i maxI
  = [ (s, len) | s <- takeWhile (< maxI) $ iterate (+ stride) start ]
  where
    len = i + 1
    stride = len * 4
    start = i

-- >>> take 10 $ posSlices 1 73
-- [(1,2),(9,2),(17,2),(25,2),(33,2),(41,2),(49,2),(57,2),(65,2)]
negSlices :: Int -> Int -> [(Int,Int)] -- [(start, len)]
negSlices i maxI
  = [ (s, len) | s <- takeWhile (< maxI) $ iterate (+ stride) start ]
  where
    len = i + 1
    stride = len * 4
    start = i + len * 2
