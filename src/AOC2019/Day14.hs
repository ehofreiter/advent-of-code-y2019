{-# LANGUAGE TupleSections #-}
module AOC2019.Day14 where

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
import Data.Ratio
import Data.Traversable
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Sequence as Seq
import Data.Sequence (Seq(..), (><))
import qualified Data.Vector as V
import Linear.V3
import Linear.Vector
import qualified Text.Parsec as P

import AOC2019.Common

dataPath = "data/day14/"
runF = dataPath ++ "input.txt"
testF = dataPath ++ "test.txt"

runI = load runF
testI = load testF

run = runI >>= exec
test = testI >>= exec

load :: FilePath -> IO [Reaction]
load filePath = do
  strs <- loadFile filePath
  pure (map readReaction strs)

exec :: [Reaction] -> IO ()
exec inputs = do
  -- print inputs
  print (part1 inputs)
  print (part2 inputs)

part2 :: [Reaction] -> Int
part2 rs = solveMaxFuel (mkRM rs) (10^12)

solveMaxFuel :: RM -> Int -> Int
solveMaxFuel rm ore = loop (1, oreForFuel rm 1) Nothing
  where
    loop minFuelOre mMaxFuelOre =
      let fuelGuess = nextFuelGuess minFuelOre mMaxFuelOre
      in
        if fuelGuess == fst minFuelOre
        then fuelGuess
        else
          let oreGuess = oreForFuel rm fuelGuess
          in  case compare oreGuess ore of
            EQ -> fuelGuess
            LT -> loop (fuelGuess, oreGuess) mMaxFuelOre
            GT -> loop minFuelOre (Just (fuelGuess, oreGuess))
    nextFuelGuess (minFuel, minOre) mMaxFuelOre = case mMaxFuelOre of
      Nothing -> 2 * minFuel
      Just (maxFuel, maxOre) -> (maxFuel + minFuel) `div` 2

oreForFuel :: RM -> Int -> Int
oreForFuel rm fuel =
  let (required, reactions) = solveReactions rm fuel
  in  required Map.! "ORE"

part1 :: [Reaction] -> Int
part1 rs =
  let rm = mkRM rs
      (required, reactions) = solveReactions rm 1
  in  required Map.! "ORE"

type State = (ChemV, ChemV) -- required, reactions

solveReactions :: RM -> Int -> State
solveReactions rm fuel = loop (initState fuel)
  where
    loop (required, reactions) =
      either id loop $ stepSolve rm (required, reactions)

initState :: Int -> State
initState fuel = (Map.singleton "FUEL" fuel, Map.empty)

solutionSteps :: RM -> Int -> [Either State State]
solutionSteps rm fuel = iterateM (stepSolve rm) (pure (initState fuel))

stepSolve :: RM -> State -> Either State State
stepSolve rm (required, reactions) =
  case Map.lookupMin (convertible required) of
    Nothing -> Left (required, reactions)
    Just (chem, q) ->
      let (reaction, r) = requiredReactions rm (chem, q)
          required' = required ^+^ r *^ reactionToChemV reaction
          reactions' = reactions ^+^ chemU (chem, r)
      in  Right (required', reactions')

requiredReactions :: RM -> ChemQ -> (Reaction, Int)
requiredReactions rm (chem, q) =
  let reaction@((_, qPerR), _) = rm Map.! chem
  in  (reaction, ceiling $ q % qPerR)

-- | Converts the given reaction to a ChemV of REQUIRED chemicals, i.e.
-- inputs - outputs.
reactionToChemV :: Reaction -> ChemV
reactionToChemV (output, inputs) =
  chemV inputs ^-^ chemV [output]

convertible :: ChemV -> ChemV
convertible = Map.filter (> 0) . Map.delete "ORE"

type ReactionQ = (Reaction, Int)

type ChemV = Map.Map Chem Int

chemU :: ChemQ -> ChemV
chemU = uncurry Map.singleton

chemV :: [ChemQ] -> ChemV
chemV = Map.fromList

type RM = Map.Map Chem Reaction

mkRM :: [Reaction] -> RM
mkRM = Map.fromList . map f
  where
    f r@((chem, n), inputs) = (chem, r)

type Chem = String
type ChemQ = (Chem, Int)
type Reaction = (ChemQ, [ChemQ]) -- output, inputs

readReaction :: String -> Reaction
readReaction str = (readChemQ output, map readChemQ (splitOn ", " inputs))
  where
    [inputs, output] = splitOn " => " str

readChemQ :: String -> ChemQ
readChemQ str = (chem, read n)
  where
    [n, chem] = splitOn " " str
