{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module AOC2019.Day18 where

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
import qualified AOC2019.CoordVec as CV

dataPath = "data/day18/"
runF = dataPath ++ "input.txt"
testF = dataPath ++ "test.txt"

runI = load runF
testI = load testF

run = runI >>= exec
test = testI >>= exec

load :: FilePath -> IO Vault
load filePath = do
  strs <- loadFile filePath
  pure $ readVault strs

exec :: Vault -> IO ()
exec inputs = do
  printVault inputs
  print (part1 inputs)
  -- part2 inputs

part1 :: Vault -> Int
part1 = pCost . solve

newtype SolveState = SS
  { ssPaths :: Set.Set Path
  } deriving (Show)

data Path = Path
  { pCost :: Int -- important that path is sorted on cost first
  , pCurrent :: Node
  , pKeys :: Set.Set Char
  , pPath :: [Char] -- track sequence for debugging
  } deriving (Show)

pathToTriple :: Path -> (Int, Node, Set.Set Char)
pathToTriple p = (pCost p, pCurrent p, pKeys p)

instance Eq Path where
  p1 == p2 = pathToTriple p1 == pathToTriple p2

instance Ord Path where
  compare p1 p2 = compare (pathToTriple p1) (pathToTriple p2)

solve :: Vault -> Path
solve vault = loop initSS
  where
    loop ss = case stepSolve vaultGraph ss of
      Right ss' -> loop ss'
      Left path -> path
    vaultGraph = mkVaultGraph vault

allSolveSteps :: Vault -> [Either Path SolveState]
allSolveSteps vault = iterateM (stepSolve vaultGraph) (Right initSS)
  where
    vaultGraph = mkVaultGraph vault

initSS :: SolveState
initSS = SS
  { ssPaths = Set.singleton initPath
  }

initPath :: Path
initPath = Path
  { pCost = 0
  , pCurrent = NEntrance
  , pKeys = Set.empty
  , pPath = []
  }

stepSolve :: VaultGraph -> SolveState -> Either Path SolveState
stepSolve vg ss = case Set.minView (ssPaths ss) of
  Nothing -> error "out of solve paths"
  Just (path, paths)
    | isComplete vg path -> Left path
    | otherwise -> Right SS
      --{ ssPaths = foldl' (flip Set.insert) prunedPaths prunedNewPaths
      { ssPaths = foldl' (flip Set.insert) paths newPaths
      }
    where
      -- prunedPaths = Set.filter (not . canPrune newPaths) paths
      -- prunedNewPaths = filter (not . canPrune paths) newPaths
      newPaths = map (addPathStep path) newSteps
      newSteps =
        [(c, n) | (n, c) <- Map.toList $ reachableKeys vg path]

addPathStep :: Path -> Step -> Path
addPathStep path (cost, node) = Path
  { pCost = cost + pCost path
  , pCurrent = node
  , pKeys = maybe id Set.insert (getKey node) $ pKeys path
  , pPath = maybe id (:) (getKey node) $ pPath path
  }

getKey :: Node -> Maybe Char
getKey node = case node of
  NKey c -> Just c
  _ -> Nothing

isComplete :: VaultGraph -> Path -> Bool
isComplete vg path = vgKeyCount vg == Set.size (pKeys path)

isTraversable :: Path -> Node -> Bool
isTraversable path node = case node of
  NDoor c -> toLower c `Set.member` pKeys path
  _ -> True

canPrune :: Foldable t => t Path -> Path -> Bool
canPrune paths path = any (`betterThan` path) paths

betterThan :: Path -> Path -> Bool
path1 `betterThan` path2
  =  pCost path1 <= pCost path2
  && pCurrent path1 == pCurrent path2
  && (pKeys path2 `Set.isProperSubsetOf` pKeys path1)

printSS :: SolveState -> IO ()
printSS = mapM_ putStrLn . showSS

showSS :: SolveState -> [String]
showSS = map showPath . Set.toList . ssPaths

printPath :: Path -> IO ()
printPath = putStrLn . showPath

showPath :: Path -> String
showPath path = show (pCost path, pCurrent path, pPath path)

data VaultGraph = VG
  { vgAdj :: Map.Map Node (Set.Set Step)
  , vgKeyCount :: Int -- how many keys?
  }

reachableKeys :: VaultGraph -> Path -> Map.Map Node Int
reachableKeys vg path =
  loop Set.empty (Set.singleton (0, pCurrent path)) Map.empty
  where
    loop :: Set.Set Node -> Set.Set Step -> Map.Map Node Int -> Map.Map Node Int
    loop seen frontier keyCosts = case Set.minView frontier of
      Nothing -> keyCosts
      Just ((cost, node), frontier') ->
        loop seen' frontier'' keyCosts'
        where
          seen' = Set.insert node seen
          frontier'' = foldl' (flip Set.insert) frontier' otherSteps
          keyCosts' = foldl' insertKeyCosts keyCosts newKeySteps
          insertKeyCosts m (c, n) = Map.insertWith min n c m
          (newKeySteps, otherSteps) = partition isNewKey newSteps
          newSteps =
            [ (cost + cost', n)
            | (cost', n) <- Set.toList $ vgAdj vg Map.! node
            , Set.notMember n seen
            , isTraversable path n
            ]
          isNewKey (_, n) = case n of
            NKey c -> Set.notMember c (pKeys path)
            _ -> False

data Node
  = NEntrance
  | NKey Char
  | NDoor Char
  deriving (Eq, Ord, Show)

showNode :: Node -> Char
showNode ns = case ns of
  NEntrance -> '@'
  NKey c -> c
  NDoor c -> c

type Step = (Int, Node) -- cost, node

showStep :: Step -> String
showStep (cost, node) = '(' : showNode node : ':' : show cost <> ")"

showAdj :: Map.Map Node (Set.Set Step) -> [String]
showAdj nodeSteps =
  [ showNode node : ':' : '['
    :  intercalate "," (map showStep $ Set.toList steps)
    <> "]"
  | (node, steps) <- Map.toList nodeSteps
  ]

printAdj :: Map.Map Node (Set.Set Step) -> IO ()
printAdj = mapM_ putStrLn . showAdj

mkVaultGraph :: Vault -> VaultGraph
mkVaultGraph vault = VG
  { vgAdj = adj
  , vgKeyCount = length $ filter isKey $ Map.keys adj
  }
  where
    adj = exploreVault vault

isKey :: Node -> Bool
isKey node = case node of
  NKey _ -> True
  _ -> False

initES :: CV.Coord -> ExploreState
initES coord = ES
  { esSeen = Set.empty
  , esFrontier = Set.singleton (0, coord)
  , esSteps = Set.empty
  }

exploreVault :: Vault -> Map.Map Node (Set.Set Step)
exploreVault vault = Map.fromList
  [ (node, exploreVaultFrom vault coord)
  | (node, coord) <- findNodeCoords vault
  ]

findNodeCoords :: Vault -> [(Node, CV.Coord)]
findNodeCoords = catMaybes . CV.toList . imap f
  where
    f coord spot = do
      node <- nodeFromSpot spot
      Just (node, coord)

nodeFromSpot :: Spot -> Maybe Node
nodeFromSpot spot = case spot of
  SEntrance -> Just NEntrance
  SKey c -> Just (NKey c)
  SDoor c -> Just (NDoor c)
  _ -> Nothing

exploreVaultFrom :: Vault -> CV.Coord -> Set.Set Step
exploreVaultFrom vault coord = loop (initES coord)
  where
    loop es = case stepExplore vault es of
      Left adj -> adj
      Right es' -> loop es'
    entranceCoord = CV.toList vault

data ExploreState = ES
  { esSeen :: Set.Set CV.Coord
  , esFrontier :: Set.Set (Int, CV.Coord) -- cost, node coordinate
  , esSteps :: Set.Set Step
  }

stepExplore
  :: Vault -> ExploreState
  -> Either (Set.Set Step) ExploreState
stepExplore vault es = case Set.minView (esFrontier es) of
  Nothing -> Left (esSteps es)
  Just ((cost, coord), frontier) -> Right ES
    { esSeen = Set.insert coord (esSeen es)
    , esFrontier = frontier'
    , esSteps = steps'
    }
    where
      frontier' = foldl' (flip Set.insert) frontier newCostEmptyCoords
      newCostEmptyCoords =
        [ (cost+1, c)
        | (c, s) <- newCoordSpots
        , s == SEmpty || s == SEntrance
        ]
      steps' = foldl' addStep (esSteps es) newKeyDoorSteps
      newKeyDoorSteps =
        [ (cost+1, n)
        | Just n <- map (nodeFromSpot . snd) newCoordSpots
        , n /= NEntrance
        ]
      newCoordSpots = filter (notSeen . fst) $ adjCoordSpots vault coord
      notSeen c = Set.notMember c (esSeen es)

addStep :: Set.Set Step -> Step -> Set.Set Step
addStep steps step@(cost, coord) =
  if any ((== coord) . snd) steps
  then steps
  else Set.insert step steps

adjCoordSpots :: Vault -> CV.Coord -> [(CV.Coord, Spot)]
adjCoordSpots = CV.adjPairs4

findEntrance :: Vault -> CV.Coord
findEntrance vault
  = CV.unflattenCoord vault
  . fromJust
  . elemIndex SEntrance
  $ CV.toList vault

type Vault = CV.CoordVec Spot

readVault :: [String] -> Vault
readVault = CV.fromLists . (map.map) readSpot

showVault :: Vault -> [String]
showVault = (map.map) showSpot . CV.toLists

printVault :: Vault -> IO ()
printVault = mapM_ putStrLn . showVault

data Spot
  = SEntrance
  | SEmpty
  | SWall
  | SKey Char
  | SDoor Char
  deriving (Eq, Ord, Show)

isKeyDoor :: Spot -> Bool
isKeyDoor s = case s of
  SKey _ -> True
  SDoor _ -> True
  _ -> False

readSpot :: Char -> Spot
readSpot c = case c of
  '@' -> SEntrance
  '.' -> SEmpty
  '#' -> SWall
  c'
    | isLower c' -> SKey c'
    | isUpper c' -> SDoor c'

showSpot :: Spot -> Char
showSpot s = case s of
  SEntrance -> '@'
  SEmpty -> '.'
  SWall -> '#'
  SKey lc -> lc
  SDoor uc -> uc

