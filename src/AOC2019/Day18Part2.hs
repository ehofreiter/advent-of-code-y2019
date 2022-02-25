{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}

module AOC2019.Day18Part2 where

import Control.Applicative
import Control.Monad
import Control.Monad.Identity
import Control.Monad.State
import Control.Lens (view, _3)
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
runF = dataPath ++ "input2.txt"
testF = dataPath ++ "test2.txt"

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
  -- print (part1 inputs)
  print (part2 inputs)

part2 :: Vault -> Int
part2 = pCost . solve

newtype SolveState = SS
  { ssPaths :: Set.Set Path
  } deriving (Show)

data Path = Path
  { pCost :: Int -- important that path is sorted on cost first
  , pCurrent :: Map.Map Dir Node
  , pKeys :: Set.Set Char
  , pPath :: [(Dir, Char)] -- track sequence for debugging
  } deriving (Show)

type Current = Map.Map Dir Node

pathToTriple :: Path -> (Int, Current, Set.Set Char)
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
  , pCurrent = initCurrent
  , pKeys = Set.empty
  , pPath = []
  }

initCurrent :: Current
initCurrent = Map.fromList [(d, NEntrance d) | d <- [TL, TR, BR, BL]]

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
      newPaths = map (addPathStep path) newDirSteps
      newDirSteps =
        [ (d, (c, n))
        | (d, ncs) <- Map.toList $ reachableKeys vg path
        , (n, c) <- Map.toList ncs
        ]

addPathStep :: Path -> (Dir, Step) -> Path
addPathStep path (dir, (cost, node)) = Path
  { pCost = cost + pCost path
  , pCurrent = Map.insert dir node $ pCurrent path
  , pKeys = maybe id Set.insert (getKey node) $ pKeys path
  , pPath = maybe id (:) ((dir ,) <$> getKey node) $ pPath path
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
showPath path = unwords
  [ show $ pCost path
  , showCurrent $ pCurrent path
  , "[" <> unwords [[showDir d,c] | (d, c) <- pPath path] <> "]"
  ]

showCurrent :: Current -> String
showCurrent dirNodes
  =  "["
  <> intercalate ","
     [ [showDir d,'=',showNode n] | (d, n) <- Map.toList dirNodes ]
  <> "]"

data VaultGraph = VG
  { vgAdj :: Map.Map Node (Set.Set Step)
  , vgKeyCount :: Int -- how many keys?
  }

reachableKeys :: VaultGraph -> Path -> Map.Map Dir (Map.Map Node Int)
reachableKeys vg path = loopDir <$> pCurrent path
  where
    loopDir node = loop Set.empty (Set.singleton (0, node)) Map.empty
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

data Dir
  = TL -- top-left
  | TR
  | BR -- bottom-right
  | BL
  deriving (Eq, Ord, Show, Enum, Bounded)

showDir :: Dir -> Char
showDir d = case d of
  TL -> '!'
  TR -> '@'
  BR -> '$'
  BL -> '%'

data Node
  = NEntrance Dir
  | NKey Char
  | NDoor Char
  deriving (Eq, Ord, Show)

showNode :: Node -> Char
showNode ns = case ns of
  NEntrance d -> showDir d
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
  , vgKeyCount = length $ filter isKeyN $ Map.keys adj
  }
  where
    adj = exploreVault vault

isKeyN :: Node -> Bool
isKeyN node = case node of
  NKey _ -> True
  _ -> False

isEntranceN :: Node -> Bool
isEntranceN node = case node of
  NEntrance _ -> True
  _ -> False

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
  SEntrance d -> Just (NEntrance d)
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

initES :: CV.Coord -> ExploreState
initES coord = ES
  { esSeen = Set.empty
  , esFrontier = Set.singleton (0, coord)
  , esSteps = Set.empty
  }

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
        , isEmptyEntranceS s
        ]
      steps' = foldl' addStep (esSteps es) newKeyDoorSteps
      newKeyDoorSteps =
        [ (cost+1, n)
        | Just n <- map (nodeFromSpot . snd) newCoordSpots
        , not (isEntranceN n)
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

findEntrances :: Vault -> [CV.Coord]
findEntrances vault =
  [ CV.unflattenCoord vault
    . fromJust
    . elemIndex (SEntrance dir)
    $ CV.toList vault
  | dir <- [TL, TR, BR, BL]
  ]

type Vault = CV.CoordVec Spot

readVault :: [String] -> Vault
readVault = CV.fromLists . (map.map) readSpot

showVault :: Vault -> [String]
showVault = (map.map) showSpot . CV.toLists

printVault :: Vault -> IO ()
printVault = mapM_ putStrLn . showVault

data Spot
  = SEntrance Dir
  | SEmpty
  | SWall
  | SKey Char
  | SDoor Char
  deriving (Eq, Ord, Show)

isKeyDoorS :: Spot -> Bool
isKeyDoorS s = case s of
  SKey _ -> True
  SDoor _ -> True
  _ -> False

isEmptyEntranceS :: Spot -> Bool
isEmptyEntranceS s = case s of
  SEntrance _ -> True
  SEmpty -> True
  _ -> False

readSpot :: Char -> Spot
readSpot c = case c of
  '!' -> SEntrance TL
  '@' -> SEntrance TR
  '$' -> SEntrance BR
  '%' -> SEntrance BL
  '.' -> SEmpty
  '#' -> SWall
  c'
    | isLower c' -> SKey c'
    | isUpper c' -> SDoor c'

showSpot :: Spot -> Char
showSpot s = case s of
  SEntrance d -> showDir d
  SEmpty -> '.'
  SWall -> '#'
  SKey lc -> lc
  SDoor uc -> uc

