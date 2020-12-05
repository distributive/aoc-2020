module Days.Day03 (runDay) where

{- ORMOLU_DISABLE -}
import Data.List
import Data.Map.Strict (Map)
import Data.Map.Strict (findWithDefault)
import qualified Data.Map.Strict as Map
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Vector (Vector)
import qualified Data.Vector as Vec
import qualified Util.Util as U

import qualified Program.RunDay as R (runDay)
import Data.Attoparsec.Text
import Data.Void

import Util.Parsers (coordinateParser)
{- ORMOLU_ENABLE -}

runDay :: Bool -> String -> IO ()
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = coordinateParser mapTree 0
    where
        mapTree '.' = Just 0
        mapTree '#' = Just 1
        mapTree  _  = Nothing

------------ TYPES ------------
type Input = (Map (Int, Int) Int)

type OutputA = Int

type OutputB = [Int]

------------ PART A ------------
partA :: Input -> OutputA
partA input = length . filter (1==) $ [ findWithDefault 0 (y*3 `mod` 31, y) input | y <- [0..322] ]

------------ PART B ------------
partB :: Input -> OutputB
partB input = map findForRatio [(1,1), (3,1), (5,1), (7,1), (1,2)]
    where
        findForRatio (a,b) = length . filter (1==) $ [ findWithDefault 0 (x `mod` 31, y) input | y <- [0..322], x <- [0..(322*a `div` b)], b*x == a*y ]
