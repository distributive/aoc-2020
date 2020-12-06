module Days.Day06 (runDay) where

{- ORMOLU_DISABLE -}
import Data.List as L
import Data.Set as S
import Data.Map.Strict (Map)
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
{- ORMOLU_ENABLE -}

runDay :: Bool -> String -> IO ()
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = (many1 letter `sepBy` endOfLine) `sepBy` (string "\n\n")
-- The first parser I worked out all by myself :)

------------ TYPES ------------
type Input = [[String]]

type OutputA = Int

type OutputB = Int

------------ PART A ------------
partA :: Input -> OutputA
partA input = sum $ fmap size (fmap (S.fromList . concat) input)

------------ PART B ------------
partB :: Input -> OutputB
partB input = sum $ fmap (size . (L.foldr intersection $ S.fromList "abcdefghijklmnopqrstuvwxyz") . (fmap S.fromList)) input
