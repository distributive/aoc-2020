module Days.Day10 (runDay) where

{- ORMOLU_DISABLE -}
import Data.List as L
import Data.Map.Strict (Map, findWithDefault, member)
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
inputParser = decimal `sepBy` endOfLine

------------ TYPES ------------
type Input = [Int]

type OutputA = Int

type OutputB = Int

------------ PART A ------------
partA :: Input -> OutputA
partA input =
    let ls = sort $ input ++ [0, maximum input + 3]
        diffs = zipWith (\a b -> abs (a - b)) ls (tail $ ls)
    in (length $ filter (==1) diffs) * (length $ filter (==3) diffs)

------------ PART B ------------
partB :: Input -> OutputB
partB input =
    let sorted = sort $ input ++ [0, maximum input + 3]
        mapping = traverseSeq (Map.fromList $ getSubsequences sorted) 0 Map.empty
    in findWithDefault (-1) 0 mapping
        where
            getSubsequences [] = []
            getSubsequences (x:xs) = (x, L.takeWhile (<= x + 3) xs) : getSubsequences xs

            traverseSeq :: Map Int [Int] -> Int -> Map Int Int -> Map Int Int
            traverseSeq sequences index mem
                | Map.member index mem       = mem
                | index == maximum input + 3 = Map.insert index 1 mem
                | otherwise                  =
                    let children = findWithDefault [] index sequences
                        new = foldr (traverseSeq sequences) mem children
                    in Map.insert index (sum $ fmap (\child -> findWithDefault 1 child new) children) new
