module Days.Day09 (runDay) where

{- ORMOLU_DISABLE -}
import Data.List
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
inputParser = decimal `sepBy` endOfLine

------------ TYPES ------------
type Input = [Int]

type OutputA = Int

type OutputB = Int

------------ PART A ------------
partA :: Input -> OutputA
partA input = check (Data.List.take 25 input) (drop 25 input)
    where
        check key (x:xs) = if isValid key x then check (tail key ++ [x]) xs else x
        isValid key val = length [(x,y) | x <- key, y <- key, x + y == val, x /= y] > 0

------------ PART B ------------
partB :: Input -> OutputB
partB input =
    let pair = fromJust . fromJust $ find isJust $ checkSum 0 (partA input) 10000000 0 <$> [drop x input | x <- [0..(length input)]]
    in fst pair + snd pair
        where
            checkSum _ _ _ _ [] = Nothing
            checkSum count val low high (x:xs) =
                 if
                     |         x == val -> Nothing
                     | count + x == val -> Just (low, high)
                     | count + x  > val -> Nothing
                     | otherwise        -> checkSum (count + x) val (min x low) (max x high) xs
