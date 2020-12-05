module Days.Day05 (runDay) where

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

import Control.Applicative
import Control.Monad
{- ORMOLU_ENABLE -}

runDay :: Bool -> String -> IO ()
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = binary `sepBy` endOfLine
    where
        binary = let f = char 'F' >> return '0'
                     b = char 'B' >> return '1'
                     l = char 'L' >> return '0'
                     r = char 'R' >> return '1'
            in replicateM 10 (f <|> b <|> l <|> r)

------------ TYPES ------------
type Input = [String]

type OutputA = Int

type OutputB = Int

----------- FUNCTIONS ----------
toBin :: [Char] -> Int -> Int
toBin [] val = 0
toBin ('1':xs) val = (toBin xs val) + val + (2 ^ length xs)
toBin ( _ :xs) val = (toBin xs val) + val

------------ PART A ------------
partA :: Input -> OutputA
partA input = maximum $ map (`toBin` 0) input

------------ PART B ------------
partB :: Input -> OutputB
partB input = head [x | x <- [1..1023], (not $ x `elem` map (`toBin` 0) input) && ((x - 1) `elem` map (`toBin` 0) input)]
