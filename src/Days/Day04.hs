module Days.Day04 (runDay) where

{- ORMOLU_DISABLE -}
import Data.List
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Vector (Vector)
import Data.Char
import qualified Data.Vector as Vec
import qualified Util.Util as U

import qualified Program.RunDay as R (runDay)
import Data.Attoparsec.Text
import Data.Void
import Control.Applicative
import Control.Monad (void, guard)
import Data.Either (isRight)

import Util.Parsers
{- ORMOLU_ENABLE -}

runDay :: Bool -> String -> IO ()
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = parser `sepBy` (string "\n\n")
    where
        parser =
            Map.fromList <$> do
                entry `sepBy` space
        entry =
            (,)
                <$> (many1 letter)
                <*> ((char ':') *> many1 (letter <|> digit <|> char '#'))


------------ TYPES ------------
type Input = [Map String String]

type OutputA = Int

type OutputB = Int

------------ PART A ------------
partA :: Input -> OutputA
partA input = length $ filter check input
    where
        check entry = all (`Map.member` entry) $ ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"]

------------ PART B ------------
partB :: Input -> OutputB
partB input = length $ filter check input
    where
        check entry =
            (all (`Map.member` entry) $ ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"]) && checkValues entry
        checkValues :: Map String String -> Bool
        checkValues entry =
            let byr = Map.findWithDefault "" "byr" entry
                iyr = Map.findWithDefault "" "iyr" entry
                eyr = Map.findWithDefault "" "eyr" entry
                hgt = Map.findWithDefault "" "hgt" entry
                hcl = Map.findWithDefault "" "hcl" entry
                ecl = Map.findWithDefault "" "ecl" entry
                pid = Map.findWithDefault "" "pid" entry
                (h, unit) = span isDigit hgt
                height = read h
            in and [
                length byr == 4, read byr >= 1920, read byr <= 2002,
                length iyr == 4, read iyr >= 2010, read iyr <= 2020,
                length eyr == 4, read eyr >= 2020, read eyr <= 2030,
                unit == "cm" && height > 149 && height < 194 || unit == "in" && height > 58 && height < 77,
                length hcl == 7, head hcl == '#', (all isHexDigit $ tail hcl),
                ecl `elem` ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"],
                length pid == 9, all isDigit pid
                ]
