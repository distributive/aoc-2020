module Days.Day07 (runDay) where

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

import Util.Parsers
{- ORMOLU_ENABLE -}

runDay :: Bool -> String -> IO ()
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = Map.fromList <$> parser `sepBy` endOfLine
    where
        parser = do
            c <- colour
            string " bags contain "
            contents <- empty <|> bag `sepBy` string ", "
            char '.'
            return (c, Map.fromList contents)
        colour = do
            (adj, col) <- many1 letter `around` space
            return (adj ++ " " ++ col)
        bag = do
            d <- decimal
            skipSpace
            c <- colour
            string " bag"
            optional $ string "s"
            return (c, d)
        empty = do
            string "no other bags"
            return []

------------ TYPES ------------
type Input = Map String (Map String Int)

type OutputA = Int

type OutputB = Int

------------ PART A ------------
partA :: Input -> OutputA
partA input = length . filter id $ fmap (canContain "shiny gold") $ Map.toList input
    where
        canContain query (colour, contents)
            | Map.size contents == 0    = False
            | Map.member query contents = True
            | otherwise                 = any id $ fmap (canContain query . getContent) $ Map.keys contents
        getContent colour = (colour, Map.findWithDefault Map.empty colour input)

------------ PART B ------------
partB :: Input -> OutputB
partB input = getBags ("shiny gold", 1)
    where
        getBags (query, count) =
            let contents = Map.findWithDefault Map.empty query input
            in count * ((sum $ Map.elems contents) + (sum $ fmap (getBags) $ Map.toList contents))
