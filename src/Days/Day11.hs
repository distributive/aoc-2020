module Days.Day11 (runDay) where

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

import Util.Parsers (coordinateParser)
{- ORMOLU_ENABLE -}

runDay :: Bool -> String -> IO ()
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = coordinateParser mapSeat 0
    where
        mapSeat '.' = Just False
        mapSeat 'L' = Just True
        mapSeat  _  = Nothing

------------ TYPES ------------
type Coord = (Int, Int)

type Input = Map Coord Bool

type OutputA = Int

type OutputB = Int

------------ PART A ------------
partA :: Input -> OutputA
partA input =
    let states = iterate step $ fmap (\x -> False) $ Map.filter id input
        state = fst . head $ dropWhile (\(a,b) -> a /= b) $ zip states (tail states)
    in length $ Map.filter id state
        where
            step :: Map Coord Bool -> Map Coord Bool
            step state = Map.mapWithKey (check state) state

            check :: Map Coord Bool -> Coord -> Bool -> Bool
            check state coord value
                |     value = if numAdjacent state coord >= 4 then False else True
                | not value = if numAdjacent state coord == 0 then True else False

            numAdjacent :: Map Coord Bool -> Coord -> Int
            numAdjacent state (x,y) = sum . map (\x -> if x then 1 else 0) $ [
                    Map.findWithDefault False (x+1,y+1) state,
                    Map.findWithDefault False (x  ,y+1) state,
                    Map.findWithDefault False (x-1,y+1) state,
                    Map.findWithDefault False (x+1,y  ) state,
                    Map.findWithDefault False (x-1,y  ) state,
                    Map.findWithDefault False (x+1,y-1) state,
                    Map.findWithDefault False (x  ,y-1) state,
                    Map.findWithDefault False (x-1,y-1) state
                ]

------------ PART B ------------
partB :: Input -> OutputB
partB input =
    let states = iterate step $ fmap (\x -> False) $ Map.filter id input
        state = fst . head $ dropWhile (\(a,b) -> a /= b) $ zip states (tail states)
    in length $ Map.filter id state
        where
            step :: Map Coord Bool -> Map Coord Bool
            step state = Map.mapWithKey (check state) state

            check :: Map Coord Bool -> Coord -> Bool -> Bool
            check state coord value
                |     value = if numAdjacent state coord >= 5 then False else True
                | not value = if numAdjacent state coord == 0 then True else False

            numAdjacent :: Map Coord Bool -> Coord -> Int
            numAdjacent state (x,y) = sum . map (\x -> if x then 1 else 0) $ [
                    Map.findWithDefault False (seatCast (x,y) ( 1, 1)) state,
                    Map.findWithDefault False (seatCast (x,y) ( 0, 1)) state,
                    Map.findWithDefault False (seatCast (x,y) (-1, 1)) state,
                    Map.findWithDefault False (seatCast (x,y) ( 1, 0)) state,
                    Map.findWithDefault False (seatCast (x,y) (-1, 0)) state,
                    Map.findWithDefault False (seatCast (x,y) ( 1,-1)) state,
                    Map.findWithDefault False (seatCast (x,y) ( 0,-1)) state,
                    Map.findWithDefault False (seatCast (x,y) (-1,-1)) state
                ]

            seatCast :: Coord -> Coord -> Coord
            seatCast start (d_x,d_y) =
                let ray = tail $ iterate (\(x,y) -> (x+d_x, y+d_y)) start
                in head $ dropWhile (\coord -> not $ Map.findWithDefault True coord input) ray
