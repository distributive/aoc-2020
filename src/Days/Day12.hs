module Days.Day12 (runDay) where

{- ORMOLU_DISABLE -}
import Data.List
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe
import Data.Functor (($>))
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
inputParser = parser `sepBy` endOfLine
    where
        parser = do
            c <- command
            d <- decimal
            return (c, d)
        command = choice
            [
                string "N" $> N,
                string "E" $> E,
                string "S" $> S,
                string "W" $> W,
                string "L" $> L,
                string "R" $> R,
                string "F" $> F
            ]

------------ TYPES ------------
data Instruction = N | E | S | W | L | R | F deriving (Show, Eq)

type Order = (Instruction, Int)

type Coord = (Int, Int)

-- State = (Ship coordinates, Ship heading)
type State = (Coord, Instruction)

-- WState = (Ship coordinates, Waypoint coordinates)
type WState = (Coord, Coord)



type Input = [Order]

type OutputA = Int

type OutputB = Int

----------- FUNCTIONS ----------
manhatten :: Coord -> Int
manhatten (x,y) = sum [abs x, abs y]

------------ PART A ------------
partA :: Input -> OutputA
partA input = manhatten . fst $ foldr travel ((0,0), E) $ reverse input
    where
        travel :: Order -> State -> State
        travel (N, dis) ((x,y), dir) = ((x,y + dis), dir)
        travel (E, dis) ((x,y), dir) = ((x + dis,y), dir)
        travel (S, dis) ((x,y), dir) = ((x,y - dis), dir)
        travel (W, dis) ((x,y), dir) = ((x - dis,y), dir)

        travel (F, dis) (coord, dir) = travel (dir, dis) (coord, dir)

        travel (L,  90) (coord, N) = (coord, W)
        travel (L,  90) (coord, E) = (coord, N)
        travel (L,  90) (coord, S) = (coord, E)
        travel (L,  90) (coord, W) = (coord, S)
        travel (L, 270) state      = travel (R, 90) state
        travel (R,  90) (coord, N) = (coord, E)
        travel (R,  90) (coord, E) = (coord, S)
        travel (R,  90) (coord, S) = (coord, W)
        travel (R,  90) (coord, W) = (coord, N)
        travel (R, 270) state      = travel (L, 90) state
        travel (_, 180) (coord, N) = (coord, S)
        travel (_, 180) (coord, E) = (coord, W)
        travel (_, 180) (coord, S) = (coord, N)
        travel (_, 180) (coord, W) = (coord, E)


------------ PART B ------------
partB :: Input -> OutputB
partB input = manhatten . fst $ foldr travel ((0,0), (10,1)) $ reverse input
    where
        travel :: Order -> WState -> WState
        travel (N, dis) (pos, (x,y)) = (pos, (x,y + dis))
        travel (E, dis) (pos, (x,y)) = (pos, (x + dis,y))
        travel (S, dis) (pos, (x,y)) = (pos, (x,y - dis))
        travel (W, dis) (pos, (x,y)) = (pos, (x - dis,y))

        travel (L,  90) (pos, (x,y)) = (pos, (-y, x))
        travel (L, 180) (pos, (x,y)) = (pos, (-x,-y))
        travel (L, 270) (pos, (x,y)) = (pos, ( y,-x))
        travel (R, dis) state = travel (L, 360-dis) state

        travel (F, dis) ((u,v), (x,y)) = ((u + (dis*x), v + (dis*y)), (x,y))
