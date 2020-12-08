module Days.Day08 (runDay) where

{- ORMOLU_DISABLE -}
import Data.List
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Vector (Vector)
import Data.Vector ((!), (//))
import qualified Data.Vector as Vec
import qualified Util.Util as U

import qualified Program.RunDay as R (runDay)
import Data.Attoparsec.Text
import Data.Void

import Control.Applicative
import Control.Monad
import Control.Monad.State.Lazy
{- ORMOLU_ENABLE -}

runDay :: Bool -> String -> IO ()
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = Vec.fromList <$> instruction `sepBy` endOfLine
    where
        instruction =
            choice
            [
                string "acc " >> Acc <$> (signed decimal),
                string "jmp " >> Jmp <$> (signed decimal),
                string "nop " >> Nop <$> (signed decimal)
            ]

{- Original, worked out all by myself (the above is modified on suggestion by Sam) -}
-- inputParser = instruction `sepBy` endOfLine
--     where
--         instruction = do
--             ins <- count 3 letter
--             skipSpace
--             value <- signed decimal
--             return (ins, value)

------------ TYPES ------------
data Instruction = Acc Int | Jmp Int | Nop Int deriving (Show, Eq)
data Output = Terminate Int | Loop Int
type Program = Vector Instruction

type Input = Program

type OutputA = Int

type OutputB = Int

---------- INTERPRETER ---------
runProgram :: Set Int -> Int -> Int -> Program -> Output
runProgram visited counter accumulator program =
    if
        | counter `Set.member` visited -> Loop accumulator
        | counter >= length program    -> Terminate accumulator
        | otherwise -> case program!counter of
            Acc val -> runProgram (Set.insert counter visited) (counter +   1) (accumulator + val) program
            Jmp val -> runProgram (Set.insert counter visited) (counter + val)  accumulator        program
            Nop val -> runProgram (Set.insert counter visited) (counter +   1)  accumulator        program

------------ PART A ------------
partA :: Input -> OutputA
partA input = case runProgram Set.empty 0 0 input of
    Loop      out -> out
    Terminate out -> error "Program terminated when a loop was expected"


------------ PART B ------------
partB :: Input -> OutputB
partB input = fromOutput $ head $ filter isTerm $ (runProgram Set.empty 0 0) <$> [swapAt i input | i <- [0..length input]]
    where
        fromOutput (Terminate val) = val
        fromOutput (Loop      val) = val
        isTerm (Terminate _) = True
        isTerm (Loop      _) = False
        swapAt index program = program // [(index, swap $ program!index)]
        swap (Nop val) = Jmp val
        swap (Jmp val) = Nop val
        swap (Acc val) = Acc val
