module Days.Day02 (runDay) where

{- ORMOLU_DISABLE -}
import Util.Parsers
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
import Data.Bits (xor)
{- ORMOLU_ENABLE -}

runDay :: Bool -> String -> IO ()
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = ruleAndPassword `sepBy` endOfLine
  where
    ruleAndPassword = do
      (lower, upper) <- decimal `around` char '-'
      skipSpace
      givenLetter <- letter
      string ": "
      password <- many1 letter
      return (PasswordRule {..}, password)

-- This, and the associated types, were copied from Sam's solution
-- Stupid black magic fuckery

------------ TYPES ------------
type Password = String

data PasswordRule = PasswordRule
  { lower :: Int,
    upper :: Int,
    givenLetter :: Char
  }
  deriving (Show)

type Input = [(PasswordRule, Password)]

type OutputA = Int

type OutputB = Int

------------ PART A ------------
partA :: Input -> OutputA
partA = length . filter checkRule
    where
        checkRule (rule, password) =
            let count = length . filter (givenLetter rule ==) $ password
            in count >= lower rule && count <= upper rule

------------ PART B ------------
partB :: Input -> OutputB
partB = length . filter checkRule
    where
        checkRule (rule, password) =
            let low = givenLetter rule == password!!(lower rule - 1)
                up  = givenLetter rule == password!!(upper rule - 1)
            in low `xor` up
