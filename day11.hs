import Data.Char (digitToInt)
import Data.Functor.Identity
import Data.Either
import Data.List
import qualified Data.Map as Map
import Data.Maybe
import System.IO
import Text.Parsec
import Text.Parsec.String

-- testStr = "11111\n19991\n19191\n19991\n11111"
testStr = "5483143223\n2745854711\n5264556173\n6141336146\n6357385478\n4167524645\n2176841721\n6882881134\n4846848554\n5283751526"
-- m = listOfListsToMat $ parseInput testStr

type IntMatrix = Map.Map (Int, Int) Int
type BoolMatrix = Map.Map (Int, Int) Bool

inputParser :: ParsecT String () Identity [[Int]]
inputParser =
  do lines <- sepBy integers eol
     eof
     return lines
integers :: Parser [Int]
integers = many1 (digitToInt <$> digit)
eol = char '\n'

parseInput :: String -> [[Int]]
parseInput = fromRight [] . parse inputParser "(unknown)"

enumerate = zip [0..]

listOfListsToMat :: [[Int]] -> IntMatrix
listOfListsToMat m = Map.fromList [
    ((i, j), elt) | (i, row) <- enumerate m, (j, elt) <- enumerate row]

neighbors :: (Int, Int) -> [(Int, Int)]
neighbors (i, j) = [(i-1, j), (i+1, j), (i, j-1), (i, j+1),
                    (i-1, j-1), (i-1, j+1), (i+1, j-1), (i+1, j+1)]

-- Step 1: increment the energy
incEnergy :: IntMatrix -> IntMatrix
incEnergy = Map.map (+1)

-- Step 2: find the ones that are currently flashing
newFlashing :: [(Int, Int)] -> IntMatrix -> [(Int, Int)]
-- f : already flashed this step
newFlashing f m = Map.keys $
  Map.filterWithKey (\k v -> (v > 9) && (k `notElem` f)) m

-- Find out how much energy to add to each octopus; f = newFlashing
energyIncrease :: [(Int, Int)] -> IntMatrix -> IntMatrix
energyIncrease f = Map.intersection (Map.fromList $
  map (\a -> (head a, length a))
  $ group . sort
  $ concatMap neighbors f)

-- Apply the increase from the previous step
applyIncrease :: IntMatrix -> IntMatrix -> IntMatrix
applyIncrease = Map.unionWith (+)

-- Returns the lists of flashes + the result
innerStep :: IntMatrix -> ([(Int, Int)], IntMatrix)
innerStep = go []
  where go f m = if null newf then (f, m)
                 else go (f ++ newf) (applyIncrease (energyIncrease newf m) m)
                 where newf = newFlashing f m

-- At the end, reset the ones that have flashed
resetFlashed :: [(Int, Int)] -> IntMatrix -> IntMatrix
resetFlashed f = Map.union (Map.fromList [(x, 0) | x <- f])

step :: IntMatrix -> (IntMatrix, Int)
step m = (resetFlashed f m', length f)
  where (f, m') = innerStep (incEnergy m)

sumStep :: (IntMatrix, Int) -> (IntMatrix, Int)
sumStep (m, l) = (m', l + l')
  where (m', l') = step m

solve :: [[Int]] -> Int
solve ll = snd $ iterate sumStep (listOfListsToMat ll, 0) !! 100

-- Part 2
-- (counter, map, number of flashing)
countStep :: (Int, IntMatrix, Int) -> (Int, IntMatrix, Int)
countStep (i, m, l) = (i+1, m', l')
  where (m', l') = step m

solvePart2 :: [[Int]] -> Int
solvePart2 ll = fst
  $ fromJust
  $ find (\(_,m',l) -> Map.size m' == l) (iterate countStep (0, m, 0))
  where m = listOfListsToMat ll
        fst (a, _, _) = a

main = do
    file <- readFile "/tmp/day11-input.txt"
    print (solve $ parseInput file)
    print (solvePart2 $ parseInput file)
