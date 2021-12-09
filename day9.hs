import Data.Char (digitToInt)
import Data.Functor.Identity
import Data.Either
import Data.List
import qualified Data.Map as Map
import Data.Maybe
import System.IO
import Text.Parsec
import Text.Parsec.String

testStr = "2199943210\n3987894921\n9856789892\n8767896789\n9899965678"

type IntMatrix = Map.Map (Int, Int) Int

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

neighbors :: Int -> Int -> [(Int, Int)]
neighbors i j = [(i-1, j), (i+1, j), (i, j-1), (i, j+1)]

validNeighbors :: IntMatrix -> Int -> Int -> [(Int, Int)]
validNeighbors l i j = filter (`Map.member` l) (neighbors i j)

getNeighborVals :: IntMatrix -> Int -> Int -> [Int]
getNeighborVals l i j = mapMaybe (`Map.lookup` l) (neighbors i j)

isMin :: IntMatrix -> (Int, Int) -> Bool
isMin l (i, j) = all (> (l Map.! (i, j))) (getNeighborVals l i j)

risk :: IntMatrix -> (Int, Int) -> Int
risk l (i, j) = if isMin l (i, j) then 1 + l Map.! (i, j) else 0

totalRisk :: IntMatrix -> Int
totalRisk l = sum $ map (risk l) (Map.keys l)

solve :: [[Int]] -> Int
solve = totalRisk . listOfListsToMat

-- Part 2
smallestNeighbor :: IntMatrix -> (Int, Int) -> (Int, (Int, Int))
smallestNeighbor l (i, j) = minimum $ zip (map (l Map.!) n) n
  where n = validNeighbors l i j

-- Only valid for values < 9
findBasin l (i, j) = if snval < curval then findBasin l sn else (i, j)
  where
    (snval, sn) = smallestNeighbor l (i, j)
    curval = l Map.! (i, j)

findBasins :: IntMatrix -> [(Int, Int)]
findBasins l = map (findBasin l) (Map.keys (Map.filter (<9) l))

solvePart2 :: [[Int]] -> Int
solvePart2 l = product
  . take 3
  . reverse
  . sort
  $ length <$> (group . sort $ findBasins $ listOfListsToMat l)

main = do
    file <- readFile "/tmp/day9-input.txt"
    print (solve $ parseInput file)
    print (solvePart2 $ parseInput file)
