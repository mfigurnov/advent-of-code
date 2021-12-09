import Data.Functor.Identity
import Data.Either
import Data.List
import qualified Data.Map as Map
import System.IO
import Text.Parsec
import Text.Parsec.String

testStr = "0,9 -> 5,9\n8,0 -> 0,8\n9,4 -> 3,4\n2,2 -> 2,1\n7,0 -> 7,4\n6,4 -> 2,0\n0,9 -> 2,9\n3,4 -> 1,4\n0,0 -> 8,8\n5,5 -> 8,2"

type LineCoords = ((Int, Int), (Int, Int))
type IntMatrix = Map.Map (Int, Int) Int

inputParser :: ParsecT String () Identity [LineCoords]
inputParser =
  do lines <- sepBy line eol
     eof
     return lines
line = do x1 <- integer
          char ','
          y1 <- integer
          string " -> "
          x2 <- integer
          char ','
          y2 <- integer
          return ((x1, y1), (x2, y2))
integer :: Parser Int
integer = (read :: String -> Int) <$> many1 digit
eol = char '\n'

parseInput :: String -> [LineCoords]
parseInput = fromRight [] . parse inputParser "(unknown)"

emptyMap :: IntMatrix
emptyMap = Map.empty

incrMat :: IntMatrix -> (Int, Int) -> IntMatrix
incrMat mat (x, y) = Map.alter incr (x, y) mat
  where incr (Just n) = Just (n + 1)
        incr Nothing = Just 1

updateMap :: IntMatrix -> LineCoords -> IntMatrix
updateMap mat ((x1, y1), (x2, y2))
  | x1 == x2 && y1 == y2 = incrMat mat (x1, y1)
  | x1 == x2 = updateMap (incrMat mat (x1, y1)) ((x1, y1 + signum (y2 - y1)), (x2, y2))
  | y1 == y2 = updateMap (incrMat mat (x1, y1)) ((x1 + signum (x2 - x1), y1), (x2, y2))
  | otherwise = mat

countDangerous :: IntMatrix -> Int
countDangerous = length . filter ((>=2) . snd) . Map.toList

solve :: [LineCoords] -> Int
solve = countDangerous . foldl updateMap emptyMap

updateMap' :: IntMatrix -> LineCoords -> IntMatrix
updateMap' mat ((x1, y1), (x2, y2))
  | x1 == x2 && y1 == y2 = incrMat mat (x1, y1)
  | otherwise = updateMap' (incrMat mat (x1, y1)) ((x1 + signum (x2 - x1), y1 + signum (y2 - y1)), (x2, y2))

solvePart2 :: [LineCoords] -> Int
solvePart2 = countDangerous . foldl updateMap' emptyMap

main = do
    file <- readFile "/tmp/day5-input.txt"
    print (solve $ parseInput file)
    print (solvePart2 $ parseInput file)
