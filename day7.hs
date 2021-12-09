import Data.Functor.Identity
import Data.Either
import Data.List
import System.IO
import Text.Parsec
import Text.Parsec.String

testStr = "16,1,2,0,4,2,7,1,2,14"

inputParser :: ParsecT String () Identity [Int]
inputParser =
  do lines <- sepBy integer (char ',')
     eof
     return lines
integer :: Parser Int
integer = (read :: String -> Int) <$> many1 digit

parseInput :: String -> [Int]
parseInput = fromRight [] . parse inputParser "(unknown)"

medians :: [Int] -> [Int]
medians l@(_:_:_:_) = medians (init (tail l))
medians (x:xs) = [x]

median :: [Int] -> Int
median = head . medians . sort

solve :: [Int] -> Int
solve xs = sum . map (abs . subtract (median xs)) . sort $ xs

-- Part 2
cost :: Int -> Int
cost dx = (abs dx * (abs dx + 1)) `div` 2

totalCost :: [Int] -> Int -> Int
totalCost xs m = sum . map (cost . subtract m) $ xs

solvePart2 :: [Int] -> Int
solvePart2 xs = minimum [totalCost xs m | m <- [minimum xs .. maximum xs]]

main = do
    file <- readFile "/tmp/day7-input.txt"
    print (solve $ parseInput file)
    print (solvePart2 $ parseInput file)
