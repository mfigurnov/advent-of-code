import Data.Functor.Identity
import Data.Either
import Data.List
import System.IO
import Text.Parsec
import Text.Parsec.String

testStr = "3,4,3,1,2"

inputParser :: ParsecT String () Identity [Int]
inputParser =
  do lines <- sepBy integer (char ',')
     eof
     return lines
integer :: Parser Int
integer = (read :: String -> Int) <$> many1 digit

parseInput :: String -> [Int]
parseInput = fromRight [] . parse inputParser "(unknown)"

tick :: Int -> Int
tick 0 = 6
tick n = n - 1

naiveStep :: [Int] -> [Int]
naiveStep xs = map tick xs ++ replicate (length (filter (0==) xs)) 8

solve :: Int -> [Int] -> Int
solve n xs = length $ iterate naiveStep xs !! n

type Counts = [Int]

incElement :: [Int] -> Int -> [Int]
incElement (n:ns) 0 = (n+1) : ns
incElement (n:ns) x = n : incElement ns (x-1)

convertInputToCounts :: [Int] -> Counts
convertInputToCounts = foldl' incElement (replicate 9 0)

optStep :: Counts -> Counts
optStep (n0:ns) = take 6 ns ++ [(ns !! 6) + n0, ns !! 7, n0]

solveOpt :: Int -> Counts -> Int
solveOpt n cs = sum $ iterate optStep cs !! n

main = do
    file <- readFile "/tmp/day6-input.txt"
    print (solveOpt 80 $ convertInputToCounts $ parseInput file)
    print (solveOpt 256 $ convertInputToCounts $ parseInput file)
