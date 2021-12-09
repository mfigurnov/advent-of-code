import Data.Functor.Identity
import Data.Char
import Data.Either
import Data.Maybe
import Data.List
import System.IO
import Text.Parsec
import Text.Parsec.String

testStr = "acedgfb cdfbe gcdfa fbcad dab cefabd cdfgeb eafb cagedb ab | cdfeb fcadb cdfeb cdbaf\nbe cfbegad cbdgef fgaecd cgeb fdcge agebfd fecdb fabcd edb | fdgacbe cefdb cefbgd gcbe\nedbfga begcd cbg gc gcadebf fbgde acbgfd abcde gfcbed gfec | fcgedb cgb dgebacf gc"

inputParser :: ParsecT String () Identity [([String], [String])]
inputParser =
  do lines <- sepBy line eol
     eof
     return lines
eol = char '\n'
line =
  do x <- sort . filter (not . null) <$> sepBy chars (char ' ')
     string "| "
     y <- sepBy chars (char ' ')
     return (x, y)
chars = many (oneOf "abcdefg")

parseInput :: String -> [([String], [String])]
parseInput = fromRight [] . parse inputParser "(unknown)"

digitLines :: Int -> String
digitLines 0 = "abcefg"
digitLines 1 = "cf"
digitLines 2 = "acdeg"
digitLines 3 = "acdfg"
digitLines 4 = "bcdf"
digitLines 5 = "abdfg"
digitLines 6 = "abdefg"
digitLines 7 = "acf"
digitLines 8 = "abcdefg"
digitLines 9 = "abcdfg"

numDigitLines :: Int -> Int
numDigitLines = length . digitLines

isSimpleDigit :: String -> Bool
isSimpleDigit s = length s `elem` (numDigitLines <$> [1, 4, 7, 8])

solve :: [([String], [String])] -> Int
solve s = sum $ fromEnum . isSimpleDigit <$> concat (snd <$> s)

-- Part 2
permuteStr :: String -> String -> String
permuteStr perm s = [perm !! (ord c - ord 'a') | c <- s]

permDigitLines :: String -> Int -> String
permDigitLines perm i = sort $ permuteStr perm (digitLines i)

allPermutations = [[permDigitLines perm i | i <- [0..9]] |
                   perm <- permutations ['a'..'g']]
allSortedPermutations = sort <$> allPermutations

findMatch :: [String] -> Int
findMatch s = fromJust $ elemIndex (sort (map sort s)) allSortedPermutations

decode :: Int -> [String] -> [Int]
decode matchIdx s = fromJust .
  (`elemIndex` (allPermutations !! matchIdx))
  <$> map sort s

decimalToNum :: [Int] -> Int
decimalToNum = foldl1 (\x y -> 10*x+y)

solvePart2Row :: ([String], [String]) -> Int
solvePart2Row (a, b) = decimalToNum $ decode (findMatch a) b

solvePart2 :: [([String], [String])] -> Int
solvePart2 = sum . map solvePart2Row

main = do
    file <- readFile "/tmp/day8-input.txt"
    print (solve $ parseInput file)
    print (solvePart2 $ parseInput file)
