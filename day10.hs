import Data.Functor.Identity
import Data.Either
import Data.List
import qualified Data.Map as Map
import System.IO
import Text.Parsec
import Text.Parsec.String

testStr = "[({(<(())[]>[[{[]{<()<>>\n[(()[<>])]({[<{<<[]>>(\n{([(<{}[<>[]}>{[]{[(<()>\n(((({<>}<{<{<>}{[]{[]{}\n[[<[([]))<([[{}[[()]]]\n[{[{({}]{}}([{[{{{}}([]\n{<[[]]>}<{[{[{[]{()[[[]\n[<(<(<(<{}))><([]([]()\n<{([([[(<>()){}]>(<<{{\n<{([{{}}[<[[[<>{}]]]>[]]"

inputParser :: ParsecT String () Identity [String]
inputParser = many (noneOf "\n") `sepBy` char '\n'

parseInput :: String -> [String]
parseInput = fromRight [] . parse inputParser "(unknown)"

matching :: Map.Map Char Char
matching = Map.fromList [
    ('(', ')'),
    ('[', ']'),
    ('{', '}'),
    ('<', '>')]

scores :: Map.Map Char Int
scores = Map.fromList [
    (')', 3),
    (']', 57),
    ('}', 1197),
    ('>', 25137)]

-- process args: stack string
process :: String -> String -> Int
process _ [] = 0 -- success or incomplete string
process st (b:bs)
  -- opening
  | b `Map.member` matching = process ((matching Map.! b):st) bs
  -- closing that matches
  | not (null st) && (head st == b) = process (tail st) bs
  -- error, closing that does not match
  | otherwise = scores Map.! b

solve :: [String] -> Int
solve = sum . map (process [])

process' :: String -> String -> String
process' [] [] = [] -- success
process' st [] = st -- incomplete string, return the stack
process' st (b:bs)
  -- opening
  | b `Map.member` matching = process' ((matching Map.! b):st) bs
  -- closing that matches
  | not (null st) && (head st == b) = process' (tail st) bs
  -- error, closing that does not match
  | otherwise = []

scores' :: Map.Map Char Int
scores' = Map.fromList [
    (')', 1),
    (']', 2),
    ('}', 3),
    ('>', 4)]

totalScore' :: String -> Int
totalScore' = foldl (\x y -> 5 * x + (scores' Map.! y)) 0

median :: Ord a => [a] -> a
median xs = ys !! m
  where ys = sort xs
        m = length ys `div` 2

solvePart2 :: [String] -> Int
solvePart2 = median . filter (>0) . map (totalScore' . process' [])

main = do
    file <- readFile "/tmp/day10-input.txt"
    print (solve $ parseInput file)
    print (solvePart2 $ parseInput file)
