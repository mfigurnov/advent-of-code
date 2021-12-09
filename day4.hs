{-# LANGUAGE TupleSections #-}

import Data.Either
import Data.List
import System.IO
import Text.Parsec
import Text.Parsec.String

testStr = "7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1\n\n22 13 17 11  0\n 8  2 23  4 24\n21  9 14 16  7\n 6 10  3 18  5\n 1 12 20 15 19\n\n 3 15  0  2 22\n 9 18 13 17  5\n19  8  7 25 23\n20 11 10 24  4\n14 21 16 12  6\n\n14 21 17 24  4\n10 16 15  9 19\n18  8 23 26 20\n22 11 13  6  5\n 2  0 12  3  7"

inputParser =
  do draws <- sepBy integer (char ',')
     eol
     eol
     boards <- sepBy board (string "\n\n")
     eof
     return (draws, boards)
board = sepBy boardLine eolButNotEolEol
boardLine = do
  skipMany (char ' ')
  sepBy integer (skipMany (char ' '))
integer :: Parser Int
integer = (read :: String -> Int) <$> many1 digit
eol = char '\n'
eolButNotEolEol = do
  lookAhead $ notFollowedBy $ string "\n\n"
  char '\n'

parseInput = fromRight ([], [[[]]]) . parse inputParser "(unknown)"

map2d :: (a -> b) -> [[a]] -> [[b]]
map2d fn = map (map fn)

type Marked = (Int, Bool)
emptyBoard :: [[Int]] -> [[Marked]]
emptyBoard = map2d (, False)

playBoard :: Int -> [[Marked]] -> [[Marked]]
playBoard draw = map2d $ \(value, marked) -> (value, marked || value == draw)

anyRowIsFilled :: [[Marked]] -> Bool
-- 'any and' is magic suggested by the linter. Thanks?..
anyRowIsFilled = any and . map2d snd

isWinningBoard :: [[Marked]] -> Bool
isWinningBoard b = anyRowIsFilled b || anyRowIsFilled (transpose b)

sumUnmarked :: [[Marked]] -> Int
sumUnmarked b = sum [value | (value, marked) <- concat b, not marked]

playTheGame :: [Int] -> Int -> [[Marked]] -> (Int, Int)
playTheGame [] n b = (n, -10000)  -- the board never won :(
playTheGame (d:ds) n b = if isWinningBoard newboard then
    (n, sumUnmarked newboard * d)
    else playTheGame ds (n+1) newboard
  where newboard = playBoard d b

solve :: ([Int], [[[Int]]]) -> Int
solve (draws, boards) = snd $ minimum -- the first winning board
    $ map (playTheGame draws 0 . emptyBoard) boards

solvePart2 :: ([Int], [[[Int]]]) -> Int
solvePart2 (draws, boards) = snd $ maximum -- the last winning board
    $ map (playTheGame draws 0 . emptyBoard) boards

main = do
    file <- readFile "/tmp/day4-input.txt"
    print (solve $ parseInput file)
    print (solvePart2 $ parseInput file)
