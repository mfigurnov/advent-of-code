import Data.Functor.Identity
import Data.Either
import Data.List
import qualified Data.Set as Set
import System.IO
import Text.Parsec
import Text.Parsec.String

testStr = "6,10\n0,14\n9,10\n0,3\n10,4\n4,11\n6,0\n6,12\n4,1\n0,13\n10,12\n3,4\n3,0\n8,4\n1,10\n2,14\n8,10\n9,0\n\nfold along y=7\nfold along x=5"

inputParser :: ParsecT String () Identity ([(Int, Int)], [(Char, Int)])
inputParser =
  do pointLines <- sepBy pointLine eolButNotEolEol
     count 2 eol
     foldLines <- sepBy foldLine eol
     eof
     return (pointLines, foldLines)
integer :: Parser Int
integer = (read :: String -> Int) <$> many1 digit
pointLine =
  do x <- integer
     char ','
     y <- integer
     return (x, y)
foldLine =
  do string "fold along "
     axis <- oneOf "xy"
     char '='
     line <- integer
     return (axis, line)
eol = char '\n'
eolButNotEolEol = do
  lookAhead $ notFollowedBy $ string "\n\n"
  char '\n'

parseInput :: String -> ([(Int, Int)], [(Char, Int)])
parseInput = fromRight ([], []) . parse inputParser "(unknown)"

showMap :: [(Int, Int)] -> IO ()
showMap m = putStr
  $ unlines
  $ [[ch $ Set.member (x, y) s | x <- [0..maxx]] | y <- [0..maxy]]
  where maxx = maximum $ map fst m
        maxy = maximum $ map snd m
        s = Set.fromList m
        ch False = '.'
        ch True = '#'

fold :: Int -> Int -> Int
fold val y = if y > val then 2 * val - y else y

unique :: Ord a => [a] -> [a]
unique = Set.toList . Set.fromList

foldAlong :: Char -> Int -> [(Int, Int)] -> [(Int, Int)]
foldAlong 'x' val m = unique $ map (\(x, y) -> (fold val x, y)) m
foldAlong 'y' val m = unique $ map (\(x, y) -> (x, fold val y)) m

solve :: ([(Int, Int)], [(Char, Int)]) -> Int
solve (m, f:fs) = length $ foldMany m [f]  --uncurry foldAlong f m

foldMany :: [(Int, Int)] -> [(Char, Int)] -> [(Int, Int)]
-- foldMany = foldl' (\m' f -> uncurry foldAlong f m')
foldMany = foldl' (flip $ uncurry foldAlong)

solvePart2 :: ([(Int, Int)], [(Char, Int)]) -> IO ()
solvePart2 (m, fs) = showMap $ foldMany m fs

main = do
    file <- readFile "/tmp/day13-input.txt"
    print (solve $ parseInput file)
    solvePart2 $ parseInput file
