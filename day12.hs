import Data.Functor.Identity
import Data.Char
import Data.Either
import qualified Data.Map as Map
import Data.Maybe
import Data.List
import Data.Tuple
import System.IO
import Text.Parsec
import Text.Parsec.String

testStr = "start-A\nstart-b\nA-c\nA-b\nb-d\nA-end\nb-end"
testInput = parseInput testStr

inputParser :: ParsecT String () Identity [(String, String)]
inputParser =
  do lines <- sepBy line (char '\n')
     eof
     return lines
line :: ParsecT String () Identity (String, String)
line =
  do x <- many1 (noneOf "-\n")
     char '-'
     y <- many1 (noneOf "-\n")
     return (x, y)

parseInput :: String -> [(String, String)]
parseInput = fromRight [] . parse inputParser "(unknown)"

type Graph = Map.Map String [String]

makeGraph :: [(String, String)] -> Graph
makeGraph e = Map.fromList
  $ map (\x -> (fst $ head x, map snd x))
  $ groupBy (\x y -> fst x == fst y)
  $ sort (e ++ map swap e)

-- Only visit each small cave once
validExtension :: [String] -> String -> Bool
validExtension p e = not (all isLower e) || notElem e p

-- Paths are extended by appending at the head
extendPath :: Graph -> [String] -> [[String]]
extendPath _ p@("end":ps) = [p]
extendPath g p@(p1:_) = [e:p | e <- g Map.! p1, validExtension p e]

findAllPaths g = go [["start"]]
  where go ps = if all (\t -> head t == "end") ps then ps
                else go $ concatMap (extendPath g) ps

solve :: [(String, String)] -> Int
solve e = length $ findAllPaths $ makeGraph e

-- Part 2

-- Only visit each small cave once
validExtension' :: [String] -> String -> Bool
validExtension' p "start" = False  -- never visit the start again
validExtension' p e = not (all isLower e)
  || e `notElem` p
  || (length (nub (e:lower)) == length lower)  -- can visit the lowercase caves twice
  where lower = filter (all isLower) p

extendPath' :: Graph -> [String] -> [[String]]
extendPath' _ p@("end":ps) = [p]
extendPath' g p@(p1:_) = [e:p | e <- g Map.! p1, validExtension' p e]

findAllPaths' g = go [["start"]]
  where go ps = if all (\t -> head t == "end") ps then ps
                else go $ concatMap (extendPath' g) ps

solvePart2 :: [(String, String)] -> Int
solvePart2 e = length $ findAllPaths' $ makeGraph e

main = do
    file <- readFile "/tmp/day12-input.txt"
    print (solve $ parseInput file)
    print (solvePart2 $ parseInput file)
