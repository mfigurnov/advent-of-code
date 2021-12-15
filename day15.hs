import Data.Char (digitToInt)
import Data.Functor.Identity
import Data.Either
import Data.List
import qualified Data.Map as Map
import Data.Maybe
import qualified Data.Set as Set
import System.IO
import Text.Parsec
import Text.Parsec.String

testStr = "1163751742\n1381373672\n2136511328\n3694931569\n7463417111\n1319128137\n1359912421\n3125421639\n1293138521\n2311944581"
m = parseInput testStr

type IntMatrix = Map.Map (Int, Int) Int
type PathMatrix = Map.Map (Int, Int) (Int, (Int, Int))
type VertexQueue = Set.Set (Int, (Int, Int))

infinity :: Int
infinity = 1000000000

inputParser :: ParsecT String () Identity [[Int]]
inputParser =
  do lines <- sepBy integers eol
     eof
     return lines
integers :: Parser [Int]
integers = many1 (digitToInt <$> digit)
eol = char '\n'

parseInput :: String -> IntMatrix
parseInput s = fromRight Map.empty $ makeMat <$> parse inputParser "(unknown)" s

enumerate = zip [0..]

makeMat :: [[Int]] -> IntMatrix
makeMat m = Map.fromList [
    ((i, j), elt) | (i, row) <- enumerate m, (j, elt) <- enumerate row]

neighbors :: (Int, Int) -> [(Int, Int)]
neighbors (i, j) = [(i-1, j), (i+1, j), (i, j-1), (i, j+1)]

initPaths :: IntMatrix -> PathMatrix
initPaths m = Map.insert (0, 0) (0, (-1, -1)) $
  Map.map (const (infinity, (-1, -1))) m

initVertexQueue :: PathMatrix -> VertexQueue
initVertexQueue p = Set.fromList [(v, k) | (k, (v, _)) <- Map.toList p]

dijkstraIter :: IntMatrix -> (PathMatrix, VertexQueue) -> (PathMatrix, VertexQueue)
dijkstraIter costs (p, q) = (p', q')
  where (du, u) = Set.findMin q
        -- Compute the updated paths
        p_update = Map.fromList [(v, (dv', u)) |
          v <- neighbors u,
          v `Map.member` p, -- only valid neighbors
          let dv' = du + (costs Map.! v), dv' < fst (p Map.! v)]
        -- Update the path matrix and the queue based on p_update
        p'= Map.union p_update p
        q' = Set.union (initVertexQueue p_update) (Set.deleteMin q)

-- Computes the path matrix for (0, 0) -> (i, j)
dijkstra :: IntMatrix -> PathMatrix
dijkstra costs = fst $ until (Set.null . snd) (dijkstraIter costs) (p, q)
  where p = initPaths costs
        q = initVertexQueue p

solve :: IntMatrix -> Int
solve costs = fst $ dijkstra costs Map.! fst (Map.findMax costs)

tileValue :: Int -> Int -> Int -> Int
tileValue x dx dy = ((x - 1 + dx + dy) `mod` 9) + 1

-- Assumes that the matrix is square!
tileMat :: Int -> IntMatrix -> IntMatrix
tileMat num mat = Map.fromList [
    ((i + size * dx, j + size * dy), tileValue v dx dy) |
    ((i, j), v) <- Map.toList mat, dx <- [0..(num-1)], dy <- [0..(num-1)]]
  where size = (fst . fst . Map.findMax) mat + 1

solvePart2 :: IntMatrix -> Int
solvePart2 = solve . tileMat 5

main = do
    file <- readFile "/tmp/day15-input.txt"
    print (solve $ parseInput file)
    print (solvePart2 $ parseInput file)
