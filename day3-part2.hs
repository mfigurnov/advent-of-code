import Data.Ord
import Data.List
import Data.Maybe
import System.IO

testStr = "00100\n11110\n10110\n10111\n10101\n01111\n00111\n11100\n10000\n11001\n00010\n01010"

parseRow :: String -> [Int]
parseRow = map (read . (:[]))

parseMatrix :: String -> [[Int]]
parseMatrix m = parseRow <$> lines m

count :: Int -> [Int] -> Int
count el = length . filter (== el)

leastCommonDigit :: [Int] -> Int
leastCommonDigit m = if count 0 m <= count 1 m then 0 else 1

mostCommonDigit :: [Int] -> Int
mostCommonDigit m = if count 0 m > count 1 m then 0 else 1

binToDec :: [Int] -> Int
binToDec = foldr (\elt carry -> elt + carry * 2) 0 . reverse

getColumn :: Int -> [[Int]] -> [Int]
getColumn idx = map (!! idx)

filterBits :: ([Int] -> Int) -> Int -> [[Int]] -> (Int, [[Int]])
filterBits fn idx [] = (idx, [])
filterBits fn idx [row] = (idx, [row])
filterBits fn idx rows = filterBits fn (idx + 1) $
  filter (\row -> (row !! idx) == (fn . getColumn idx) rows) rows

solveOxygen :: String -> Int
solveOxygen = binToDec . head . snd . filterBits mostCommonDigit 0 . parseMatrix

solveCO2 :: String -> Int
solveCO2 = binToDec . head . snd . filterBits leastCommonDigit 0 . parseMatrix

main = do
    file <- readFile "/tmp/day3-input.txt"
    let oxygen = solveOxygen file
    let co2 = solveCO2 file
    print (oxygen * co2)
