import Data.List
import System.IO

testStr = "00100\n11110\n10110\n10111\n10101\n01111\n00111\n11100\n10000\n11001\n00010\n01010"

parseRow :: String -> [Int]
parseRow = map (read . (:[]))

parseMatrix :: String -> [[Int]]
parseMatrix m = parseRow <$> lines m

count :: Int -> [Int] -> Int
count el = length . filter (== el)

leastCommonDigit :: [Int] -> Int
leastCommonDigit m = if count 0 m < count 1 m then 0 else 1

mostCommonDigit :: [Int] -> Int
mostCommonDigit m = if count 0 m >= count 1 m then 0 else 1

mostCommonBits :: String -> [Int]
mostCommonBits m = map mostCommonDigit $ transpose $ parseMatrix m

leastCommonBits :: String -> [Int]
leastCommonBits m = map leastCommonDigit $ transpose $ parseMatrix m

binToDec :: [Int] -> Int
binToDec = foldr (\elt carry -> elt + carry * 2) 0 . reverse

findGamma :: String -> Int
findGamma = binToDec . mostCommonBits

findEta :: String -> Int
findEta = binToDec . leastCommonBits

main = do
    file <- readFile "/tmp/day3-input.txt"
    let gamma = findGamma file
    let eta = findEta file
    print (gamma * eta)
