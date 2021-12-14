import Data.Functor.Identity
import Data.Either
import Data.List
import qualified Data.Map as Map
import System.IO
import Text.Parsec
import Text.Parsec.String

testStr = "NNCB\n\nCH -> B\nHH -> N\nCB -> H\nNH -> C\nHB -> C\nHC -> B\nHN -> C\nNN -> C\nBH -> H\nNC -> B\nNB -> B\nBN -> B\nBB -> N\nBC -> B\nCC -> N\nCN -> C"

inputParser :: ParsecT String () Identity (String, [((Char, Char), Char)])
inputParser =
  do initStr <- many (noneOf "\n")
     count 2 eol
     subs <- subLine `sepBy` eol
     eof
     return (initStr, subs)
subLine :: ParsecT String () Identity ((Char, Char), Char)
subLine =
  do c1 <- letter
     c2 <- letter
     string " -> "
     target <- letter
     return ((c1, c2), target)
eol = char '\n'

parseInput :: String -> (String, [((Char, Char), Char)])
parseInput = fromRight ([], []) . parse inputParser "(unknown)"

-- Brute force solution (sufficient for part 1)
process :: Ord t => [((t, t), t)] -> [t] -> [t]
process subs (x:xs) = go x xs
  where go prev [] = [prev]
        go prev (x':xs') = case Map.lookup (prev, x') subsMap of
            Nothing -> prev : go x' xs'
            Just c -> prev : c : go x' xs'
        subsMap = Map.fromList subs

getCharCounts :: String -> [Int]
getCharCounts = map length <$> group . sort

solve :: (String, [((Char, Char), Char)]) -> Int
solve (initStr, subs) = (maximum charCounts) - (minimum charCounts)
  where finalStr = iterate (process subs) initStr !! 10
        charCounts = getCharCounts finalStr

-- An efficient solution (required for part 2)
-- Key idea: track the number of *sequential pairs* of characters under the substitution
type CharCounts = Map.Map Char Integer
type PairCounts = Map.Map (Char, Char) Integer

strToPairs :: String -> PairCounts
strToPairs = go Map.empty
  where go m [] = m
        go m [_] = m
        go m (a:b:xs) = go (addToMap m (a, b) 1) (b:xs)

addToMap :: (Ord k, Num a) => Map.Map k a -> k -> a -> Map.Map k a
addToMap m k v = Map.alter fn k m
  where fn (Just v') = Just (v + v')
        fn Nothing = Just v

step :: [((Char, Char), Char)] -> PairCounts -> PairCounts
step subs p = foldl' fn Map.empty (Map.toList p)
  where fn m ((c1, c2), v) = case Map.lookup (c1, c2) subsMap of
            Nothing -> addToMap m (c1, c2) v
            Just c -> addToMap (addToMap m (c1, c) v) (c, c2) v
        subsMap = Map.fromList subs

-- We count up the characters by considering the second elements of each pair,
-- and that covers all characters except the first one, which we add separately.
pairCountsToCounts :: String -> PairCounts -> CharCounts
pairCountsToCounts s p = addToMap secondCharMap (head s) 1
  where fn m ((c1, c2), v) = addToMap m c2 v
        secondCharMap = foldl' fn Map.empty (Map.toList p)

solve' :: Int -> (String, [((Char, Char), Char)]) -> Integer
solve' cnt (initStr, subs) = (maximum charCounts) - (minimum charCounts)
  where finalCounts = iterate (step subs) (strToPairs initStr) !! cnt
        charCounts = Map.elems (pairCountsToCounts initStr finalCounts)

main = do
    file <- readFile "/tmp/day14-input.txt"
    print (solve' 10 $ parseInput file)
    print (solve' 40 $ parseInput file)
