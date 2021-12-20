{-# LANGUAGE DeriveFunctor, DeriveFoldable #-}

import Data.Functor.Identity
import Data.List
import Data.Maybe
import Control.Monad (void)
import System.IO
import Text.Parsec
import Text.Parsec.String

-- testStr = "[[[[1,2],[3,4]],[[5,6],[7,8]]],9]"
testStr = "[[[0,[5,8]],[[1,7],[9,6]]],[[4,[1,2]],[[1,4],2]]]\n[[[5,[2,8]],4],[5,[[9,9],0]]]\n[6,[[[6,2],[5,6]],[[7,6],[4,7]]]]\n[[[6,[0,7]],[0,9]],[4,[9,[9,0]]]]\n[[[7,[6,4]],[3,[1,3]]],[[[5,5],1],9]]\n[[6,[[7,3],[3,2]]],[[[3,8],[5,7]],4]]\n[[[[5,4],[7,7]],8],[[8,3],8]]\n[[9,3],[[9,9],[6,[4,9]]]]\n[[2,[[7,7],7]],[[5,8],[[9,3],[0,2]]]]\n[[[[5,2],5],[8,[3,7]]],[[5,[7,5]],[4,4]]]"

data FishNumber a = Leaf a | Pair (FishNumber a) (FishNumber a)
  deriving (Eq, Show, Functor, Foldable)

-- Parsing
parseWith :: GenParser Char () a -> String -> a
parseWith parser body =
  case parse parser "[input]" body of
    Right x -> x
    Left e -> error (show e)

parseLeaf :: ParsecT String () Identity (FishNumber Int)
parseLeaf = Leaf <$> integer

parseFishNumbers :: ParsecT String () Identity [FishNumber Int]
parseFishNumbers = parseFishNumber `sepBy` char '\n'

parseFishNumber :: ParsecT String () Identity (FishNumber Int)
parseFishNumber = do
  void $ char '['
  x <- try parseLeaf <|> parseFishNumber
  void $ char ','
  y <- try parseLeaf <|> parseFishNumber
  void $ char ']'
  return (Pair x y)

integer :: Parser Int
integer = (read :: String -> Int) <$> many1 digit

-- Explode
addIds :: FishNumber Int -> FishNumber (Int, Int)
addIds fishNumber = fst $ go fishNumber [0..]
  where go (Leaf n) (x:xs) = (Leaf (n, x), xs)
        go (Pair l r) xs = (Pair l' r', xs'')
          where (l', xs') = go l xs
                (r', xs'') = go r xs'

removeIds :: FishNumber (Int, Int) -> FishNumber Int
removeIds = fmap fst

valueAt :: Int -> FishNumber (Int, Int) -> Maybe Int
valueAt i m = fst <$> find (\(val, i') -> i == i') m

addAt :: Int -> Int -> FishNumber (Int, Int) -> FishNumber (Int, Int)
addAt i val = fmap (\(val', i') -> (if i == i' then val + val' else val', i'))

findExplode :: FishNumber a -> Maybe a
findExplode f = go f 0
  where go (Leaf x) n = if n >= 5 then Just x else Nothing
        go (Pair l r) n = if isJust l' then l' else r'
          where l' = go l (n+1)
                r' = go r (n+1)

replaceWithZero :: Int -> FishNumber (Int, Int) -> FishNumber (Int, Int)
replaceWithZero idx = go
  where go (Leaf x) = Leaf x
        go (Pair (Leaf (a, i)) (Leaf (b, j))) =
          if i == idx then Leaf (0, -1) else Pair (Leaf (a, i)) (Leaf (b, j))
        go (Pair l r) = Pair (go l) (go r)

explode :: FishNumber Int -> FishNumber Int
explode f = go (findExplode f')
  where f' = addIds f
        go Nothing = f
        go (Just (valueIdxL, idx)) = removeIds
          $ replaceWithZero idx
          $ addAt (idx - 1) valueIdxL
          $ addAt (idx + 2) valueIdxR f'
            where valueIdxR = fromJust (valueAt (idx+1) f')

-- Split
findSplit :: FishNumber (Int, Int) -> Maybe (Int, Int)
findSplit (Leaf (x, i)) = if x >= 10 then Just (x, i) else Nothing
findSplit (Pair l r) = if isJust goLeft then goLeft else goRight
  where goLeft = findSplit l
        goRight = findSplit r

replaceLeafWith :: Int -> FishNumber (Int, Int) -> FishNumber (Int, Int) -> FishNumber (Int, Int)
replaceLeafWith idx new = go
  where go (Leaf (x, i)) = if i == idx then new else Leaf (x, i)
        go (Pair l r) = Pair (go l) (go r)

split :: FishNumber Int -> FishNumber Int
split f = go (findSplit f')
  where f' = addIds f
        go Nothing = f
        go (Just (value, idx)) = removeIds
          $ replaceLeafWith idx (addIds $ splitValue value) f'
        splitValue n = Pair (Leaf (floor halfN)) (Leaf (ceiling halfN))
          where halfN = fromIntegral n / 2

-- Reduce
reduce f = if f /= f'' then reduce f'' else f
  where f' = explode f
        f'' = if f == f' then split f else f'

-- Add a list of fish numbers
addTwo :: FishNumber Int -> FishNumber Int -> FishNumber Int
addTwo l r = reduce (Pair l r)

addList :: [FishNumber Int] -> FishNumber Int
addList = foldl1 addTwo

-- Compute the magnitude of the result
magnitude :: FishNumber Int -> Int
magnitude (Pair l r) = 3 * magnitude l + 2 * magnitude r
magnitude (Leaf n) = n

solve :: String -> Int
solve s = magnitude $ addList $ parseWith parseFishNumbers s

solvePart2 :: String -> Int
solvePart2 s = maximum [magnitude (addTwo x y) | x <- ns, y <- ns, x /= y]
  where ns = parseWith parseFishNumbers s

main = do
    file <- readFile "/tmp/day18-input.txt"
    print (solve file)
    print (solvePart2 file)
