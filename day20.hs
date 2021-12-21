import Data.Functor.Identity ( Identity )
import Data.Maybe
import qualified Data.Map as Map
import Control.Monad (void)
import Text.Parsec
import Text.Parsec.String

-- Matrix
type BoolVector = Map.Map Int Bool
type BoolMatrix = Map.Map (Int, Int) Bool

enumerate :: [a] -> [(Int, a)]
enumerate = zip [0..]

makeVec :: [Bool] -> BoolVector
makeVec m = Map.fromList $ enumerate m

makeMat :: [[Bool]] -> BoolMatrix
makeMat m = Map.fromList [((i, j), elt) | (i, row) <- enumerate m, (j, elt) <- enumerate row]

charToBool :: Char -> Bool
charToBool c
  | c == '#' = True
  | c == '.' = False
  | otherwise = undefined

parseWith :: GenParser Char () a -> String -> a
parseWith parser body =
  case parse parser "[input]" body of
    Right x -> x
    Left e -> error (show e)

boolList :: ParsecT String () Identity [Bool]
boolList = do
    s <- many (oneOf "#.")
    return (charToBool <$> s)

inputParser :: ParsecT String () Identity (BoolVector, BoolMatrix)
inputParser = do
    algo <- boolList
    void $ count 2 eol
    image <- boolList `sepBy` eol
    return (makeVec algo, makeMat image)

eol :: ParsecT String () Identity Char
eol = char '\n'

-- Convolutional logic
binToDec :: [Int] -> Int
binToDec = foldr (\elt carry -> elt + carry * 2) 0 . reverse

neighbors :: (Int, Int) -> [(Int, Int)]
neighbors (i, j) = [(i-1, j-1), (i-1, j), (i-1, j+1),
                    (i, j-1), (i, j), (i, j+1),
                    (i+1, j-1), (i+1, j), (i+1, j+1)]

extractPatch :: BoolMatrix -> (Int, Int) -> Maybe Int
extractPatch m xy = fmap (binToDec . fmap fromEnum) (mapM (`Map.lookup` m) $ neighbors xy)

lookupPatch :: BoolVector -> Maybe Int -> Maybe Bool
lookupPatch algo (Just idx) = Map.lookup idx algo
lookupPatch algo Nothing = Nothing

convolve :: BoolVector -> BoolMatrix -> BoolMatrix
convolve algo m = Map.mapMaybeWithKey (\xy _ -> lookupPatch algo (extractPatch m xy)) m

pad :: Int -> BoolMatrix -> BoolMatrix
pad p m = Map.fromList [
    ((i, j), fromMaybe False (Map.lookup (i, j) m)) | i <- [-p .. maxX + p], j <- [-p .. maxY + p]]
    where maxX = maximum (map fst (Map.keys m))
          maxY = maximum (map snd (Map.keys m))

numOn :: BoolMatrix -> Int
numOn m = sum (map fromEnum $ Map.elems m)

solve :: (BoolVector, BoolMatrix) -> Int
solve (algo, mat) = numOn $ iterate (convolve algo) (pad 5 mat) !! 2

solvePart2 :: (BoolVector, BoolMatrix) -> Int
solvePart2 (algo, mat) = numOn $ iterate (convolve algo) (pad 110 mat) !! 50

main :: IO ()
main = do
    file <- readFile "/tmp/day20-input.txt"
    print (solve $ parseWith inputParser file)
    print (solvePart2 $ parseWith inputParser file)
