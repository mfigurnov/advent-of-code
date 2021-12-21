import qualified Data.Map as Map
import Data.Maybe

data State = State {pos :: Int,
                    score :: Int}
    deriving (Show, Eq)

detDice :: [Int]
detDice = 1 : map (\x -> 1 + (x `mod` 100)) detDice

updatePosScore :: Int -> (Int, Int) -> (Int, Int)
updatePosScore sumDice (p, s) = (p', s + p')
  where p' = 1 + (p + sumDice - 1) `mod` 10

play :: ([Int], State) -> ([Int], State)
play (d, state) = (t, State p' s')
  where (h, t) = splitAt 3 d
        (p', s') = updatePosScore (sum h) (pos state, score state)

playTwo :: ([Int], State, State, Int, Bool) -> ([Int], State, State, Int, Bool)
playTwo (d, s1, s2, nRolls, False) = (d', s1', s2, nRolls + 3, True)
  where (d', s1') = play (d, s1)
playTwo (d, s1, s2, nRolls, True) = (d', s1, s2', nRolls + 3, False)
  where (d', s2') = play (d, s2)

gameOver :: ([Int], State, State, Int, Bool) -> Bool
gameOver (_, s1, s2, _, _) = (score s1 >= 1000) || (score s2 >= 1000)

finalStates :: (Int, Int) -> (State, State, Int)
finalStates (p1, p2) = (s1', s2', nRolls')
  where s1 = State p1 0
        s2 = State p2 0
        (_, s1', s2', nRolls', _) =
          until gameOver playTwo (detDice, s1, s2, 0, False)

partOneScore :: (State, State, Int) -> Int
partOneScore (s1, s2, nRolls) = nRolls * min (score s1) (score s2)

solve :: (Int, Int) -> Int
solve = partOneScore . finalStates

-- Part 2
data FullState = FullState {
  pos1 :: Int,
  score1 :: Int,
  pos2 :: Int,
  score2 :: Int,
  turn :: Bool  -- False -> player 1 plays; True -> player 2 plays
}
    deriving (Show, Eq, Ord)

-- Mapping of game states to the number of wins of player 1
type StateMap = Map.Map FullState Integer

step :: FullState -> Int -> FullState
step st sumDice = case turn st of
  False -> FullState pos1' score1' (pos2 st) (score2 st) True
    where (pos1', score1') = updatePosScore sumDice (pos1 st, score1 st)
  True -> FullState (pos1 st) (score1 st) pos2' score2' False
    where (pos2', score2') = updatePosScore sumDice (pos2 st, score2 st)

gameScore st
  | score1 st >= 21 = Just 1
  | score2 st >= 21 = Just 0
  | otherwise = Nothing

sumDices :: [Int]
sumDices = [r1 + r2 + r3 | r1 <- [1..3], r2 <- [1..3], r3 <- [1..3]]

memoSum :: StateMap -> FullState -> [Int] -> (StateMap, Integer)
memoSum m st [] = (m, 0)
memoSum m st [d] = (m', n')
  where (m', n') = memoRecursive m (step st d)
memoSum m st (d:ds) = (m'', n' + n'')
  where (m', n') = memoRecursive m (step st d)
        (m'', n'') = memoSum m' st ds

memoRecursive :: StateMap -> FullState -> (StateMap, Integer)
memoRecursive m st = case gameScore st of
  Just x -> (m, x)
  Nothing ->
    case Map.lookup st m of
      Just n -> (m, n)  -- already memoised
      Nothing -> (Map.insert st n' m', n')
        where (m', n') = memoSum m st sumDices

solvePart2 :: (Int, Int) -> Integer
solvePart2 (p1, p2) = snd $ memoRecursive Map.empty (FullState p1 0 p2 0 False)

main = do
    let pos = (10, 9)
    print (solve pos)
    print (solvePart2 pos)
