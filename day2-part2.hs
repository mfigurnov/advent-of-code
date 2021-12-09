import System.IO

processStep :: (Int, Int, Int) -> String -> Int -> (Int, Int, Int)
processStep (hrz, depth, aim) command value
  | command == "forward" = (hrz + value, depth + aim * value, aim)
  | command == "down" = (hrz, depth, aim + value)
  | command == "up" = (hrz, depth, aim - value)
  | otherwise = (0, 0, 0)

getCommand :: String -> String
getCommand = head . words

getValue :: String -> Int
getValue s = read $ words s !! 1

processString :: (Int, Int, Int) -> String -> (Int, Int, Int)
processString coords s = processStep coords (getCommand s) (getValue s)

solve :: [String] -> (Int, Int, Int)
solve = foldl processString (0, 0, 0)

main = do
    file <- readFile "/tmp/day2-input.txt"
    let (hrz, depth, _) = solve $ lines file
    print (hrz * depth)

