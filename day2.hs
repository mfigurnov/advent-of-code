import System.IO

processStep :: (Int, Int) -> String -> Int -> (Int, Int)
processStep (hrz, depth) command value
  | command == "forward" = (hrz + value, depth)
  | command == "down" = (hrz, depth + value)
  | command == "up" = (hrz, depth - value)
  | otherwise = (0, 0)

getCommand :: String -> String
getCommand = head . words

getValue :: String -> Int
getValue s = read $ words s !! 1

processString :: (Int, Int) -> String -> (Int, Int)
processString coords s = processStep coords (getCommand s) (getValue s)

solve :: [String] -> (Int, Int)
solve = foldl processString (0, 0)

main = do
    file <- readFile "/tmp/day2-input.txt"
    let (hrz, depth) = solve $ lines file
    print (hrz * depth)
