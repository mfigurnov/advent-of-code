import System.IO

fn lst = sum $ map (\(x,y) -> if x < y then 1 else 0) $ zip lst (tail lst)

main = do
    contents <- readFile "day1-input.txt"
    let lst = map read $ words contents :: [Int]
    print (fn lst)
