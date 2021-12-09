import System.IO

slidingSum3 lst = map (\(x,y,z)->x+y+z) $ zip3 lst (tail lst) (tail (tail lst))
countIncreasing lst = sum $ map (\(x,y) -> if x < y then 1 else 0) $ zip lst (tail lst)

main = do
    contents <- readFile "day1-input.txt"
    let lst = map read $ words contents :: [Int]
    print (countIncreasing (slidingSum3 lst))
