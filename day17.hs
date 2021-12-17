type Coords = (Int, Int)
type DoubleCoords = (Int, Int, Int, Int)

step :: DoubleCoords -> DoubleCoords
step (x, y, vx, vy) = (x + vx, y + vy, vx - signum vx, vy - 1)

traj :: Coords -> [Coords]
traj (vx, vy) = map (\(x,y,vx,vy) -> (x,y)) $ iterate step (0, 0, vx, vy)

trajUntilOvershoot :: DoubleCoords -> Coords -> [Coords]
trajUntilOvershoot bounds v = takeWhile (notOvershoot bounds) (traj v)
  where notOvershoot (min_x, max_x, min_y, max_y) (x, y) = x <= max_x && y >= min_y

isBetween :: DoubleCoords -> Coords -> Bool
isBetween (min_x, max_x, min_y, max_y) (x, y) =
  min_x <= x && x <= max_x && min_y <= y && y <= max_y

isHit :: DoubleCoords -> [Coords] -> Bool
isHit bounds = any (isBetween bounds)

maxY :: [Coords] -> Int
maxY [] = -100000
maxY c = (maximum . map snd) c

testBounds = (20, 30, -10, -5) :: DoubleCoords
testV = (7, 2) :: Coords

scanRange bounds = [(vx, vy) | vy <- [min_y..max_y_range], vx <- [0..max_x]]
  where (min_x, max_x, min_y, max_y) = bounds
        max_y_range = max_x + min_y  -- a very loose bound on the maximum valid y

solve bounds = maximum
  $ map maxY
  $ filter (isHit bounds)
  $ map (trajUntilOvershoot bounds) (scanRange bounds)

solvePart2 bounds = length
  $ filter (isHit bounds)
  $ map (trajUntilOvershoot bounds) (scanRange bounds)

main = do
    -- hard-coding the inputs today
    let bounds = (195, 238, -93, -67) :: DoubleCoords
    print (solve bounds)
    print (solvePart2 bounds)
