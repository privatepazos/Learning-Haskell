-- Enter your code here. Read input from STDIN. Print output to STDOUT

import Prelude
import Control.Monad

type Point = (Float, Float)
type Line = (Point,Point)

-- Creates Input

readpolygondots :: IO [Point]
readpolygondots = do
    text <- getLine
    let n = read text :: Int
    replicateM n readPair

readPair :: IO Point
readPair = do
    text <- getLine
    let [z1, z2] = words text
    return (read z1 :: Float, read z2 :: Float)

-- Modifies input container

pointstoLine :: [Point] -> [Line]
pointstoLine points = zip points (tail $ cycle points)

-- Sum Areas
-- Note that base changes sign with the direction of the integral. This is key so that the underlying areas add up or
-- substract each other. This is the critical point of this algorithm.

arealine :: Point -> Point -> Float
arealine x y = base*avheight
  where
    base = fst y - fst x
    avheight = abs(snd x + snd y)/2

areavector :: [Point] -> [Float]
areavector = fmap (uncurry arealine).pointstoLine


-- Program does stuff

main :: IO ()
main = do
    testCases <- readpolygondots
    print $ abs.sum $ areavector testCases
