-- Enter your code here. Read input from STDIN. Print output to STDOUT
import Prelude
import Control.Monad

type Point = (Int, Int)
type Lines = (Point,Point)

-- Create Input

readpolygondots :: IO [Point]
readpolygondots = do
    text <- getLine
    let n = read text :: Int
    replicateM n readPair

readPair :: IO Point
readPair = do
    text <- getLine
    let [z1, z2] = words text
    return (read z1 :: Int, read z2 :: Int)

-- Modify input container

pointstolines :: [Point] -> [Lines]
pointstolines points = zip points (tail $ cycle points)

pythagoras :: Point -> Point -> Float
pythagoras x y = sqrt $ fromIntegral ( a^2 + b^2 )
  where
    a = (fst x - fst y)
    b = (snd x - snd y)



perimeter :: [Point] -> Float
perimeter = sum. fmap (uncurry pythagoras).pointstolines

main :: IO ()
main = do
    testCases <- readpolygondots
    print $ perimeter testCases



    

 



