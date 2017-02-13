{-
For a given integer , print the first  rows of Pascal's Triangle. Print each row with each value separated by a single space. 
The value at the  row and  column of the triangle is equal to  where indexing starts from . These values are the binomial coefficients.
-}
import Prelude
import Data.Char
import Control.Monad

type Row = [Int]


fac n 
  | n == 0 = 1
  | n > 0 = n*fac (n-1)


pascal :: Int -> Int -> Int
pascal row col =  fac (row) `div` (fac(col)*fac(row-col))

pasrow :: Int -> Row
pasrow x = map (pascal x) [0..x]

pasarray :: Int -> [Row]
pasarray x = map pasrow [0..x-1]

-- Only thing left: Get the output format right. From [Row] -> String (including newlines)
lststr :: Row -> [Char]
lststr k = map chr k

rowtostr :: Row -> String
rowtostr row = (unwords $ map (show) row) ++ "\n"

main :: IO ()
main = do
  k <- getLine
  mapM_ putStr (map rowtostr (pasarray (read k)))
