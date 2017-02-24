{-
Extract the Grey body spectrum of graphite at different temperatures
-}

import Prelude
import Data.Char
import Control.Monad

type Spectrum = [Int]

-- Constants of the Universe (In SI Units)

h = 6.626e-34 -- m2 kg / s
c = 3e8       -- m/s
k = 1.380e-23 -- m2 kg s-2 K-1


greybodyspec :: Double -> Double -> Double
greybodyspec lambda t = 2*h*f^^3/(c^^2) *1/(exp(h*f/(k*t))-1)
  where f = h*c/lambda


main :: IO ()
main = do
  print $ greybodyspec 1000e-9 300