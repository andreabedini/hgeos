module GEOSTest.Arith
    ( frange
    , mfloor
    ) where

mfloor :: Double -> Double -> Double
mfloor m x = (fromInteger $ floor (x / m)) * m

frange :: Double -> Double -> Double -> [Double]
frange lower upper step =
    let count = (upper - lower) / step
    in [lower + (fromIntegral i) * step | i <- [0..(round count - 1)]]
