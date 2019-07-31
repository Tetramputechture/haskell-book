module Division where

data DividedResult =
    Result Integer
  | DividedByZero
      deriving (Eq, Show)

dividedBy :: Integral a => a -> a -> DividedResult
dividedBy num denom = go num denom 0
  where go n  d count
         | d == 0 = DividedByZero
         | abs(n) < abs(d) = Result count
         | (n < 0) /= (d < 0) = 
             go (n + d) d (count - 1)
         | otherwise =
             go (n - d) d (count + 1)
