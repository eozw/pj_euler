
module Pe where

-- fibonacci sequence
fibs = 0 : 1 : zipWith (+) fibs (tail fibs)



