
module Pe where

-- fibonacci sequence
fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

-- prime numbers
primes = 2:3:f [3] [5,7..]
  where
    f (x:xs) ys =
      let (ps, qs) = span (< x^2) ys
      in  ps ++ f (xs ++ ps) [z | z <- qs, mod z x /= 0]


