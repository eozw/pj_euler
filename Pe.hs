
module Pe where

-- fibonacci sequence
fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

-- prime numbers
primes = 2:3:f [3] [5,7..]
  where
    f (x:xs) ys =
      let (ps, qs) = span (< x^2) ys
      in  ps ++ f (xs ++ ps) [z | z <- qs, mod z x /= 0]

-- factors
factors n = factors' n primes []
  where
    factors' n (p:ps) fs =
      let (q,r) = quotRem n p
      in case (q,r) of
        (1,0) -> p:fs
        (_,0) -> factors' q (p:ps) (p:fs)
        (_,_) -> factors' n ps fs

-- palindrome
toList 0 = []
toList n =
  let (q,r) = quotRem n 10
  in r : toList q

isPalindrome n =
  let ns = toList n
  in ns == reverse ns

-- quickSort
{- use sort, List.sortBy
quickSort _ [] = []
quickSort isLarger (x:xs) =
  let (nls,ls) = divideList xs [] []
  in quickSort isLarger nls ++ [x] ++ quickSort isLarger ls
  where
    divideList [] as bs = (as,bs)
    divideList (y:ys) as bs =
      if isLarger x y
        then divideList ys as (y:bs)
        else divideList ys (y:as) bs
 -}


