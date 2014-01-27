
module Pe where

import Data.Ord(compare)

-- fibonacci sequence
fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

-- prime numbers
primes = 2:3:f [3] [5,7..]
  where
    f (x:xs) ys =
      let (ps, qs) = span (< x^2) ys
      in  ps ++ f (xs ++ ps) [z | z <- qs, mod z x /= 0]

-- factors
factors 1 = []
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

-- union of two lists
union x [] = x
union [] y = y
union (x:xs) (y:ys) =
  case (compare x y) of
    LT -> x : union    xs (y:ys)
    EQ -> x : union    xs    ys
    GT -> y : union (x:xs)   ys

-- Tree
data Tree a = Tree a (Tree a) (Tree a) deriving Show
Tree a lt rt !!! n
  | n == 0    = a
  | odd n     = lt !!! div n 2
  | otherwise = rt !!! ((div n 2) - 1)
instance Functor Tree where
  fmap f (Tree a lt rt) = Tree (f a) (fmap f lt) (fmap f rt)
natTree = Tree 0 (fmap ((+1).(*2)) natTree) (fmap ((*2).(+1)) natTree)

