
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

{- numbering rule
> (\ls -> zip ls $ map f ls) [0..10]
f n | n == 0     = []
    | odd n      = 'L': (f $ div n 2)
    | otherwise  = 'R': (f $ (div n 2) -1)
 -}
{-
add 1, then see lower digits
0  =    0 = ""    ->    1
1  =    1 = "L"   ->   1L
2  =   10 = "R"   ->   1R
3  =   11 = "LL"  ->  1LL
4  =  100 = "RL"  ->  1LR
5  =  101 = "LR"  ->  1RL
6  =  110 = "RR"  ->  1RR
7  =  111 = "LLL" -> 1LLL
8  = 1000 = "RLL" -> 1LLR
9  = 1001 = "LRL" -> 1LRL
10 = 1010 = "RRL"

9 = 1001 -> 9+1 = 1010 -> L
9/2 = 4 = 100 -> 4+1 = 101 -> R
4/2 = 2 = 10 -> 2+1 = 11 -> R

     9 -> 1001 -> 1010 -> L
-> 9/2 ->  100 ->  101 -> R
-> 4/2-1 ->  1 ->   10 -> R
-> 0
 -}

instance Functor Tree where
  fmap f (Tree a lt rt) = Tree (f a) (fmap f lt) (fmap f rt)

natTree = Tree 0 (fmap ((+1).(*2)) natTree) (fmap ((*2).(+1)) natTree)
collatzTree = Tree (0,0) ((fmap fl) collatzTree) ((fmap fr) collatzTree)
  where
    fl (n,_) = let m = 2*n + 1 in (m, fc m)
    fr (n,_) = let m = 2*n + 2 in (m, fc m)
    fc m | m == 1    = 1
         | even m    = (+1) . snd $ (collatzTree !!! (div m 2)        )
         | otherwise = (+2) . snd $ (collatzTree !!! (3*(div m 2) + 2))
--         | otherwise = (+1) . snd $ (collatzTree !!! (3 * m + 1)      )


-- Zipper for Tree (Learn you a Haskell for great good)
data Crumb a = LeftCrumb a (Tree a)
             | RightCrumb a (Tree a)
             deriving Show
type Crumbs a = [Crumb a]

goLeft (Tree a l r, cs) = (l, LeftCrumb a r:cs)
goRight (Tree a l r, cs) = (r, RightCrumb a l:cs)
goUp (t, LeftCrumb a r:cs) = (Tree a t r, cs)
goUp (t, RightCrumb a l:cs) = (Tree a l t, cs)

type Zipper a = (Tree a, Crumbs a)

modify f (Tree a l r, cs) = (Tree (f a) l r, cs)
attach t (_, cs) = (t, cs)
topMost (t, []) = (t, [])
topMost z = topMost (goUp z)

