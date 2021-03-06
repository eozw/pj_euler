
import Data.List(sortBy)
import Data.Ord(compare)
import Data.Char(digitToInt)
import qualified Pe

{-
 - 0.
 -}

q0 = undefined

--


{-
 - 1.
 -
 - If we list all the natural numbers below 10 that are multiples of
 - 3 or 5, we get 3, 5, 6 and 9. The sum of these multiples is 23.
 -
 - Find the sum of all the multiples of 3 or 5 below 1000.
 -}

q1 = sum [x | x <- [1..(1000-1)], x `mod` 3 == 0 || x `mod` 5 == 0]

-- 233168


{-
 - 2.
 -
 - Each new term in the Fibonacci sequence is generated
 - by adding the previous two terms. By starting with 1 and 2,
 - the first 10 terms will be:
 -
 -    1, 2, 3, 5, 8, 13, 21, 34, 55, 89, ...
 -
 - By considering the terms in the Fibonacci sequence
 - whose values do not exceed four million, find the sum of
 - the even-valued terms.
 -}

q2 = sum $ takeWhile (<=4000000) $ [x | x <- Pe.fibs, even x]

-- 4613732


{-
 - 3.
 -
 - The prime factors of 13195 are 5, 7, 13 and 29.
 -
 - What is the largest prime factor of the number 600851475143 ?
 -}

q3' = head . Pe.factors $ 13195
q3= head . Pe.factors $ 600851475143

-- 6857


{-
 - 4.
 -
 - A palindromic number reads the same both ways.
 - The largest palindrome made from the product of two 2-digit numbers
 - is 9009 = 91 × 99.
 -
 - Find the largest palindrome made from the product of two 3-digit numbers.
 -}

q4a n =
  let l = 10^(n-1) + 1
      u = 10^n - 1
      ls = [(x,y,z) | x <- [l..u], y <- [l..u], x <= y, let z = x * y, Pe.isPalindrome z]
  in sortBy cmp ls
  where
    cmp (_,_,a) (_,_,b) = compare a b

q4' = (\(_,_,x) -> x) . head . reverse . q4a $ 2
q4 = (\(_,_,x) -> x) . head . reverse . q4a $ 3

-- 906609

{-
 - 5.
 -
 - 2520 is the smallest number that can be divided by each of the numbers
 - from 1 to 10 without any remainder.
 -
 - What is the smallest positive number that is evenly divisible by all of
 - the numbers from 1 to 20?
 -}

q5a n = map (reverse . Pe.factors) [2..n]
q5b n = foldl Pe.union [] . q5a $ n
q5c n =
  let ls = q5b n
  in (product ls, ls)

q5' = fst . q5c $ 10
q5 = fst . q5c $ 20

-- 232792560


{-
 - 6.
 -
 - The sum of the squares of the first ten natural numbers is,
 -    1^2 + 2^2 + ... + 10^2 = 385
 -
 - The square of the sum of the first ten natural numbers is,
 -    (1 + 2 + ... + 10)^2 = 552 = 3025
 -
 - Hence the difference between the sum of the squares of the first ten
 - natural numbers and the square of the sum is 3025 - 385 = 2640.
 -
 - Find the difference between the sum of the squares of the first one
 - hundred natural numbers and the square of the sum.
 -}

q6a n =
  let ls = [1..n]
      x = (^2) . sum $ ls
      y = sum . map (^2) $ ls
  in x - y

q6' = q6a 10
q6 = q6a 100

-- 25164150


{-
 - 7.
 -
 - By listing the first six prime numbers: 2, 3, 5, 7, 11, and 13,
 - we can see that the 6th prime is 13.
 -
 - What is the 10 001st prime number?
 -}

q7a n = head . drop (n-1) $ Pe.primes

q7' = q7a 6
q7 = q7a 10001

-- 104743


{-
 - 8.
 -
 - Find the greatest product of five consecutive digits
 - in the 1000-digit number.
 -
 -  (see following answer)
 -}

q8a =
    "73167176531330624919225119674426574742355349194934"
  ++"96983520312774506326239578318016984801869478851843"
  ++"85861560789112949495459501737958331952853208805511"
  ++"12540698747158523863050715693290963295227443043557"
  ++"66896648950445244523161731856403098711121722383113"
  ++"62229893423380308135336276614282806444486645238749"
  ++"30358907296290491560440772390713810515859307960866"
  ++"70172427121883998797908792274921901699720888093776"
  ++"65727333001053367881220235421809751254540594752243"
  ++"52584907711670556013604839586446706324415722155397"
  ++"53697817977846174064955149290862569321978468622482"
  ++"83972241375657056057490261407972968652414535100474"
  ++"82166370484403199890008895243450658541227588666881"
  ++"16427171479924442928230863465674813919123162824586"
  ++"17866458359124566529476545682848912883142607690042"
  ++"24219022671055626321111109370544217506941658960408"
  ++"07198403850962455444362981230987879927244284909188"
  ++"84580156166097919133875499200524063689912560717606"
  ++"05886116467109405077541002256983155200055935729725"
  ++"71636269561882670428252483600823257530420752963450"
q8b = map digitToInt q8a
q8c n xs@(y:ys) =
  let (hs,ts) = splitAt n xs
      m = length hs
  in if m<n then []
            else product hs : (q8c n ys)

q8 = maximum . q8c 5 $ q8b

-- 40824


{-
 - 9.
 -
 - A Pythagorean triplet is a set of three natural numbers, a < b < c,
 - for which,
 -
 -    a^2 + b^2 = c^2
 -
 - For example, 3^2 + 4^2 = 9 + 16 = 25 = 5^2.
 -
 - There exists exactly one Pythagorean triplet for which a + b + c = 1000.
 - Find the product abc.
 -}

q9a n = [(a,b,c) | a <- [1..n], b <- [(a+1)..n]
                  , let a2b2 = a^2+b^2 ; c = floor $ sqrt $ fromIntegral a2b2
                  , a2b2==c^2]
q9b n = filter (\(_,_,_,d)-> n==d) . map (\(a,b,c) -> (a,b,c,a+b+c)) . q9a $ n

q9 = head [a*b*c | (a,b,c,_) <- q9b 1000]

-- 31875000
