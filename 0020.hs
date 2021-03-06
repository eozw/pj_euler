
import Data.List(foldl', maximumBy)
import Data.Ord(comparing)
import Data.Array((!),listArray,assocs)
import Data.Ratio((%),numerator)
import Data.Char(digitToInt)
import qualified Pe

{-
 - 10.
 -
 - The sum of the primes below 10 is 2 + 3 + 5 + 7 = 17.
 -
 - Find the sum of all the primes below two million.
 -}

q0a n = takeWhile (<n) Pe.primes

q0' = sum $ q0a 10
q0 = sum $ q0a 2000000
{-
*Main> q0
142913828922
(30.78 secs, 4434541520 bytes)
 -}

-- 142913828922


{-
 - 11.
 -
 - In the 20�~20 grid below, four numbers along a diagonal line have
 - been marked in red.
 -
 -  (see following answer)
 -
 - The product of these numbers is 26 �~ 63 �~ 78 �~ 14 = 1788696.
 -
 - What is the greatest product of four adjacent numbers in the same
 - direction (up, down, left, right, or diagonally) in the 20�~20 grid?
 -}

q1a =
  [[08,02,22,97,38,15,00,40,00,75,04,05,07,78,52,12,50,77,91,08]
  ,[49,49,99,40,17,81,18,57,60,87,17,40,98,43,69,48,04,56,62,00]
  ,[81,49,31,73,55,79,14,29,93,71,40,67,53,88,30,03,49,13,36,65]
  ,[52,70,95,23,04,60,11,42,69,24,68,56,01,32,56,71,37,02,36,91]
  ,[22,31,16,71,51,67,63,89,41,92,36,54,22,40,40,28,66,33,13,80]
  ,[24,47,32,60,99,03,45,02,44,75,33,53,78,36,84,20,35,17,12,50]
  ,[32,98,81,28,64,23,67,10,26,38,40,67,59,54,70,66,18,38,64,70]
  ,[67,26,20,68,02,62,12,20,95,63,94,39,63,08,40,91,66,49,94,21]
  ,[24,55,58,05,66,73,99,26,97,17,78,78,96,83,14,88,34,89,63,72]
  ,[21,36,23,09,75,00,76,44,20,45,35,14,00,61,33,97,34,31,33,95]
  ,[78,17,53,28,22,75,31,67,15,94,03,80,04,62,16,14,09,53,56,92]
  ,[16,39,05,42,96,35,31,47,55,58,88,24,00,17,54,24,36,29,85,57]
  ,[86,56,00,48,35,71,89,07,05,44,44,37,44,60,21,58,51,54,17,58]
  ,[19,80,81,68,05,94,47,69,28,73,92,13,86,52,17,77,04,89,55,40]
  ,[04,52,08,83,97,35,99,16,07,97,57,32,16,26,26,79,33,27,98,66]
  ,[88,36,68,87,57,62,20,72,03,46,33,67,46,55,12,32,63,93,53,69]
  ,[04,42,16,73,38,25,39,11,24,94,72,18,08,46,29,32,40,62,76,36]
  ,[20,69,36,41,72,30,23,88,34,62,99,69,82,67,59,85,74,04,36,16]
  ,[20,73,35,29,78,31,90,01,74,31,49,71,48,86,81,16,23,57,05,54]
  ,[01,70,54,71,83,51,54,69,16,92,33,48,61,43,52,01,89,19,67,48]]
data Q1 = S | E | SE | SW deriving Show
q1b (i,j) = q1a!!i!!j
q1c base dir =
  case dir of
    S  -> ((fst base)+1,  snd base   )
    E  -> ( fst base,    (snd base)+1)
    SE -> ((fst base)+1, (snd base)+1)
    SW -> ((fst base)+1, (snd base)-1)
q1d base dir len
  | len == 0 = []
  | len >  0 = q1b base : q1d (q1c base dir) dir (len - 1)
q1e m =
  let n = length q1a
      n' = n - 1
      m' = m - 1
  in [((i,j),dir) | i <- [0..n'], j <- [0..n'], dir <- [S,E,SE,SW]
                  , case dir of
                      S  -> i + m' < n
                      E  ->               j + m' <  n
                      SE -> i + m' < n && j + m' <  n
                      SW -> i + m' < n && j - m' >= 0]
q1f m = [q1d base dir m|(base, dir) <- q1e m]
q1g m = map product $ q1f m

q1' = product $ q1d (6,8) SE 4
q1 = maximum . map product . q1f $ 4

-- 70600674


{-
 - 12.
 -
 - The sequence of triangle numbers is generated by adding the natural numbers.
 - So the 7th triangle number would be 1 + 2 + 3 + 4 + 5 + 6 + 7 = 28.
 - The first ten terms would be:
 -
 -    1, 3, 6, 10, 15, 21, 28, 36, 45, 55, ...
 -
 - Let us list the factors of the first seven triangle numbers:
 -
 -     1: 1
 -     3: 1,3
 -     6: 1,2,3,6
 -    10: 1,2,5,10
 -    15: 1,3,5,15
 -    21: 1,3,7,21
 -    28: 1,2,4,7,14,28
 -
 - We can see that 28 is the first triangle number to have over five divisors.
 -
 - What is the value of the first triangle number to have over five hundred
 - divisors?
 -}

q2a = tail $ scanl (+) 0 [1..]
q2b n = [(m,q) | m <- [1..(f n)], let (q,r) = quotRem n m, r == 0]
  where
    f = floor . sqrt . fromInteger
q2c pair =
  if fst pair == snd pair
    then [fst pair]
    else [fst pair, snd pair]
q2d = concat . map q2c . q2b
q2e n = head [ls | ls <- map q2d q2a, length ls > n]
q2f = head . tail . q2e

-- q2' = q2f 5
-- q2 = q2f 500
-- (63.55 secs, 10503447460 bytes)

-- [3,2,2,2] -> [[3],[2,2,2]]
q2g zss [] = zss
q2g [[]] (x:xs) = q2g [[x]] xs
q2g zss@(ys:yss) (x:xs) =
  let y = head ys
  in if x == y
      then q2g ((y:ys):yss) xs
      else q2g ([x]:zss) xs
q2h = product . map ((1+) . length) . q2g [[]] . Pe.factors
q2i n = head . dropWhile (\p -> snd p <=n) . \xs -> zip xs $ map q2h xs

q2' = fst $ q2i 5 q2a
q2 = fst $ q2i 500 q2a
-- (9.58 secs, 1342993636 bytes)

-- 76576500


{-
 - 13.
 -
 - Work out the first ten digits of the sum of the following one-hundred
 - 50-digit numbers.
 -
 -  (see following answer)
 -
 -}

q3a :: [Integer]
q3a =
  [37107287533902102798797998220837590246510135740250
  ,46376937677490009712648124896970078050417018260538
  ,74324986199524741059474233309513058123726617309629
  ,91942213363574161572522430563301811072406154908250
  ,23067588207539346171171980310421047513778063246676
  ,89261670696623633820136378418383684178734361726757
  ,28112879812849979408065481931592621691275889832738
  ,44274228917432520321923589422876796487670272189318
  ,47451445736001306439091167216856844588711603153276
  ,70386486105843025439939619828917593665686757934951
  ,62176457141856560629502157223196586755079324193331
  ,64906352462741904929101432445813822663347944758178
  ,92575867718337217661963751590579239728245598838407
  ,58203565325359399008402633568948830189458628227828
  ,80181199384826282014278194139940567587151170094390
  ,35398664372827112653829987240784473053190104293586
  ,86515506006295864861532075273371959191420517255829
  ,71693888707715466499115593487603532921714970056938
  ,54370070576826684624621495650076471787294438377604
  ,53282654108756828443191190634694037855217779295145
  ,36123272525000296071075082563815656710885258350721
  ,45876576172410976447339110607218265236877223636045
  ,17423706905851860660448207621209813287860733969412
  ,81142660418086830619328460811191061556940512689692
  ,51934325451728388641918047049293215058642563049483
  ,62467221648435076201727918039944693004732956340691
  ,15732444386908125794514089057706229429197107928209
  ,55037687525678773091862540744969844508330393682126
  ,18336384825330154686196124348767681297534375946515
  ,80386287592878490201521685554828717201219257766954
  ,78182833757993103614740356856449095527097864797581
  ,16726320100436897842553539920931837441497806860984
  ,48403098129077791799088218795327364475675590848030
  ,87086987551392711854517078544161852424320693150332
  ,59959406895756536782107074926966537676326235447210
  ,69793950679652694742597709739166693763042633987085
  ,41052684708299085211399427365734116182760315001271
  ,65378607361501080857009149939512557028198746004375
  ,35829035317434717326932123578154982629742552737307
  ,94953759765105305946966067683156574377167401875275
  ,88902802571733229619176668713819931811048770190271
  ,25267680276078003013678680992525463401061632866526
  ,36270218540497705585629946580636237993140746255962
  ,24074486908231174977792365466257246923322810917141
  ,91430288197103288597806669760892938638285025333403
  ,34413065578016127815921815005561868836468420090470
  ,23053081172816430487623791969842487255036638784583
  ,11487696932154902810424020138335124462181441773470
  ,63783299490636259666498587618221225225512486764533
  ,67720186971698544312419572409913959008952310058822
  ,95548255300263520781532296796249481641953868218774
  ,76085327132285723110424803456124867697064507995236
  ,37774242535411291684276865538926205024910326572967
  ,23701913275725675285653248258265463092207058596522
  ,29798860272258331913126375147341994889534765745501
  ,18495701454879288984856827726077713721403798879715
  ,38298203783031473527721580348144513491373226651381
  ,34829543829199918180278916522431027392251122869539
  ,40957953066405232632538044100059654939159879593635
  ,29746152185502371307642255121183693803580388584903
  ,41698116222072977186158236678424689157993532961922
  ,62467957194401269043877107275048102390895523597457
  ,23189706772547915061505504953922979530901129967519
  ,86188088225875314529584099251203829009407770775672
  ,11306739708304724483816533873502340845647058077308
  ,82959174767140363198008187129011875491310547126581
  ,97623331044818386269515456334926366572897563400500
  ,42846280183517070527831839425882145521227251250327
  ,55121603546981200581762165212827652751691296897789
  ,32238195734329339946437501907836945765883352399886
  ,75506164965184775180738168837861091527357929701337
  ,62177842752192623401942399639168044983993173312731
  ,32924185707147349566916674687634660915035914677504
  ,99518671430235219628894890102423325116913619626622
  ,73267460800591547471830798392868535206946944540724
  ,76841822524674417161514036427982273348055556214818
  ,97142617910342598647204516893989422179826088076852
  ,87783646182799346313767754307809363333018982642090
  ,10848802521674670883215120185883543223812876952786
  ,71329612474782464538636993009049310363619763878039
  ,62184073572399794223406235393808339651327408011116
  ,66627891981488087797941876876144230030984490851411
  ,60661826293682836764744779239180335110989069790714
  ,85786944089552990653640447425576083659976645795096
  ,66024396409905389607120198219976047599490197230297
  ,64913982680032973156037120041377903785566085089252
  ,16730939319872750275468906903707539413042652315011
  ,94809377245048795150954100921645863754710598436791
  ,78639167021187492431995700641917969777599028300699
  ,15368713711936614952811305876380278410754449733078
  ,40789923115535562561142322423255033685442488917353
  ,44889911501440648020369068063960672322193204149535
  ,41503128880339536053299340368006977710650566631954
  ,81234880673210146739058568557934581403627822703280
  ,82616570773948327592232845941706525094512325230608
  ,22918802058777319719839450180888072429661980811197
  ,77158542502016545090413245809786882778948721859617
  ,72107838435069186155435662884062257473692284509516
  ,20849603980134001723930671666823555245252804609722
  ,53503534226472524250874054075591789781264330331690]

q3b n = take n . show . sum . map fromIntegral $ q3a

q3 = q3b 10

-- 5537376230


{-
 - 14.
 -
 -
 - The following iterative sequence is defined for the set of positive
 - integers:
 -
 -    n �� n/2 (n is even)
 -    n �� 3n + 1 (n is odd)
 -
 - Using the rule above and starting with 13, we generate the following
 - sequence:
 -
 -    13 �� 40 �� 20 �� 10 �� 5 �� 16 �� 8 �� 4 �� 2 �� 1
 -
 - It can be seen that this sequence (starting at 13 and finishing at 1)
 - contains 10 terms. Although it has not been proved yet (Collatz Problem),
 - it is thought that all starting numbers finish at 1.
 -
 - Which starting number, under one million, produces the longest chain?
 -
 - NOTE: Once the chain starts the terms are allowed to go above one million.
 -}

q4a n c
  | n == 1    = c
  | even n    = q4a (div n 2) (c+1)
  | otherwise = q4a (3*n + 1) (c+1)
q4b n c -- same performance (not improved)
  | n == 1    = c
  | even n    = q4b (div n 2)         (c+1)
  | otherwise = q4b (3*(div n 2) + 2) (c+2)
q4c f n =foldl' max' (1,1) [(m, f m 1) | m <- [1..n]]
  where
    max' x y = if snd x < snd y then y else x
{-
*Main> q4c q4a 1000000
(837799,525)
(260.83 secs, 41607568896 bytes)
*Main> q4c q4b 1000000
(837799,525)
(196.16 secs, 32010488692 bytes)
 -}
q4d = fmap (\n -> q4e n) Pe.natTree
q4e n
  | n == 1    = 1
  | even n    = q4d Pe.!!! (div n 2)
  | otherwise = q4b (3*(div n 2) + 2) 3
q4f n = foldl' max' (1,1) [(m, q4d Pe.!!! m) | m <- [1..n]]
  where
    max' x y = if snd x < snd y then y else x
{-
*Main> q4f 1000000
(837799,525)
(153.89 secs, 24147770048 bytes)
 -}
q4g = fmap (\n -> q4h n) Pe.natTree
q4h n
  | n == 1    = 1
  | even n    = (1+) $ q4g  Pe.!!! (div n 2)
  | otherwise = q4h' (3*(div n 2) + 2) 2
  where
    q4h' m d
      | m == 1    = d
      | m < n     = (d+) $ q4g Pe.!!! m
      | even m    = q4h' (div m 2) (d+1)
      | otherwise = q4h' (3*(div m 2) + 2) (d+2)
q4i n = foldl' max' (1,1) [(m, q4g Pe.!!! m) | m <- [1..n]]
  where
    max' x y = if snd x < snd y then y else x
{-
*Main> q4i 1000000
(837799,525)
(79.33 secs, 11096009152 bytes)
 -}
q4j n = foldl' max' (1,1) [Pe.collatzTree Pe.!!! m | m <- [1..n]]
  where
    max' x y = if snd x < snd y then y else x
{-
*Main> q4j 1000000
(837799,525)
(108.22 secs, 14865069648 bytes)
 -}
q4k nmax = maximumBy (comparing snd) (assocs arr)
  where
    arr = listArray (1,nmax) (1:[f n n 0 | n <- [2..nmax]])
    f mmax m c
      | m < mmax  = (arr ! m) + c
      | even m    = f mmax (div m 2) (c+1)
      | otherwise = f mmax (3*(div m 2) + 2) (c+2)

q4 = q4k 1000000
{-
*Main> q4
(837799,525)
(10.89 secs, 1663133160 bytes)
 -}

-- 837799


{-
 - 15.
 -
 - Starting in the top left corner of a 2�~2 grid, and only being able to
 - move to the right and down, there are exactly 6 routes to the bottom right
 - corner.
 -
 - How many such routes are there through a 20�~20 grid?
 -}

q5 = numerator . product $ map (\n -> n % (n-20)) [21..40]

-- 137846528820


{-
 - 16.
 -
 - 2^15 = 32768 and the sum of its digits is 3 + 2 + 7 + 6 + 8 = 26.
 -
 - What is the sum of the digits of the number 2^1000?
 -}

q6 = sum . map digitToInt . show $ 2^1000

-- 1366


{-
 - 17.
 -
 - If the numbers 1 to 5 are written out in words: one, two, three, four,
 - five, then there are 3 + 3 + 5 + 4 + 4 = 19 letters used in total.
 -
 - If all the numbers from 1 to 1000 (one thousand) inclusive were written
 - out in words, how many letters would be used?
 -
 - NOTE: Do not count spaces or hyphens. For example, 342 (three hundred
 - and forty-two) contains 23 letters and 115 (one hundred and fifteen)
 - contains 20 letters. The use of "and" when writing out numbers is in
 - compliance with British usage.
 -}


q7a n
  | n == 1000
    = length "onethousand"
  | n >= 100
    = (q7a (div n 100)) + (length "hundredand") + (q7a (mod n 100))
    - if (mod n 100) == 0 then (length "and") else 0
  | n >= 20
    = length (l2!!(div n 10)) + length (l1!!(mod n 10))
  | otherwise
    = length (l1!!n)
  where
  l1 = [""       ,"one"    ,"two"      ,"three"   ,"four"
       ,"five"   ,"six"    ,"seven"    ,"eight"   ,"nine"
       ,"ten"    ,"eleven" ,"twelve"   ,"thirteen","fourteen"
       ,"fifteen","sixteen","seventeen","eighteen","nineteen"]
  l2 = [""       ,""       ,"twenty"   ,"thirty"  ,"forty"
       ,"fifty"  ,"sixty"  ,"seventy"  ,"eighty"  ,"ninety"]
q7 = sum $ map q7a [1..1000]

-- 21124


{-
 - 18.
 -
 - By starting at the top of the triangle below and moving to adjacent
 - numbers on the row below, the maximum total from top to bottom is 23.
 -
 -  (see following q8a)
 -
 - That is, 3 + 7 + 4 + 9 = 23.
 -
 - Find the maximum total from top to bottom of the triangle below:
 -
 -  (see following q8b)
 -
 - NOTE: As there are only 16384 routes, it is possible to solve this problem
 - by trying every route. However, Problem 67, is the same challenge with a
 - triangle containing one-hundred rows; it cannot be solved by brute force,
 - and requires a clever method! ;o)
 -}

q8a =
  [[3]
  ,[7,4]
  ,[2,4,6]
  ,[8,5,9,3]]
q8b =
  [[75]
  ,[95,64]
  ,[17,47,82]
  ,[18,35,87,10]
  ,[20,04,82,47,65]
  ,[19,01,23,75,03,34]
  ,[88,02,77,73,07,63,67]
  ,[99,65,04,28,06,16,70,92]
  ,[41,41,26,56,83,40,80,70,33]
  ,[41,48,72,33,47,32,37,16,94,29]
  ,[53,71,44,65,25,43,91,52,97,51,14]
  ,[70,11,33,28,77,73,17,78,39,68,17,57]
  ,[91,71,52,38,17,14,91,43,58,50,27,29,48]
  ,[63,66,04,68,89,53,67,30,73,16,69,87,40,31]
  ,[04,62,98,27,23,09,70,98,73,93,38,53,60,04,23]]
q8c (xs:[])  = xs:[]
q8c (xs:xss) =
  let ys = zipWith (+) (head xss) . map (\(x,y) -> max x y) . zip xs . tail $ xs
  in q8c . (ys:) . tail $ xss

q8' = q8c . reverse $ q8a
q8 = q8c . reverse $ q8b

-- 1074


{-
 - 19.
 -
 - You are given the following information, but you may prefer to do some
 - research for yourself.
 -
 - * 1 Jan 1900 was a Monday.
 - * Thirty days has September,
 -   April, June and November.
 -   All the rest have thirty-one,
 -   Saving February alone,
 -   Which has twenty-eight, rain or shine.
 -   And on leap years, twenty-nine.
 - * A leap year occurs on any year evenly divisible by 4,
 -   but not on a century unless it is divisible by 400.
 -
 - How many Sundays fell on the first of the month during the twentieth century
 - (1 Jan 1901 to 31 Dec 2000)?
 -}

-- 171
