import Data.List
import Data.Ord
import Data.Maybe
import qualified Data.Set as Set

euler1 :: (Integral a) => a
euler1 = sum [x | x <- [1..999], mod x 3 == 0 || mod x 5 == 0]

assertPrime :: (Integral a) => a -> Bool
assertPrime x = null divisors
    where divisors = takeWhile (<= div x 2) [y | y <- [2..], mod x y == 0]

euler2 :: (Integral a) => a
euler2 = sum $ takeWhile (< 4000000) $ filter even fiblist
    where fiblist = 1 : 2 : (zipWith (+) fiblist (tail fiblist))

euler3 :: (Integral a) => a
euler3 = head $ factorize 600851475143 []
    where factorize 1 l = l
          factorize x l = factorize (x `div` (smallestPrimeFactor x)) l++[smallestPrimeFactor x]
          smallestPrimeFactor x = head [y | y <- [2..], x `mod` y == 0]

euler4 :: (Integral a) => a
euler4 = maximum $ filter assertPalindrome [x * y | x <- [999, 998 .. 100], y <- [999, 998 .. 100]]
    where assertPalindrome x = (show x) == (reverse $ show x)

euler5 :: (Integral a) => a
euler5 = foldl1 lcm' [1..20] where
    gcd a 0 = a
    gcd a b = gcd b (a `mod` b)
    lcm' a b = (a*b) `div` gcd a b

primes :: (Integral a) => [a]
primes = 2 : filter isPrime [3, 5 ..]
    where isPrime n = all (not . divides n) $ takeWhile (\p -> p*p <= n) primes
          divides n p = n `mod` p == 0

euler6 :: (Integral a) => a
euler6 = sum [x ^ 2 | x <- [1..100]] - (sum [1..100]) ^ 2

euler7 :: (Integral a) => a
euler7 = primes !! 10000

euler8 :: (Integral a) => a -> a
euler8 a = maximum $ map productOfDigits consecutiveList
    where consecutiveList = [(a `mod` (10 ^ x)) `div` (10 ^ (x - 5)) | x <- [1000, 999 .. 5]]
          productOfDigits x = product [(x `mod` (10 ^ y)) `div` (10 ^ (y - 1)) | y <- [5, 4 .. 1]]

euler9 :: (Integral a) => a
euler9 = head [a * b * c | a <- [1..], b <- [1..], c <- [1..], a + b + c == 1000, a^2 + b^2 == c^2]

euler10 :: (Integral a) => a
euler10 = sum $ takeWhile(< 2000000) primes

-- euler11 in dedicated euler11.hs

euler12 :: (Integral a) => a
euler12 = head $ filter ((> 500) . numberOfPrimeFactors) triangleSeries
    where triangleSeries = [div (n * (n + 1)) 2 | n <- [1..]]
          numberOfPrimeFactors n = product . map ((+1) . length) . group . (primeFactors n) $ filterPrimes n
          filterPrimes n = filter (\x -> n `mod` x == 0) primes

primeFactors :: (Integral a) => a -> [a] -> [a]
-- Pass a list of prime numbers to me :)
primeFactors 1 _ = []
primeFactors a (x:xs)
        | a `mod` x /= 0 = primeFactors a xs
        | otherwise = x : (primeFactors (a `div` x) (x:xs))
                   
-- euler13 in dedicated euler13.hs

euler14 = foldl1' (maxBy (comparing n_series_len)) [2, 3 .. 13]
    -- Compiled version finishes in 190 seconds
    where maxBy compare x y = case x `compare` y of
                                LT -> y
                                _  -> x
          n_series_len n = length $ takeWhile (/= 1) (iterate n_series n) ++ [1]
          n_series n
              | even n = div n 2
              | otherwise = 3 * n + 1

euler15 n = div (factorial (2*n)) ((factorial n)^2)

euler16 :: (Integral a) => a
euler16 = sum [(2 ^ 1000 `mod` (10 ^ y)) `div` (10 ^ (y - 1)) | y <- [1 .. 302]]

euler17 = sumLen $ (until100 ++ ["onehundred"] ++ [x ++ "hundredand" ++ y | x <- units, y <- until100] ++ ["onethousand"])
    where until100 = units ++ ["ten"] ++ until20 ++ tens ++ [x++y | x <- tens, y <- units]
          units = ["one", "two", "three", "four", "five", "six", "seven", "eight", "nine"]
          until20 = ["eleven", "twelve", "thirteen", "fourteen", "fifteen", "sixteen", "seventeen", "eighteen", "nineteen"]
          tens = ["twenty", "thirty", "forty", "fifty", "sixty", "seventy", "eighty", "ninety"]
          sumLen = length . (foldl1 (++))

maximizeTree triangleTree
    | length triangleTree == 1 = head (triangleTree !! 0)
    | otherwise = maximum callWithN n triangleTree
    where iter 0 = 1
          iter n = callWithN n : iter (n - 1)
          callWithN n = (slides !! chooseLRslide + maximizeTree . pruneTree n chooseLRslide $ triangleTree)
          pruneTree n 0 triangleTree = drop (1 + n) . map (reverse . drop 1 . reverse . drop (n - 1)) $ triangleTree
          pruneTree n 1 triangleTree = drop (1 + n) . map (reverse . drop (n - 1) . reverse . drop 1) $ triangleTree
          chooseLRslide = if slides !! 0 > slides !! 1 then 0 else 1
          slides = [(sum . map head . take n $ triangleTree), (sum . map last . take n $ triangleTree)]

-- euler18 :: (Integral a) => [a] -> a
-- euler18 = maximizeTree [[75], [95, 64], [17, 47, 82], [18, 35, 87, 10], [20, 04, 82, 47, 65], [19, 01, 23, 75, 03, 34], [88, 02, 77, 73, 07, 63, 67], [99, 65, 04, 28, 06, 16, 70, 92], [41, 41, 26, 56, 83, 40, 80, 70, 33], [41, 48, 72, 33, 47, 32, 37, 16, 94, 29], [53, 71, 44, 65, 25, 43, 91, 52, 97, 51, 14], [70, 11, 33, 28, 77, 73, 17, 78, 39, 68, 17, 57], [91, 71, 52, 38, 17, 14, 91, 43, 58, 50, 27, 29, 48], [63, 66, 04, 68, 89, 53, 67, 30, 73, 16, 69, 87, 40, 31], [04, 62, 98, 27, 23, 09, 70, 98, 73, 93, 38, 53, 60, 04, 23]]

euler20 :: (Integral a) => a
euler20 = sumOfDigits $ product [1 .. 100]
    where sumOfDigits x = sum [(x `mod` (10 ^ y)) `div` (10 ^ (y - 1)) | y <- [1 .. 158]]


euler21 = sum . filter assertAmicable $ [x | x <- [2 .. 9999]]
     where factors n = stemDivisors (uniquePrimeFactors n) (multiplicities n)
           uniquePrimeFactors n = Set.toList . Set.fromList . primeFactorList $ n
           multiplicities n = map length . group . primeFactorList $ n
           primeFactorList n = primeFactors n . filterPrimes $ n
           filterPrimes n = filter (\x -> n `mod` x == 0) primes
           assertAmicable n = (sumDivisors n /= n) && (sumDivisors . sumDivisors $ n) == n
           sumDivisors n = (sum . drop 1 . factors $ n)

stemDivisors :: (Integral a) => [a] -> [a] -> [a]
stemDivisors [] [] = [1]
stemDivisors (x:xs) (y:ys) = iter y
    where iter 0 = callWithN 0
          iter n = callWithN n ++ iter (n - 1)
          callWithN n = map (* (x^n)) (stemDivisors xs ys)

permute [] = []
permute [a,b] = [a,b,b,a]
permute (x:xs) = x:(permute xs)

euler24 = take 9 (filterDigits [0 .. 9] 0)
    where filterDigits digitList n = (digitList !! (fromInteger (leftOverSeries !! n))) : filterDigits (filter (/= (digitList !! (fromInteger (leftOverSeries !! n)))) digitList) (n + 1)
          leftOverSeries = take 9 (leftOver (10^6) 9)
          leftOver m n = (div m (factorial n)) : leftOver (m - (div m (factorial n)) * (factorial n)) (n - 1)


euler25 :: (Integral a) => a
euler25 = fst $ head $ filter (\x -> div (snd x) (10^999) /= 0) zippedFiblist
    where zippedFiblist = zip [1..] fiblist
          fiblist = 1 : 1 : (zipWith (+) fiblist (tail fiblist))

euler29 = length . Set.toList . Set.fromList $ [a^b | a <- [2 .. 100], b <- [2 .. 100]]

euler30 :: (Integral a) => [a]
euler30 = [x | x <- [2 ..], x == sum5p x]
    -- Manually break when necessary
    where sum5p x = sum [((x `mod` (10 ^ y)) `div` (10 ^ (y - 1))) ^ 5 | y <- [1 .. length (show x)]]

euler31old = length
             [(a, b, c, d, e, f, g, h) |
                                         a <- [0 .. div (200) 200],
                                         b <- [0 .. div (200 - 200 * a) 100],
                                         c <- [0 .. div (200 - 200 * a - 100 * b) 50],
                                         d <- [0 .. div (200 - 200 * a - 100 * b - 50 * c) 20],
                                         e <- [0 .. div (200 - 200 * a - 100 * b - 50 * c - 20 * d) 10],
                                         f <- [0 .. div (200 - 200 * a - 100 * b - 50 * c - 20 * d - 10 * e) 5],
                                         g <- [0 .. div (200 - 200 * a - 100 * b - 50 * c - 20 * d - 10 * e - 5 * f) 2],
                                         h <- [200 - 200 * a - 100 * b - 50 * c - 20 * d - 10 * e - 5 * f - 2 * g]]

denomCombs :: (Integral a) => a -> [a] -> a
denomCombs _ [] = 0
denomCombs remaining (x:xs) =
    iter (div remaining x)
    where
      iter 0 = callWithN 0
      iter n = callWithN n + iter (n - 1)
      callWithN n
          | length xs == 0 = 1 + denomCombs (remaining - n * x) xs
          | otherwise = denomCombs (remaining - n * x) xs

euler31 = denomCombs 200 [200, 100, 50, 20, 10, 5, 2]

factorial :: Integer -> Integer
factorial 0 = 1
factorial n | n > 0 = n * factorial (n-1)


euler34 = [x | x <- [1 ..], x == sumOfFacDigits x]
    -- Manually break when necessary
    where sumOfFacDigits x = sum [factorial ((x `mod` (10 ^ y)) `div` (10 ^ (y - 1))) | y <- [1 .. length (show x)]]

decToBin x = reverse $ decToBin' x
    where
      decToBin' 0 = []
      decToBin' y = let (a,b) = quotRem y 2 in [b] ++ decToBin' a

euler36 = sum [x | x <- [1 .. 999999], assertPalindrome x, (reverse . decToBin $ x) == decToBin x]
    where assertPalindrome x = (show x) == (reverse $ show x)

euler39 = maximumBy (comparing length) lists
    -- Compiled version finshes in 175s
    where lists = [p:[(p-a-b) | a <- [1 .. p], b <- [a .. p], a^2 + b^2 == (p-a-b)^2] | p <- [1 .. 1000]]

-- euler42 in dedicated euler42.hs

euler45 = Set.toList . Set.intersection triangleSeries . Set.intersection pentagonalSeries $ hexagonalSeries
    where triangleSeries = Set.fromList [div (n * (n + 1)) 2 | n <- [2 .. 100000]]
          pentagonalSeries = Set.fromList [div (n * (3*n - 1)) 2 | n <- [2 .. 100000]]
          hexagonalSeries = Set.fromList [n * (2*n - 1) | n <- [2 .. 100000]]

euler47 = map (fst) . filter (\x -> all (== 4) (snd x)) $ [(x, [primesLen x, primesLen (x + 1), primesLen (x + 2), primesLen (x + 3)]) | x <- [1 .. ]]
    -- Compiled version finishes in 22 seconds
    where primesLen x = length . Set.toList . Set.fromList . primeFactors x $ primes

euler48 :: (Integral a) => a
euler48 = (sum [x^x | x <- [1..1000]]) `mod` 10^10

euler50 :: (Integral a) => [a]
euler50 = takeWhile (< (10^6)) [x | x <- primes, any (== x) . takeWhile (<= x) $ [sum . take n $ primes | n <- [1 ..]]]

euler52 = head [x | x <- [1 ..], assertPermutation x (2*x), assertPermutation x (3*x), assertPermutation x (4*x), assertPermutation x (5*x), assertPermutation x (6*x)]
    -- Compiled version finishes in 1.8s
    where assertPermutation x y =  null . Set.toList . Set.difference (Set.fromList . extractDigits $ y) $ Set.fromList . extractDigits $ x
          extractDigits x = [(x `mod` (10 ^ y)) `div` (10 ^ (y - 1)) | y <- [1 .. length (show x)]]

euler56 = maximum . map sumOfDigits $ [a^b | a <- [1 .. 99], b <- [1 .. 99]]
    where sumOfDigits x = sum [(x `mod` (10 ^ y)) `div` (10 ^ (y - 1)) | y <- [1 .. length (show x)]]

euler57 = length . filter(\x -> length (show (head x)) > length (show (last x))) $ fracList
    where fracList = take 1000 . map (\x -> [head x + last x, last x]) . iterate nextRoot2 $ [1, 2]
          nextRoot2 [a, b] = [b, (2 * b + a)]

euler63 = [x | x <- [1 .. ], any (\y -> length y `mod` length (show x) == 0) (group . primeFactors x $ primes)]
-- Why should it be limited?

{-
euler80 :: (Integral a) => a
euler80 = filter assertImperfectSquare [1..100]
    where assertImperfectSquare x = ceiling
-}

oldeuler71 n = maximum' . fracList $ n
    -- Compiled version doesn't finish in 237s
    where fracList lim = [[x, y] | y <- [1 .. lim], x <- [1 .. y], gcd x y == 1, x * 7 < y * 3]
          maximum' xs = foldl1' fracMax xs
          fracMax x y = if (head x) * (last y) < (head y) * (last x) then y else x

euler71 = maximum' . fracList $ 1000000
    -- Compiled version finishes under a second
    where fracList lim = [[x, y] | x <- [(((div lim 7) - 1) * 3) .. (div lim 7) * 3], y <- [(((div lim 7) - 1) * 7) .. ((div lim 7) * 7)], gcd x y == 1, x * 7 < y * 3]
          maximum' xs = foldl1' fracMax xs
          fracMax x y = if (head x) * (last y) < (head y) * (last x) then y else x

euler73 = length . filter (\x -> (3 * (head x) > (last x)) && (2 * (head x) < (last x))) . fracList $ 10000
    -- Compiled version finishes in 53s
    where fracList lim = [[x, y] | y <- [2 .. lim], x <- [1 .. y], gcd x y == 1]

euler76 = denomCombs 100 [99, 98 .. 2]

euler87 = length . takeWhile (<= 50000000) $ mooList
    -- Need to speed up!
    where mooList = [x | x <- [28 .. ], any (== x) . takeWhile (<= x) $ [p1^2 + p2^3 + p3^4 | p1 <- takeWhile (\y -> y^2 < x) primes, p2 <- takeWhile (\y -> y^3 < x) primes, p3 <- takeWhile (\y -> y^4 < x) primes]]

extractDigits :: (Integral a) => a -> [a]
extractDigits x = [(x `mod` (10 ^ y)) `div` (10 ^ (y - 1)) | y <- [1 .. length (show x)]]

euler97 = take 10 . extractDigits $ (28433 * 2^7830457 + 1)

assertNotBouncy x = (maximum y == head y) && (minimum y == last y) || (maximum y == last y) && (minimum y == head y)
    where y = extractDigits x

euler112 = last . takeWhile (\x -> ((fromJust . elemIndex x $ bouncyTerms) + 1) * 100 <= (75 * x)) $ bouncyTerms
    -- Doesn't reach 90% at 21780! Is the question wrong?
    -- In fact, it never seems to reach 80% :(
    where bouncyTerms = filter (not . assertNotBouncy) [100 .. ]
