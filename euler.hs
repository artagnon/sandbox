import Data.List
import Data.Ord
import Data.Maybe
import Data.Char
import qualified Data.Set as Set
import Control.Applicative

euler1 :: (Integral a) => a
euler1 = sum . filter (\x -> (x `mod` 3 == 0 && x `mod` 5 == 0)) $ [1..999]

assertPrime :: (Integral a) => a -> Bool
assertPrime 1 = False
assertPrime x = null . takeWhile (<= div x 2) . filter(\y -> x `mod` y == 0) $ [2..]

euler2 :: (Integral a) => a
euler2 = sum . takeWhile (< 4000000) . filter even $ fiblist
    where fiblist = 1 : 2 : (zipWith (+) fiblist (tail fiblist))

euler3 :: (Integral a) => a
euler3 = head $ factorize 600851475143 []
    where factorize 1 l = l
          factorize x l = factorize (x `div` (smallestPrimeFactor x)) l ++
                          [smallestPrimeFactor x]
          smallestPrimeFactor x = head [y | y <- [2..], x `mod` y == 0]

euler4 :: Int
euler4 = maximum . filter assertPalindrome $ (*) <$>
         [999, 998 .. 100] <*>
         [999, 998 .. 100]
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
    where consecutiveList = map xFunction [1000, 999 .. 5]
          productOfDigits x = product . map (yFunction x) $ [5, 4 .. 1]
          xFunction x = a `mod` (10 ^ x) `div` 10 ^ (x - 5)
          yFunction x y = x `mod` (10 ^ y) `div` 10 ^ (y - 1)

euler9 :: (Integral a) => a
euler9 = head [a * b * c |
               a <- [1..],
               b <- [1..],
               c <- [1..],
               a + b + c == 1000,
               a^2 + b^2 == c^2]

euler10 :: (Integral a) => a
euler10 = sum $ takeWhile(< 2000000) primes

-- euler11 in dedicated euler11.hs

euler12 :: (Integral a) => a
euler12 = head . filter ((> 500) . numberOfPrimeFactors) $ triangleSeries
    where triangleSeries = [div (n * (n + 1)) 2 | n <- [1..]]
          numberOfPrimeFactors n = product .
                                   map ((+1) . length) .
                                   group .
                                   (primeFactors n) $
                                   filterPrimes n
          filterPrimes n = filter (\x -> n `mod` x == 0) primes

primeFactors :: (Integral a) => a -> [a] -> [a]
-- Pass a list of prime numbers to me :)
primeFactors 1 _ = []
primeFactors a (x:xs)
        | a `mod` x /= 0 = primeFactors a xs
        | otherwise = x : (primeFactors (a `div` x) (x:xs))
                   
-- euler13 in dedicated euler13.hs

euler14 :: Integer
euler14 = foldl1' (maxBy (comparing n_series_len)) [2, 3 .. 13]
    -- Compiled version finishes in 190 seconds
    where maxBy compare x y = case x `compare` y of
                                LT -> y
                                _  -> x
          n_series_len n = length $ takeWhile (/= 1) (iterate n_series n) ++ [1]
          n_series n
              | even n = div n 2
              | otherwise = 3 * n + 1

euler15 :: Integer -> Integer
euler15 n = div (factorial (2*n)) ((factorial n)^2)

euler16 :: (Integral a) => a
euler16 = sum . map (\y -> 2 ^ 1000 `mod` (10 ^ y) `div` 10 ^ (y - 1)) $ [1 .. 302]

euler17 :: Int
euler17 = sumLen $ (until100 ++
                    [x ++ "hundred" |
                     x <- units] ++
                    [x ++ "hundredand" ++ y |
                     x <- units, y <- until100] ++
                    ["onethousand"])
    where until100 = units ++ ["ten"] ++ until20 ++ tens ++
                     [x++y | x <- tens, y <- units]
          units = ["one", "two", "three", "four", "five",
                   "six", "seven", "eight", "nine"]
          until20 = ["eleven", "twelve", "thirteen", "fourteen",
                     "fifteen", "sixteen", "seventeen", "eighteen", "nineteen"]
          tens = ["twenty", "thirty", "forty", "fifty",
                  "sixty", "seventy", "eighty", "ninety"]
          sumLen = length . (foldl1 (++))

-- maximizeTree triangleTree
--     | length triangleTree == 1 = head (triangleTree !! 0)
--     | otherwise = maximum callWithN n triangleTree
--     where iter 0 = 1
--           iter n = callWithN n : iter (n - 1)
--           callWithN n = (slides !! chooseLRslide + maximizeTree . pruneTree n chooseLRslide $ triangleTree)
--           pruneTree n 0 triangleTree = drop (1 + n) . map (reverse . drop 1 . reverse . drop (n - 1)) $ triangleTree
--           pruneTree n 1 triangleTree = drop (1 + n) . map (reverse . drop (n - 1) . reverse . drop 1) $ triangleTree
--           chooseLRslide = if slides !! 0 > slides !! 1 then 0 else 1
--           slides = [(sum . map head . take n $ triangleTree), (sum . map last . take n $ triangleTree)]

maximizeTree :: (Ord a, Num a) => [[a]] -> a
maximizeTree tree
    | length tree == 1 = head . head $ tree
    | otherwise = maximizeTree (take (length tree - 2) tree ++
                                [zipWith (\a b -> maximum a + b)
                                 (pairUpList . last $ tree)
                                 (tree !! (length tree - 2))])
    where pairUpList list = [[list !! n, list !! (n + 1)] |
                             n <- [0 .. (length list - 2)]]

euler18 :: Integer
euler18 = maximizeTree [[75], [95, 64], [17, 47, 82], [18, 35, 87, 10], [20, 04, 82, 47, 65], [19, 01, 23, 75, 03, 34], [88, 02, 77, 73, 07, 63, 67], [99, 65, 04, 28, 06, 16, 70, 92], [41, 41, 26, 56, 83, 40, 80, 70, 33], [41, 48, 72, 33, 47, 32, 37, 16, 94, 29], [53, 71, 44, 65, 25, 43, 91, 52, 97, 51, 14], [70, 11, 33, 28, 77, 73, 17, 78, 39, 68, 17, 57], [91, 71, 52, 38, 17, 14, 91, 43, 58, 50, 27, 29, 48], [63, 66, 04, 68, 89, 53, 67, 30, 73, 16, 69, 87, 40, 31], [04, 62, 98, 27, 23, 09, 70, 98, 73, 93, 38, 53, 60, 04, 23]]

euler19 :: Int
euler19 = length . filter (== 0) . concat $
          [map (\x -> (x + janSun n) `mod` 7)
           (scanl1 (+) (if assertLeap n then monthOffsetsLeap else monthOffsets)) |
           n <- [1901, 1902 .. 2000]]
    -- Dow: Sunday is 0. Monday is 1 etc.
    where janSun y = (map (\x -> (2 + x) `mod` 7) $
                      (scanl1 (+) ([0] ++ drop 1 yrModList))) !! (y - 1901)
          monthOffsets = [0, 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30]
          monthOffsetsLeap = [0, 31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30]
          yrModList = [if assertLeap (x - 1) then 2 else 1 | x <- [1901, 1902 .. 2000]]
          assertLeap x = x `mod` 4 == 0

euler20 :: (Integral a) => a
euler20 = sumOfDigits $ product [1 .. 100]
    where sumOfDigits x = sum [(x `mod` (10 ^ y)) `div` (10 ^ (y - 1)) |
                               y <- [1 .. 158]]

euler21 :: Int
euler21 = sum . filter assertAmicable $ [2 .. 9999]
     where factors n = stemDivisors (uniquePrimeFactors n) (multiplicities n)
           uniquePrimeFactors n = nub . primeFactorList $ n
           multiplicities n = map length . group . primeFactorList $ n
           primeFactorList n = primeFactors n . filterPrimes $ n
           filterPrimes n = filter (\x -> n `mod` x == 0) primes
           assertAmicable n = (sumDivisors n /= n) &&
                              (sumDivisors . sumDivisors $ n) == n
           sumDivisors n = (sum . drop 1 . factors $ n)

-- euler22 in dedicated file

euler23 :: Int
euler23 = (sum [1 .. 23]) + (sum . filter(
                                \x -> not (x `elem` (possible2Sums x
                                                     (takeWhile (< x)
                                                      abundantSeries)))) $
                             [24, 25 .. 28123])
    -- Compiled version finishes in 1177s -- Need to speed up!
    where abundantSeries = filter(\x -> sumDivisors x > x) [x | x <- [2 ..]]
          factors n = stemDivisors (uniquePrimeFactors n) (multiplicities n)
          sumDivisors n = (sum . drop 1 . factors $ n)
          uniquePrimeFactors n = nub . primeFactorList $ n
          multiplicities n = map length . group . primeFactorList $ n
          primeFactorList n = primeFactors n . filterPrimes $ n
          filterPrimes n = filter (\x -> n `mod` x == 0) primes
          possible2Sums num list = [x + y | x <- list, y <- list]

stemDivisors :: (Integral a) => [a] -> [a] -> [a]
stemDivisors [] [] = [1]
stemDivisors (x:xs) (y:ys) = iter y
    where iter 0 = callWithN 0
          iter n = callWithN n ++ iter (n - 1)
          callWithN n = map (* (x^n)) (stemDivisors xs ys)

interpose :: a -> [a] -> [[a]]
interpose x list = iter (length list)
    where iter 0 = [x:list]
          iter n = callWithN n ++ iter (n - 1)
          callWithN n = [(take n list) ++ [x] ++ (drop n list)]

permute :: [a] -> [[a]]
permute [] = [[]]
permute [a, b] = [[a, b], [b, a]]
permute (x:xs) = concat [interpose x list | list <- permute xs]

euler24 :: [Integer]
euler24 = (sort . permute $ [0..9]) !! 999999

euler25 :: (Integral a) => a
euler25 = fst $ head $ filter (\x -> div (snd x) (10^999) /= 0) zippedFiblist
    where zippedFiblist = zip [1..] fiblist
          fiblist = 1 : 1 : (zipWith (+) fiblist (tail fiblist))

euler26 :: Integer
euler26 = maximumBy (comparing (\x -> longDivide 1 x $ [])) [1 .. 999]
    where longDivide x y rem
              | (x*10 `mod` y) == 0 = 1
              | x `elem` rem = 0
              | otherwise = 1 + (longDivide (x*10 `mod` y) y (x:rem))

euler27 :: [Int]
euler27 = maximum' [[length . takeWhile (assertPrime') $
                     [n^2 + a*n + b |
                      n <- [0 .. ]], a, b] |
                    b <- takeWhile (< 1000) primes, a <- [-(b - 1) .. 1000]]
    -- Compiled version finishes in 7s
    where assertPrime' x = x > 1 && assertPrime x
          maximum' xs = foldl1' lMax xs
          lMax x y = if head x > head y then x else y

euler29 :: Int
euler29 = length . nub $ [a^b | a <- [2 .. 100], b <- [2 .. 100]]

euler30 :: [Int]
euler30 = [x | x <- [2 ..], x == sum5p x]
    -- Manually break when necessary
    where sum5p x = sum [((x `mod` (10 ^ y)) `div` (10 ^ (y - 1))) ^ 5 |
                         y <- [1 .. length (show x)]]

denomCombs :: (Integral a) => a -> [a] -> a
denomCombs _ [] = 0
denomCombs remaining (x:xs) = iter (div remaining x)
    where
      iter 0 = callWithN 0
      iter n = callWithN n + iter (n - 1)
      callWithN n
          | null xs = 1
          | otherwise = denomCombs (remaining - n * x) xs

euler31 :: Integer
euler31 = denomCombs 200 [200, 100, 50, 20, 10, 5, 2]

factorial :: Integer -> Integer
factorial 0 = 1
factorial n | n > 0 = n * factorial (n-1)

assertPandigital :: Int -> Int -> Bool
assertPandigital x n = all (\y -> y `elem` extractDigits x) [1 .. n] &&
                       all (== 1) (map length . group . extractDigits $ x)
    where extractDigits x = [(x `mod` (10 ^ y)) `div` (10 ^ (y - 1)) |
                             y <- [1 .. length (show x)]]

euler32 :: Integer
euler32 = sum . nub $ [x*y |
                       x <- [2 .. 100],
                       y <- [(div 1000 x) .. (div 10000 x)],
                       assertPandigital'
                       (extractDigits x ++ extractDigits y ++ extractDigits (x*y))]
    where assertPandigital' x = all (\y -> y `elem` x) [1 .. 9] &&
                                all (== 1) (map length . group $ x)
          extractDigits x = [(x `mod` (10 ^ y)) `div` (10 ^ (y - 1)) |
                             y <- [1 .. length (show x)]]

euler33 :: [[Integer]]
euler33 = filter (specialFrac) $ [[x, y] | y <- [1 .. 100], x <- [1 .. (y - 1)]]
    where specialFrac [x, y] = if mod x 10 == firstDigit y then
                                  fracAssertEqual [x, y]
                                  (removeCommonDigits [x, y])
                               else
                                  False
          fracAssertEqual [x, y] [p, q] = if x * q /= 0 then x * q == y * p else False
          removeCommonDigits [x, y] =  [div x 10, numTail y]
          firstDigit x = div x (10^(length (show x) - 1))
          numTail x = mod x (10^(length (show x) - 1))

euler34 :: [Integer]
euler34 = [x | x <- [1 ..], x == sumOfFacDigits x]
    -- Manually break when necessary
    where sumOfFacDigits x = sum [factorial ((x `mod` (10 ^ y)) `div` (10 ^ (y - 1))) |
                                  y <- [1 .. length (show x)]]

numAssemble :: (Num a) => [a] -> a
numAssemble [] = 0
numAssemble x = (head x) * 10^(length x - 1) + numAssemble (tail x)

euler35 :: [Integer]
euler35 = filter (allRotPrime) $ (takeWhile (< (10^6)) smartPrimes)
    -- Compiled version finishes in 28s
    where allRotPrime x = all (assertPrime)
                          (map (numAssemble)
                           (rotateDigits . reverse . extractDigits $ x))
          rotateDigits a = [drop n a ++ take n a | n <- [0 .. length a - 1]]
          smartPrimes = filter (\x -> all (`elem` [1, 3, 7, 9])
                                      (extractDigits x)) primes

decToBin :: (Integral a) => a -> [a]
decToBin x = reverse $ decToBin' x
    where
      decToBin' 0 = []
      decToBin' y = let (a,b) = quotRem y 2 in [b] ++ decToBin' a

euler36 = sum . filter (\x -> assertPalindrome x &&
                              (reverse . decToBin $ x) == decToBin x) $ [1 .. 999999]
    where assertPalindrome x = (show x) == (reverse $ show x)

euler37 :: [Int]
euler37 = take 15 . filter (\x -> all assertPrime' (lrTruncate x)) $ primes
    where lrTruncate x = sort ((lTruncate . reverse . extractDigits $ x) ++
                               (rTruncate . reverse . extractDigits $ x))
          rTruncate y = [numAssemble . take n $ y | n <- [1 .. (length y - 1)]]
          lTruncate y = reverse [numAssemble . drop n $ y | n <- [0 .. (length y - 1)]]
          assertPrime' x = x > 1 && assertPrime x

euler38 :: [[Char]]
euler38 = concat $ [filter (\x -> length x == 9 &&
                                  (assertPandigital' (splitNum x) 9))
                    (scanl1 (++) [show (x*y) | x <- [1 .. 9]]) | y <- [2 .. ]]
    -- Break when required
    where assertPandigital' x n = all (\y -> y `elem` x) [1 .. n]
          splitNum x = [digitToInt (x !! y) | y <- [0 .. (length x - 1)]]

euler39 :: [Integer]
euler39 = maximumBy (comparing length) lists
    -- Compiled version finshes in 175s
    where lists = [p:[(p-a-b) |
                      a <- [1 .. p],
                      b <- [a .. p],
                      a^2 + b^2 == (p-a-b)^2] |
                   p <- [1 .. 1000]]

euler40 :: Integer
euler40 = product [(digitList !! ((10^x) - 1)) | x <- [0 .. 6]]
    where digitList = concat (map (reverse . extractDigits) $ [1..])

euler41 :: [Int]
euler41 = [numAssemble x |
           x <- permute [1 .. 7],
           assertPrime . numAssemble $ x,
           assertPandigital' x (length (show x))]
    where assertPandigital' x n = all (\y -> y `elem` x) [1 .. n] &&
                                  all (== 1) (map length . group $ x)
                  
-- euler42 in dedicated euler42.hs

-- euler43 in dedicated euler43.hs

euler44 :: Integer
euler44 = head [k - j |
                j <- pSeries, k <- pSeries,
                j < k,
                assertP (k + j),
                assertP (k - j)]
  where pNum n = (n * (3 * n - 1)) `div` 2
        pSeries = [pNum n | n <- [1 ..]]
        assertP x = x `elem` (takeWhile (<= x) pSeries)

euler45 :: [Integer]
euler45 = Set.toList . Set.intersection triangleSeries .
          Set.intersection pentagonalSeries $ hexagonalSeries
    where triangleSeries = Set.fromList [div (n * (n + 1)) 2 | n <- [2 .. 100000]]
          pentagonalSeries = Set.fromList [div (n * (3*n - 1)) 2 | n <- [2 .. 100000]]
          hexagonalSeries = Set.fromList [n * (2*n - 1) | n <- [2 .. 100000]]

euler46 = head .
          filter (\x -> not . any (twiceSquare) $ primeDiffList x) $
          compositeList
    where compositeList = filter (not . assertPrime) [3, 5 .. ]
          twiceSquare x = (div x 2) `elem` (takeWhile (<= x) [x^2 | x <- [1 .. ]])
          primeDiffList x = map (\y -> x-y) . takeWhile (< x) $ primes

euler47 :: [Integer]
euler47 = map (fst) .
          filter (\x -> all (== 4) (snd x)) $
          [(x, [primesLen x,
                primesLen (x + 1),
                primesLen (x + 2),
                primesLen (x + 3)]) |
           x <- [1 .. ]]
    -- Compiled version finishes in 22 seconds
    where primesLen x = length . nub . primeFactors x $ primes

euler48 :: (Integral a) => a
euler48 = (sum [x^x | x <- [1..1000]]) `mod` 10^10

euler49 = filter (assertPermuations)
          [[x, y, (y + abs (x - y))] |
           x <- takeWhile (< 10000) . filter (> 1000) $ primes,
           y <- takeWhile (\y -> y + abs (x - y) < 10000) . filter(> x) $ primes,
           assertPrime (y + abs (x - y))]
    -- Compiled version finishes in 24s
    where assertPermuations list = all (\x -> assertContainingDigits x .
                                              extractDigits $
                                              (head list)) (tail list)
          assertContainingDigits x list = Set.fromList (extractDigits x) ==
                                          Set.fromList (list)
          
euler50 :: (Integral a) => a
-- Compiled version finishes in 850s - Need to speed up!
euler50 = sum . maximumBy (comparing length) .
          filter (assertPrime . sum) .
          takeWhile (\x -> sum x <= (10^6)) $
          [drop m . take n $ primes | n <- [99000 .. ], m <- [0 .. n-1]]

replaceDigit :: (Eq a) => [a] -> a -> a -> [a]
replaceDigit [x] p q
             | x == p = [q]
             | otherwise = [x]
replaceDigit (x:xs) p q
             | x == p = q : (replaceDigit xs p q)
             | otherwise = x : (replaceDigit xs p q)

-- euler51 = filter (length . filter (== True) . map (assertPrime) $ x == 8)
-- Algorithm: Keep finding n-digit primes, filter `elem` [0, 1, 2]

euler52 :: Integer
euler52 = head [x | x <- [1 ..],
                assertPermutation x (2*x),
                assertPermutation x (3*x),
                assertPermutation x (4*x),
                assertPermutation x (5*x),
                assertPermutation x (6*x)]
    -- Compiled version finishes in 1.8s
    where assertPermutation x y = (Set.fromList . extractDigits $ y) ==
                                  (Set.fromList . extractDigits $ x)

factorial' :: (Integral a) => a -> a -> a
factorial' n r
    | n == r = 1
    | otherwise = n * (factorial' (n - 1) r)

euler53 :: Int
euler53 = length . filter (> (10^6)) $
          [div (factorial' n r) (factorial (n - r)) | n <- [1 .. 100], r <- [1 .. n]]

assertLychrel :: [Int] -> Int -> Bool
assertLychrel x iter
    | iter == 0 = True
    | x == reverse x = False
    | otherwise = assertLychrel (extractDigits $
                                 (numAssemble x + (numAssemble . reverse $ x)))
                  (iter - 1)

euler55 :: Int
euler55 = length . filter (\x -> assertLychrel x 50) .
          map (\x -> extractDigits (x + (numAssemble . extractDigits $ x))) $
          [1 .. 9999]

euler56 :: Integer
euler56 = maximum . map sumOfDigits $ [a^b | a <- [1 .. 99], b <- [1 .. 99]]
    where sumOfDigits x = sum [(x `mod` (10 ^ y)) `div` (10 ^ (y - 1)) |
                               y <- [1 .. length (show x)]]

euler57 :: Int
euler57 = length . filter(\x -> length (show (head x)) >
                                length (show (last x))) $
          fracList
    where fracList = take 1000 . map (\x -> [head x + last x, last x]) .
                     iterate nextRoot2 $ [1, 2]
          nextRoot2 [a, b] = [b, (2 * b + a)]

assertPermutation' :: [Int] -> Bool
assertPermutation' a = all (== True) [(sort . extractDigits . head $ a) ==
                                      (sort . extractDigits $ x) | x <- tail a]

euler63 :: Int
euler63 = (sum . map (\n -> length .
                            takeWhile (\x -> x == (length . extractDigits $ n^x)) $
                            [2 ..]) $ [2 .. 9]) + 9

-- euler67 in dedicated file

euler69 :: Integer
-- Too slow!
euler69 = maximumBy (comparing (\x -> (fromIntegral 1) / phiMultiple x)) $ [2 .. 1000]
  where phiMultiple n = (foldl1' (*) [(fromIntegral (x - 1)) / (fromIntegral x) |
                              x <- uniqPrimeFactorList n])
        uniqPrimeFactorList = nub $ primeFactorList
        primeFactorList n = primeFactors n . filterPrimes $ n
        filterPrimes n = filter (\x -> n `mod` x == 0) primes

euler71 :: [Integer]
euler71 = maximum' . fracList $ 1000000
    -- Compiled version finishes under a second
    where fracList lim = [[x, y] | x <- [(((div lim 7) - 1) * 3) .. (div lim 7) * 3],
                          y <- [(((div lim 7) - 1) * 7) .. ((div lim 7) * 7)],
                          gcd x y == 1, x * 7 < y * 3]
          maximum' xs = foldl1' fracMax xs
          fracMax x y = if (head x) * (last y) < (head y) * (last x) then y else x

euler73 :: Int
euler73 = length . filter (\x -> (3 * (head x) > (last x)) &&
                                 (2 * (head x) < (last x))) . fracList $
          10000
    -- Compiled version finishes in 53s
    where fracList lim = [[x, y] | y <- [2 .. lim], x <- [1 .. y], gcd x y == 1]

euler74 :: Int
euler74 = length . filter (== 60) $ [facChainLen x | x <- [1000000, 999999 .. 2]]
  where facDigits n = foldl1' (+) . map (factorial) $ extractDigits n
        facChain x xs
          | x `elem` xs = xs
          | otherwise = facChain (facDigits x) (x:xs)
        facChainLen x = length $ facChain x []

euler76 :: Integer
euler76 = denomCombs 100 [99, 98 .. 2]

euler78 :: Integer
-- Too slow!
euler78 = head [n | n <- [2 .. ], p n `mod` 1000000 == 0]
  where p n = denomCombs n [n, (n - 1) .. 2]

euler85 :: Integer
euler85 = last . takeWhile (\x -> x * (x + 1) `div` 2 < 2000000) $ [1 ..]

euler87 :: Int
euler87 = length . takeWhile (<= 50000000) $ mooList
    -- Need to speed up!
    where mooList = [x | x <- [28 .. ], any (== x) .
                                        takeWhile (<= x) $
                                        [p1^2 + p2^3 + p3^4 |
                                         p1 <- takeWhile (\y -> y^2 < x) primes,
                                         p2 <- takeWhile (\y -> y^3 < x) primes,
                                         p3 <- takeWhile (\y -> y^4 < x) primes]]

extractDigits :: (Integral a, Show a) => a -> [a]
extractDigits x = [(x `mod` (10 ^ y)) `div` (10 ^ (y - 1)) |
                   y <- [1 .. length (show x)]]

euler92 :: Int
euler92 = length . filter (== 89) $ [iterate n | n <- [2 .. 9999999]]
    where iterate 1 = 1
          iterate 89 = 89
          iterate n = iterate . sumOfSquaresOfDigits $ n
          sumOfSquaresOfDigits n = sum . map (\x -> x*x) . extractDigits $ n

euler94 :: (Integral a) => a -> [a]
euler94 lim = takeWhile (< lim) $ [2*x + y |
                                   x <- [2 .. (div lim 3 + 1)],
                                   y <- [x - 1, x + 1], mod (4*x^2 - y^2) 4 == 0,
                                   assertTriad x y]
    -- Compiled version doesn't finish in 1416s. Need to speed up!
    where assertPerfectSquare x = x ==
                                  (last . takeWhile (\y -> y <= x) $
                                   [y^2 |
                                    y <- [1 ..]])
          assertTriad x y = assertPerfectSquare (div (4*x^2 - y^2) 4)

euler97 :: [Integer]
euler97 = take 10 . extractDigits $ (28433 * 2^7830457 + 1)

assertNotBouncy :: Int -> Bool
assertNotBouncy x = (maximum y == head y) &&
                    (minimum y == last y) ||
                    (maximum y == last y) &&
                    (minimum y == head y)
    where y = extractDigits x

euler112 :: Int
euler112 = last . takeWhile (\x -> ((fromJust . elemIndex x $
                                     bouncyTerms) + 1) *
                                   100 <= (75 * x)) $ bouncyTerms
    -- Doesn't reach 90% at 21780! Is the question wrong?
    -- In fact, it never seems to reach 80% :(
    where bouncyTerms = filter (not . assertNotBouncy) [100 ..]

-- euler125 :: Int
-- euler125 = sum . filter (< 1000) $ palList
--     where palList = filter assertPalindrome . sum . map (\x -> x * x) . takeWhile (sum . square < 1000) [1 ..]
--           assertPalindrome x = (show x) == (reverse $ show x)
--           square x = x * x

euler251 :: (Integral a) => a -> [[a]]
-- Not fast enough :(
euler251 lim = filter (\x -> sum x < lim)
               [[a,
                 b,
                 div ((div (8*a - 1) 3) * (div (a + 1) 3)^2) (b^2)] | a <- [1 .. lim],
                b <- [1 .. lim - a - 1], mod a 3 == 2,
                mod (8*a) 3 == 1,
                mod ((div (8*a - 1) 3) * (div (a + 1) 3 )^2) (b^2) == 0]

euler254 :: Integer -> Integer
-- Not fast enough :(
euler254 x = sum [sg i | i <- [1 .. x]]
    where f n = sum . map (factorial) . extractDigits $ n
          sf n = sum . extractDigits . f $ n
          g i = head . filter (\n -> sf n == i) $ [1 ..]
          sg i = sum . extractDigits . g $ i
