import Data.List

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
euler12 = head $ filter ((> 500) . numberOfFactors) triangleSeries
    where triangleSeries = [div (n * (n + 1)) 2 | n <- [1..]]
          numberOfFactors n = product . map ((+1) . length) . group . (factors n) $ filterPrimes n
          filterPrimes n = filter (\x -> n `mod` x == 0) primes

factors :: (Integral a) => a -> [a] -> [a]
-- Pass a list of prime numbers to me :)
factors 1 _ = []
factors a (x:xs)
        | a `mod` x /= 0 = factors a xs
        | otherwise = x : (factors (a `div` x) (x:xs))
                   
-- euler13 in dedicated euler13.hs

euler14 = maximum $ map n_series_len [13, 12 .. ]
    where n_series_len n = length $ takeWhile (/= 1) (iterate n_series n) ++ [1]
          n_series n
              | even n = div n 2
              | otherwise = 3 * n + 1

euler16 :: (Integral a) => a
euler16 = sum [(2 ^ 1000 `mod` (10 ^ y)) `div` (10 ^ (y - 1)) | y <- [1 .. 302]]
                          
euler20 :: (Integral a) => a
euler20 = sumOfDigits $ product [1 .. 100]
    where sumOfDigits x = sum [(x `mod` (10 ^ y)) `div` (10 ^ (y - 1)) | y <- [1 .. 158]]

euler25 :: (Integral a) => a
euler25 = fst $ head $ filter (\x -> div (snd x) (10^999) /= 0) zippedFiblist
    where zippedFiblist = zip [1..] fiblist
          fiblist = 1 : 1 : (zipWith (+) fiblist (tail fiblist))
                          
euler48 :: (Integral a) => a
euler48 = (sum [x^x | x <- [1..1000]]) `mod` 10^10
          
{-
euler80 :: (Integral a) => a
euler80 = filter assertImperfectSquare [1..100]
    where assertImperfectSquare x = ceiling
-}
