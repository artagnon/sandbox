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
                   
euler7 :: (Integral a) => a
euler7 = last $ take 10001 [x | x <- [2..], assertPrime x] -- Takes too long to compute! :'(

euler6 :: (Integral a) => a
euler6 = sum [x ^ 2 | x <- [1..100]] - (sum [1..100]) ^ 2

euler10 :: (Integral a) => a
euler10 = sum take 2000000 [x | x <- [2..], assertPrime x] -- Takes too long to compute! :'(
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
                   

