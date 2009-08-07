euler1 :: (Integral a) => a
euler1 = sum [x | x <- [1..999], mod x 3 == 0 || mod x 5 == 0]

assertPrime :: (Integral a) => a -> Bool
assertPrime x = null divisors
    where divisors = takeWhile (<= div x 2) [y | y <- [2..], mod x y == 0]

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

