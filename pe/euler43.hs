interpose :: a -> [a] -> [[a]]
interpose x list = iter (length list)
  where iter 0 = [x:list]
        iter n = callWithN n ++ iter (n - 1)
        callWithN n = [(take n list) ++ [x] ++ (drop n list)]

permute :: [a] -> [[a]]
permute [] = [[]]
permute [a, b] = [[a, b], [b, a]]
permute (x:xs) = concat [interpose x list | list <- permute xs]

length' :: [a] -> Integer
length' x = read . show . length $ x :: Integer

numcat :: [Integer] -> Integer
numcat (x:[]) = x
numcat (x:xs) = 10 ^ length' xs * x + numcat xs

numPermute :: [Integer] -> [Integer]
numPermute n = [numcat x | x <- permute n]

euler43 :: Integer
euler43 = sum [x | x <- pandigital,
               num x 1 `mod` 2 == 0,
               num x 2 `mod` 3 == 0,
               num x 3 `mod` 5 == 0,
               num x 4 `mod` 7 == 0,
               num x 5 `mod` 11 == 0,
               num x 6 `mod` 13 == 0,
               num x 7 `mod` 17 == 0]
  where pandigital = numPermute [0..9]
        num x n = read . take 3 . drop n . show $ x :: Integer

main = print euler43
