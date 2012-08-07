import Control.Monad
import Data.List

inputStr = "FourscoreandsevenyearsagoourfaathersbroughtforthonthiscontainentanewnationconceivedinzLibertyanddedicatedtothepropositionthatallmenarecreatedequalNowweareengagedinagreahtcivilwartestingwhetherthatnaptionoranynartionsoconceivedandsodedicatedcanlongendureWeareqmetonagreatbattlefiemldoftzhatwarWehavecometodedicpateaportionofthatfieldasafinalrestingplaceforthosewhoheregavetheirlivesthatthatnationmightliveItisaltogetherfangandproperthatweshoulddothisButinalargersensewecannotdedicatewecannotconsecratewecannothallowthisgroundThebravelmenlivinganddeadwhostruggledherehaveconsecrateditfaraboveourpoorponwertoaddordetractTgheworldadswfilllittlenotlenorlongrememberwhatwesayherebutitcanneverforgetwhattheydidhereItisforusthelivingrathertobededicatedheretotheulnfinishedworkwhichtheywhofoughtherehavethusfarsonoblyadvancedItisratherforustobeherededicatedtothegreattdafskremainingbeforeusthatfromthesehonoreddeadwetakeincreaseddevotiontothatcauseforwhichtheygavethelastpfullmeasureofdevotionthatweherehighlyresolvethatthesedeadshallnothavediedinvainthatthisnationunsderGodshallhaveanewbirthoffreedomandthatgovernmentofthepeoplebythepeopleforthepeopleshallnotperishfromtheearth"

greplin1 :: IO ()
greplin1 = print $ findLongestPali inputStr

takeSubstrings :: [Char] -> Int -> [[Char]]
takeSubstrings str len =
 let allowedDrop = length str - len
 in do
   n <- [0 .. allowedDrop]
   return $ take len . drop n $ str

findLongestPali :: [Char] -> Maybe [Char]
findLongestPali str =
 let len = length str
     isPali x = x == reverse x
     x = reverse [1 .. len] >>= takeSubstrings str
     headM x
       | length x == 0 = Nothing
       | otherwise = return $ head x
 in
  headM . filter isPali $ x

greplin2 :: IO ()
greplin2 = print $ sumPrimeDivisors $ 1 + (head . findPrimeFib $ 227000)
 where sumPrimeDivisors = sum . (primeFactors primes)

primes :: (Integral a) => [a]
primes = 2 : filter isPrime [3, 5 ..]
   where isPrime n = all (not . divides n) $ takeWhile (\p -> p*p <= n) primes
         divides n p = n `mod` p == 0

primeFactors :: (Integral a) => [a] -> a -> [a]
-- Pass a list of primes and a number to find its prime factors
primeFactors _ 1 = []
primeFactors (x:xs) a
       | a `mod` x /= 0 = primeFactors xs a
       | otherwise = x : (primeFactors (x:xs) (a `div` x))

findPrimeFib :: Integer -> [Integer]
findPrimeFib lim =
 let headM x
       | length x == 0 = []
       | otherwise = return $ [head x]
     isPrime n = null . takeWhile (< div n 2) . filter (divides n) $ [2 ..]
     divides n p = n `mod` p == 0
     fibs = 0 : 1 : zipWith (+) fibs (tail fibs)
 in do
   x <- fibs
   guard (x > lim)
   guard (isPrime x)
   return x

inputList = [3, 4, 9, 14, 15, 19, 28, 37, 47, 50, 54, 56, 59, 61, 70, 73, 78, 81, 92, 95, 97, 99]

greplin3 :: IO ()
greplin3 = print $ countFilterSubsets inputList

allSubsets :: [a] -> [[a]]
allSubsets xs = filterM (\_ -> [True, False]) xs

countFilterSubsets :: [Int] -> Int
countFilterSubsets xs =
 let filteredSubsets = filter maxCriteria . filter (not . null) . allSubsets
     maxCriteria xs = (maximum xs) == (sum . delete (maximum xs) $ xs)
 in
  length $ filteredSubsets xs
