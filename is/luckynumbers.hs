import Data.List
import Data.List.Ordered
import Control.Monad

primes = 2 : sieve [3, 5 ..]
  where sieve [] = []
        sieve (p:xs) =  p : sieve (xs `minus` [p*p, p*p+2*p ..])


assertPrime 1 = False
assertPrime x = x == (last . takeWhile (<= x) $ primes)

extractDigits :: (Integral a) => a -> [a]
extractDigits 0 = []
extractDigits x = (x `mod` 10):(extractDigits $ x `div` 10)

luckyNumbers :: Int -> Int -> Int
luckyNumbers a b = length . filter (isLucky) $ [a .. b]
  where isLucky x = (assertPrime . sumDigits $ x) && (assertPrime . sumSqDigits $ x)
        sumDigits = foldl1' (+) . extractDigits
        sumSqDigits = foldl1' (+) . map (\x -> x * x) . extractDigits

parseString :: String -> (Int, Int)
parseString str = let list = map read $ words str in (list !! 0, list !! 1)

readCaseList :: IO [(Int, Int)]
readCaseList = getLine >>= (\n -> replicateM (read n) $ (liftM parseString) getLine)

main :: IO ()
main = readCaseList >>= mapM_ (print . uncurry luckyNumbers)
