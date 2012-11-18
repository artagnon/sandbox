import Data.List
import Text.Printf
import System.IO

countWelcomes :: [Char] -> [Char] -> Int
countWelcomes [] (x:xs) = 0
countWelcomes inputStr (x:xs)
    | length searchIndices > 0 = iter (length searchIndices - 1)
    | otherwise = 0
    where
      iter 0 = callWithN 0
      iter n = callWithN n + iter (n - 1)
      callWithN n
          | null xs && (not . null $ searchIndices) = 1
          | (not . null $ searchIndices) = countWelcomes (drop (searchIndices !! n + 1) inputStr) xs
          | otherwise = 0
      searchIndices = elemIndices x inputStr

processInput :: [Char] -> [Char]
processInput a = concat ["Case #" ++ show (x + 1) ++ ": " ++ (resultStr x) ++ "\n" | x <- [0 .. length testCases - 1]]
    where testCases = tail . lines $ a
          resultInt x = show (mod (countWelcomes (testCases !! x) "welcome to code jam") 1000)
          resultStr x = (concat ["0" | x <- [1 .. (4 - length (resultInt x))]]) ++ (resultInt x)

main = interact processInput
