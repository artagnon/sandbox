import Data.List
import Data.Char
import Data.Maybe
import System.IO

numAssemble :: (Num a) => [a] -> a
numAssemble [] = 0
numAssemble x = (head x) * 10^(length x - 1) + numAssemble (tail x)
    
stripDict :: Char -> Int -> [[Char]] -> [[Char]]
stripDict charOccur index dic = filter (\x -> charOccur == x !! index) dic

decodeLanguage :: [[Char]] -> Int -> [[Char]] -> Int
decodeLanguage (x:xs) index dic = iter (length x - 1)
    where
      iter 0 = callWithN 0
      iter n = callWithN n + iter (n - 1)
      callWithN n
          | null xs && (not . null $ strippedDict n) = 1
          | (not . null $ strippedDict n) = decodeLanguage xs (index + 1) (strippedDict n)
          | otherwise = 0
      strippedDict n = stripDict (x !! n) index dic

parseIntoList :: [Char] -> [[Char]]
parseIntoList [] = [[]]
parseIntoList (x:xs)
    | x == ')' = concat [[[y]] | y <- takeWhile isLetter $ xs] ++ (parseIntoList . dropWhile isLetter $ xs)
    | x == '(' = [[x | x <- takeWhile isLetter $ xs]] ++ (parseIntoList . dropWhile isLetter $ xs)
    | otherwise = [[x] | x <- takeWhile isLetter $ (x:xs)] ++ (parseIntoList . dropWhile isLetter $ xs)

processInput :: [Char] -> [Char]
processInput a = concat ["Case #" ++ show (x + 1) ++ ": " ++ show (handletcString (testCases !! x)) ++ "\n" | x <- [0 .. length (testCases) - 1]]
    where handletcString str = decodeLanguage (init . parseIntoList $ str) 0 dictionary
          dictionary = take dictionaryLen . tail . lines $ a
          firstLine = head . lines $ a
          dictionaryLen = numAssemble . map (digitToInt) $ (words firstLine !! 1)
          testCases = drop (1 + dictionaryLen) . lines $ a

debugInput :: [Char] -> [Char]
debugInput a = concat [show dictionaryLen, show dictionary, show testCases]
    where handletcString str = decodeLanguage (init . parseIntoList $ str) 0 dictionary
          dictionary = take dictionaryLen . tail . lines $ a
          firstLine = head . lines $ a
          dictionaryLen = numAssemble . map (digitToInt) $ (words firstLine !! 1)
          testCases = drop (1 + dictionaryLen) . lines $ a

main = interact processInput
