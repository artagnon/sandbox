import Data.List
import Control.Monad
import Data.List.Ordered

trialSum :: [(String, Int)] -> Bool
trialSum xs = (sum . map snd $ xs) == 0

pickOne = id

pickTwo xs = [[m, rest] | m <- xs, rest <- (xs `minus` [xs !! 0 .. m])]

pickThree xs = filter (not . null) $ [map (\rest ->  m : rest) $ pickTwo (xs `minus` [xs !! 0 .. m]) | m <- xs]

diet :: [(String, Int)] -> [String]
diet = map fst

parseString :: String -> (String, Int)
parseString str = let list = words str in (list !! 0, read $ list !! 1)

readCaseList :: IO [(String, Int)]
readCaseList = getLine >>= (\n -> replicateM (read n) $ (liftM parseString) getLine)

main :: IO ()
main = liftM diet readCaseList >>= mapM_ putStrLn
