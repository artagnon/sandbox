import Data.List.Ordered (minus)
import Control.Monad (liftM, mapM_, replicateM)

trialSum :: [(String, Int)] -> Bool
trialSum xs = (sum . map snd $ xs) == 0

slice :: Int -> Int -> [a] -> [a]
slice from to xs = take (to - from + 1) (drop from xs)

pick :: (Eq a, Num a, Ord b) => a -> [b] -> [[b]]
pick 1 xs = [[x] | x <- xs]
pick n xs = filter (not . null) . concat $
            [map (\rest ->  xs !! m : rest) $
             pick (n - 1) (xs `minus` (slice 0 m xs)) |
             m <- [0 .. (length xs - 1)]]

diet :: [(String, Int)] -> [String]
diet xs = if not . null $ horse
             then map fst . head $ horse
          else
            return "no solution"
  where horse = filter (trialSum) . concat $ [pick n $ xs | n <- [1 .. length xs]]

parseString :: String -> (String, Int)
parseString str = let list = words str in (list !! 0, read $ list !! 1)

readCaseList :: IO [(String, Int)]
readCaseList = getLine >>= (\n -> replicateM (read n) $ (liftM parseString) getLine)

main :: IO ()
main = liftM diet readCaseList >>= mapM_ putStrLn
