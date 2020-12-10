module Main (main) where

strToIntArr :: [String] -> [Int]
strToIntArr arr = map read arr 

findSum :: [Int] -> Int -> Maybe (Int, Int)
findSum [] sum = Nothing
findSum (x:xs) sum
    | (sum - x) `elem` xs = Just ((sum - x), x)
    | otherwise = findSum xs sum

findSumOfThree :: [Int] -> Int -> (Int,Int,Int)
findSumOfThree (x:xs) sumTo = case findSum xs sum of
	Nothing -> findSumOfThree xs sumTo
	Just (x1, x2) -> (x, x1, x2)
	where sum = sumTo - x

main :: IO ()
main = do
    dat <- readFile "./input"
    let intarr = strToIntArr (words dat)
    let (num1, num2, num3) = findSumOfThree intarr 2020
    putStrLn ("Found three numbers " ++ (show num1) ++ " and " ++ (show num2) ++ " and " ++ (show num3) ++ " multiplying to " ++ (show (num1 * num2 * num3)))

