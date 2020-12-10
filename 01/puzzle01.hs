module Main (main) where

strToIntArr :: [String] -> [Int]
strToIntArr arr = map read arr 

findSum :: [Int] -> Int -> (Int, Int)
findSum (x:xs) sum
    | (sum - x) `elem` xs = ((sum - x), x)
    | otherwise = findSum xs sum

main :: IO ()
main = do
    dat <- readFile "./input"
    let intarr = strToIntArr (words dat)
    let (num1, num2) = findSum intarr 2020
    putStrLn ("Found two numbers " ++ (show num1) ++ " and " ++ (show num2) ++ " multiplying to " ++ (show (num1 * num2)))

