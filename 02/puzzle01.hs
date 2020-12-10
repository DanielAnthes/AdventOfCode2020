module Main (main) where

import Data.Char

parseNumber :: String -> String
parseNumber (x:xs)
    | isDigit x = x : parseNumber xs
    | otherwise = "" 


-- >>> parseDatabaseEntry "5-13 f: lfgdplfffxffswck"
-- ('f',5,13,"lfgdplfffxffswck")
parseDatabaseEntry :: String -> (Char, Int, Int, String)
parseDatabaseEntry entry = (c, minInt, maxInt, passwd)
    where
        firstNumber      = parseNumber entry
        minInt           = read firstNumber :: Int
        maxInt           = read (parseNumber (drop (length firstNumber + 1) entry)) :: Int
        c                = head (head (take 1 (drop 1 (words entry))))
        passwd           = words entry !! 2


countOccurrence :: Char -> [Char] -> Int
countOccurrence c [] = 0
countOccurrence c (x:xs)
    | x == c    = 1 + countOccurrence c xs
    | otherwise = countOccurrence c xs


-- >>> isAllowedPassword 5 13 'f' "lfgdplfffxffswck"
-- True
isAllowedPassword :: Int -> Int -> Char -> String -> Bool
isAllowedPassword min max c passwd
    | count >= min && count <= max = True
    | otherwise                    = False
        where
            count = countOccurrence c passwd


-- >>> countAllowedPasswords ["5-13 f: lfgdplfffxffswck"]
-- 1
countAllowedPasswords :: [String] -> Int
countAllowedPasswords [] = 0
countAllowedPasswords (x:xs)
    | isAllowedPassword min max c passwd = 1 + countAllowedPasswords xs
    | otherwise                          = countAllowedPasswords xs
    where (c, min, max, passwd) = parseDatabaseEntry x
        

main :: IO ()
main = do
    dat <- readFile "./input"
    let database = lines dat
    let validPWCount = countAllowedPasswords database
    putStrLn ("Found " ++ show validPWCount ++ " valid passwords.")
