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


-- >>> isAllowedPassword 1 3 'c' "ccc"

-- >>> "abc" !! 1
-- 'b'

isAllowedPassword :: Int -> Int -> Char -> String -> Bool
isAllowedPassword idx1 idx2 c passwd
    | res1 /= res2 = True
    | otherwise    = False
    where
        res1 = (length passwd >= idx1) && ((passwd !! (idx1-1)) == c)
        res2 = (length passwd >= idx2) && ((passwd !! (idx2-1)) == c)
       

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
