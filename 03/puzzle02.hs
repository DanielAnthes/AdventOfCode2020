module Main (main) where

move :: [String] -> Int -> Int -> Int -> Int
move forest xpos xmove ymove
    | length forest <= 1        = 0
    | newrow !! newxpos == '#'  = 1 + move restofforest newxpos xmove ymove
    | otherwise                 = move restofforest newxpos xmove ymove
    where
        newxpos = xpos + xmove
        restofforest = drop ymove forest
        newrow = head restofforest


checkSlopes :: [(Int, Int)] -> [String] -> [Int]
checkSlopes [slope] forest = [move forest 0 xmove ymove]
    where (xmove, ymove) = slope
checkSlopes (slope:slopes) forest = [move forest 0 xmove ymove] ++ (checkSlopes slopes forest)
    where (xmove, ymove) = slope



main :: IO ()
main = do
    dat <- readFile "./input"
    let rows = lines dat
    let infrows = map cycle rows
    let slopes = [(1,1),(3,1),(5,1),(7,1),(1,2)]
    let numtrees = checkSlopes slopes infrows
    print ("Number of trees encountered " ++ (show numtrees) ++ " their product is: " ++ (show (foldl (*) 1 numtrees)))
