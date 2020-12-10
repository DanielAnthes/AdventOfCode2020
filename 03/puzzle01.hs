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

-- >>> main
main :: IO ()
main = do
    dat <- readFile "./input"
    let rows = lines dat
    let infrows = map cycle rows
    let numtrees = move infrows 0 3 1
    print (show numtrees ++ " trees encountered")