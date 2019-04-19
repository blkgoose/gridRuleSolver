module Main where

data Tree a = Node a [Tree a] deriving Show

main = print out
    where
        start = Node 1 []
        steps = 5
        moves = [-3, 3, -30, 30, -22, 22, -18, 18]

        next :: Int -> [Int]
        next p = map (+p) moves
        -- Add bounds

        gen :: Tree Int -> Int -> Tree Int
        gen p 0 = p
        gen (Node v bra) n = Node v nb
            where
                nb = map (\a -> Node a []) x
                x  = filter (>0) $ filter (<=100) $ next v

        out = gen start 1
