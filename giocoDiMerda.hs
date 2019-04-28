module Main where

import Data.List
import Data.Ord

data Tree a = Node a [Tree a] deriving Show
type Node = Tree [Vec]

type Vec = (Int, Int)

(§+)  (ax, ay) (bx, by) = (ax+bx, ay+by)
(§<=) (ax, ay) (bx, by) = ax <= bx && ay <= by
(§>=) (ax, ay) (bx, by) = ax >= bx && ay >= by
(|>) x f = f x

nodeVal :: Tree a -> a
nodeVal (Node v _) = v

nodeBra :: Tree a -> [Tree a]
nodeBra (Node _ b) = b

inList :: (Foldable t, Eq a) => t a -> a -> Bool
inList = flip elem

main = print solution
    where
        next :: Node -> [Node]
        next (Node pos _) = out
            where
            at    = head pos
            moves = [(0,3),(0,-3),(3,0),(-3,0),(2,2),(-2,2),(2,-2),(-2,-2)]
            out = moves
                  |> map    (at §+)
                  |> filter (not . inList pos)
                  |> filter (§<= (10,10))
                  |> filter (§>= (1,1))
                  |> map    (\x -> Node (x : pos) [])

        gen :: Int -> [Node] -> [Node]
        gen 0 nodes = nodes
        gen n nodes = map (\oldNode -> Node (nodeVal oldNode) (gen (n-1) $ next oldNode)) nodes

        steps = 50

        endNodes :: Node -> [[Vec]]
        endNodes (Node v []) = [v]
        endNodes (Node _ b) = concat $ map endNodes b

        tree :: [Node]
        tree = gen steps everyPos
            where
                everyPos = map (\p -> Node [p] []) [(x, y) | x <- [1..10], y <- [1..10]]

        solution = concatMap endNodes tree
                   |> map (reverse)
                   |> filter (\p -> length p == steps + 1)
                   |> take 1
