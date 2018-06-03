{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE InstanceSigs #-}

import           Criterion.Types (Benchmark)
import           Control.Monad               (unless)
import           Control.Monad.ST            (ST, runST)
import           Criterion.Main              (bench, bgroup, defaultMain, nf)
import           Data.Foldable               (forM_)
import           Data.List                   (sort)
import           Data.Foldable    (forM_)
import           System.Random 

main :: IO ()
main = defaultMain $
    let n     = 1000
        half  = n `quot` 2
        list1 = ([1..n],                          "Sorted " ++ show n)
        list2 = ([n,(n-1)..1],                    "Sorted-reversed " ++ show n)
        list3 = ([1..half] ++ [half,(half-1)..1], "Pick " ++ show n)
        list4 = (randomList n 1,                  "Random " ++ show n)

    in map makeBenchGroup [list1, list2, list3, list4]
  where
    makeBenchGroup :: ([Int], String) -> Benchmark
    makeBenchGroup (list, name) =
        bgroup name
        [ bench "List.sort" $ nf sort              list
        , bench "heapSort"  $ nf heapSort list
        , bench "mergeSort" $ nf mergeSort         list
        , bench "quickSort" $ nf quickSort         list
        ]

randomList :: Int -> Int -> [Int]
randomList 0 _ = []
randomList 1 x = [x]
randomList n x = let a = randomList (n - 1) x in (next (head a) : a)
  where
    next :: Int -> Int
    next i = (911 * i + 701) `rem` 1000000007

-- MergeSort

mergeSort :: Ord x => [x] -> [x]
mergeSort [] = []
mergeSort [x] = [x]
mergeSort l =
    let (left, right) = splitAt (length l `div` 2) l in
    merge (mergeSort left) (mergeSort right) where
      merge xs [] = xs
      merge [] xs = xs
      merge (x:xs) (y:ys)
        | x > y = y : merge (x:xs) ys
        | otherwise = x : merge xs (y:ys)

-- HeapSort

data Heap a = Empty | Tree Int a (Heap a) (Heap a)

getRank :: Heap a -> Int
getRank (Tree r _ _ _) = r
getRank Empty          = 0

mk :: a -> Heap a -> Heap a -> Heap a
mk x a b =
    if getRank a <= getRank b then
      Tree (getRank a + 1) x b a
    else
      Tree (getRank b + 1) x a b

mergeHeaps :: Ord a => Heap a -> Heap a -> Heap a
mergeHeaps h Empty = h
mergeHeaps Empty h = h
mergeHeaps h1@(Tree _ x a1 b1) h2@(Tree _ y a2 b2) =
    if x >= y then
      mk x a1 (mergeHeaps b1 h2)
    else
      mk y a2 (mergeHeaps h1 b2)

toList :: Ord a => [a] -> Heap a -> [a]
toList xs Empty          = xs
toList xs (Tree _ x a b) = toList (x:xs) $ mergeHeaps a b

heapSort :: Ord a => [a] -> [a]
heapSort [] = []
heapSort xs = toList [] (foldr (\x -> mergeHeaps (mk x Empty Empty)) Empty xs)

-- QuickSort

partition :: Ord a => [a] -> a -> ([a], [a])
partition []     _     = ([], [])
partition (x:xs) pivot = let (l, r) = partition xs pivot in
    if x < pivot then (x:l, r) else (l, x:r)

findPivot :: Ord a => [a] -> Int -> (a, [a])
findPivot as piv = (as!!piv, take piv as ++ drop (piv + 1) as)

quickSort :: forall a. Ord a => [a] -> [a]
quickSort arr = go $ zip arr $ randomList (length arr) 701
  where
    go :: [(a, Int)] -> [a]
    go []               = []
    go xs@((_, seed):_) =
        let index = seed `rem` length xs
            (pivot, rest) = findPivot xs index
            (l, r) = partition rest pivot
        in go l ++ (fst pivot : go r)








