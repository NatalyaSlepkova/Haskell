{-# LANGUAGE InstanceSigs #-}

module Dummy where

import Data.List (sort, words)
import System.Random (newStdGen, randomRs)
import Data.Monoid (Monoid)
import Data.Semigroup

-- Block 1

order3 :: Ord a => (a, a, a) -> (a, a, a)
order3 (a, b, c) =
  let [a2, b2, c2] = sort [a, b, c]
  in (a2, b2, c2)

smartReplicate :: [Int] -> [Int]
smartReplicate = concatMap (\x -> replicate x x)

contains :: Eq a => a -> [[a]] -> [[a]]
contains item = filter (elem item)

stringSum :: String -> Int
stringSum s = sum (map (\x -> read x :: Int) (words s))

testContains :: Bool
testContains =
   let l = [[1..5], [1, 4..8], [7, 18]] in
   let v = 7 in
   let res = contains v l in
   res == [[1:: Int, 4..8], [7:: Int, 18]]

testSmartReplicate :: Bool
testSmartReplicate =
    let l = [1, 3, 1] in
    let res = smartReplicate l in
    head res == 1 && res !! 1 == 3 && res !! 2 == 3 && res !! 3 == 3 && res !! 4 == 1

testOrder3 :: [Int] -> Bool
testOrder3 l =
    let (v1, v2, v3) = order3 (head l, l !! 1, l !! 2) in
    let s = sort l in
    v1 == head s && v2 == s !! 1 && v3 == s !! 2
-- Block 2

--removeElement :: Int -> [x] -> (Maybe x, [x])
--removeElement n l = recur 1
--    where recur _ _ [] = []
--          recur i n (x:xs) = if i == n
--            then

randomIntList :: Int -> Int -> Int -> IO [Int]
randomIntList n from to = take n . randomRs (from, to) <$> newStdGen

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

-- Block 3

data Day = Sun | Mon | Tue | Wen | Fith | Fri | Sat deriving (Eq, Enum, Show, Read)

nextDay :: Day -> Day
nextDay Sun = Mon
nextDay day = succ day

afterDays :: Day -> Int -> Day
afterDays a 0 = a
afterDays a n = nextDay $ afterDays a (n - 1)

isWeekend :: Day -> Bool
isWeekend day
  | day == Sat || day == Sun = True
  | otherwise = False

daysToParty :: Day -> Int
daysToParty = countDays 0
  where
    countDays acc Fri = acc
countDays acc d = countDays (acc + 1) (nextDay d)

testNextDay :: Bool
testNextDay = nextDay Sun == Mon && nextDay Fri == Sat && nextDay Sat ==O Sun

testAfterDays =
    let day = Mon in
    let x = 6 in
    Sun == afterDays day x

testIsWeekend :: Bool
testIsWeekend = isWeekend Sat && isWeekend Sun && not (isWeekend Mon)
-------------

data Nat = Z | S Nat

numToNat :: (Num t, Eq t) => t -> Nat
numToNat n = let makeFromNum num nat_n = case num of
                                       0 -> nat_n
                                       x -> S (makeFromNum (x - 1) nat_n)
                                       in makeFromNum n Z

natToNum :: Num t => Nat -> t -> t
natToNum nat_n n = case nat_n of
    Z -> (+) 0 n
    S x -> natToNum x ((+) n 1)


instance Show Nat where
    show a = show (natToNum a (0::Int))


add :: Nat -> Nat -> Nat
add x y = case y of
    Z -> x
    S yy -> S(add x yy)


addNum :: (Eq t1, Eq t2, Num t1, Num t2) => t1 -> t2 -> Nat
addNum x y = add (numToNat x) (numToNat y)

sub :: Nat -> Nat -> Nat
sub x y = case (x, y) of
    (xx, Z) -> xx
    (Z, _) -> Z
    (S xx, S yy) -> sub xx yy


mul :: Nat -> Nat -> Nat
mul x y = case (x, y) of
    (_, Z) -> Z
    (Z, _) -> Z
    (xx, S yy) -> add (mul xx yy) xx

comp :: Nat -> Nat -> Ordering
comp Z Z = EQ
comp _ Z = GT
comp Z _ = LT
comp (S a) (S b) = comp a b


instance Ord Nat where
  compare :: Nat -> Nat -> Ordering
  compare = comp


instance Eq Nat where
  (==) :: Nat -> Nat -> Bool
  x == y = comp x y == EQ

isEven :: Nat -> Bool
isEven a = even (natToNum a (0::Int))

testAdd :: (Eq a, Num a) => a -> a -> Bool
testAdd x y = natToNum (addNum x y) 0 == x + y
testMul :: (Eq a, Num a) => a -> a -> Bool
testMul x y = natToNum (mul (numToNat x) (numToNat y)) 0 == x * y
testSub :: (Eq a, Num a) => a -> a -> Bool
testSub x y = natToNum (sub (numToNat x) (numToNat y)) 0 == x - y

-- Block 5

maybeConcat :: [Maybe [a]] -> [a]
maybeConcat = foldr f []
  where
    f Nothing acc   = acc
    f (Just li) acc = li ++ acc

eitherConcat :: (Monoid l, Monoid r) => [Either l r] -> (l, r)
eitherConcat = foldr f (mempty, mempty)
  where
    f (Left l) (ls, rs)  = (l `mappend` ls, rs)
    f (Right r) (ls, rs) = (ls, r `mappend` rs)