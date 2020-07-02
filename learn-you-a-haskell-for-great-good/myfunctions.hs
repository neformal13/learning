import Binary
import Data.Char
import Data.List
twoDigits2Int :: Char -> Char -> Int
twoDigits2Int x y 
    | isDigit x && isDigit y = digitToInt x * 10 + digitToInt y
    | otherwise = 100

dist :: (Double, Double) -> (Double, Double) -> Double
dist (x1, y1) (x2, y2) = 
    sqrt $ (x1 - x2) ^ 2 + (y1 - y2)^2

doubleFact :: Integer -> Integer
doubleFact 0 = 1
doubleFact 1 = 1
doubleFact n | n > 0 = n * doubleFact (n - 2)
             | otherwise = error "Facrial should be grater then 0"

fibonacci (-1) = 1
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n | n > 0 = fibonacci (n - 1) + fibonacci (n - 2)
            | n < 0 = fibonacci (n + 2) - fibonacci (n + 1)

fibonacci' (-1) = 1
fibonacci' 0 = 0
fibonacci' 1 = 1
fibonacci' n = helper 0 1 n
             
helper a _ 0 = a 
helper a b n | n > 0 = helper b (b + a) (n - 1)
             | n < 0 = helper b (a - b) (n + 1)

seqA :: Integer -> Integer
seqA 0 = 1
seqA 1 = 2
seqA 2 = 3
seqA n = let 
    helper a b c 0 = a  
    helper a b c n = helper (a + b - 2 * c) a b (n - 1)
    in helper 3 2 1 (n - 2)


sum'n'count :: Integer -> (Integer, Integer)
sum'n'count n = 
    (calculateSum 0 (abs n), calculateLength 0 (abs n))
    where  
        calculateSum acc 0 = acc
        calculateSum acc x = calculateSum (acc + x `mod` 10)  (x `div` 10)
        
        calculateLength 0 0 = 1
        calculateLength acc 0 = acc
        calculateLength acc x = calculateLength  (acc + 1)  (x `div` 10)
 
integration :: (Double -> Double) -> Double -> Double -> Double
integration f a b 
    | a == b = 0
    | otherwise = h * (((f a + f b) / 2) + sum 0 1)
    where 
        n = 1000
        h = ( b - a) / n
        sum acc n | n == 1000 = acc
                  | otherwise = sum (acc + f (a + h * n)) (n+1)

on3 :: (b -> b -> b -> c) -> (a -> b) -> a -> a -> a -> c
on3 op f x y z = op (f x) (f y) (f x)

class Printable a where
    toString:: a -> String

instance Printable Bool where
    toString True = "true"
    toString False = "false"

instance Printable () where
    toString _ = "unit type"

instance (Printable a, Printable b) => Printable (a, b) where 
    toString (a,b) = "(" ++ toString a  ++ "," ++ toString b ++ ")"

{- -}
class KnownToGork a where
    stomp :: a -> a
    doesEnrageGork :: a -> Bool

class KnownToMork a where
    stab :: a -> a
    doesEnrageMork :: a -> Bool

class (KnownToGork a, KnownToMork a) => KnownToGorkAndMork a where
    stompOrStab :: a -> a
    stompOrStab a | doesEnrageMork a && doesEnrageGork a = stomp . stab $ a
                  | doesEnrageMork a = stomp a
                  | doesEnrageGork a = stab a
                  | otherwise = a

class (Eq a, Enum a, Bounded a) => SafeEnum a where
  ssucc :: a -> a
  ssucc a | maxBound == a = minBound
          | otherwise = succ a

  spred :: a -> a
  spred a | minBound == a = maxBound
          | otherwise = pred a

avg :: Int -> Int -> Int -> Double
avg a b c = (fromIntegral a + fromIntegral b + fromIntegral c ) / 3

bar x y z = x + y
foo a b = bar a a (a + b)
value = foo (3 * 10) (5 - 2)

{-
~> value = foo (3 * 10) (5 - 2)
~> bar p d (p + d)
~> p + d 
-}

addTwoElements :: a -> a -> [a] -> [a]
addTwoElements  x1 x2 xs = x1:x2:xs

nTimes:: a -> Int -> [a]
nTimes x 0 = []
nTimes x n = x:nTimes x (n-1)

oddsOnly :: Integral a => [a] -> [a]
oddsOnly [] = []
oddsOnly (x:xs) = 
    if (x `mod` 2) == 1 
        then x:oddsOnly xs
        else oddsOnly xs 

isPalindrome :: Eq a => [a] -> Bool
isPalindrome [ ] = True
isPalindrome [_] = True
isPalindrome xs = 
    if head xs == last xs 
        then  isPalindrome $ init $ tail $ xs
        else False

sum3 :: (Eq a, Num a) => [a] -> [a] -> [a] -> [a]
sum3 [] [] [] = []
sum3 x y z = 
    (if x == [] then 0 else head x) +  
    (if y == [] then 0 else head y) + 
    (if z == [] then 0 else head z) : 
    sum3
        (if x == [] then [] else tail x) 
        (if y == [] then [] else tail y)  
        (if z == [] then [] else tail z)

groupElems :: Eq a => [a] -> [[a]]
groupElems   [] = []
groupElems   [x] = [[x]]
groupElems (a:arr) = 
    helper [] [a] arr
    where 
        helper acc  group    [] = acc ++ [group]
        helper acc group (x:xs) = 
            if x == head group
                then helper acc (x:group) xs
                else helper (acc ++ [group]) [x] xs

readDigits :: String -> (String, String)
readDigits = span isDigit 

filterDisj :: (a -> Bool) -> (a -> Bool) -> [a] -> [a]
filterDisj f1 f2 = filter (\a -> (f1 a || f2 a))

qsort :: Ord a => [a] -> [a]
qsort [] = []
qsort (x:xs) =
    let smallerSorted = qsort (filter (<= x) xs)
        biggerSorted  = qsort (filter (> x) xs)
    in  smallerSorted ++ [x] ++ biggerSorted

squares'n'cubes :: Num a => [a] -> [a]
squares'n'cubes = concatMap (\a -> [a^2, a^3])

perms :: [a] -> [[a]]
perms [] = [[]]
perms [x] = [[x]]
perms [x, y] = [[x,y], [y, x]]
perms xs = 
    concatMap (\(x, xs') -> map (\as -> x:as) $ (perms xs')) 
    $ mapToPair xs

mapToPair xs = map (\n -> (xs!!n, dropElbyIndex n xs)) $ (toIndex xs)

dropElbyIndex n [] = []
dropElbyIndex n xs = 
    join split
    where 
        split = splitAt n xs
        join (a, []) = a
        join (a, x:b) = a ++ b

toIndex xs = take (length xs) [0..]


delAllUpper :: String -> String
delAllUpper sentance = unwords . filter ( not . (all isUpper) ) . words $ sentance

max3 :: Ord a => [a] -> [a] -> [a] -> [a]
max3 = zipWith3 (\a b c -> max a $ max b c)

fibStream :: [Integer]
fibStream = zipWith (+) (0:fibStream) (0:1:fibStream)
    
data Odd = Odd Integer 
  deriving (Eq, Show)

instance Enum Odd where
    succ (Odd a) = Odd (a + 2)
    pred (Odd a) = Odd (a - 2)
    
    -- toEnum:: Int -> Odd
    toEnum  a = Odd (fromIntegral a)
    
    -- fromEnum:: Odd -> Int
    fromEnum (Odd a) = fromIntegral a 

    enumFrom (Odd a) = map Odd [a, a+2..]
    enumFromTo (Odd a) (Odd b) = map Odd [a, a+2..b]
    enumFromThen  (Odd a) (Odd b) = map Odd [a, b..]
    enumFromThenTo (Odd a) (Odd b) (Odd c)  = map Odd [a, b ..c]
    

-- change :: (Ord a, Num a) => a -> [[a]]
-- change a = [change | 
--         z <- [ replicate x 7 | x <- [0..(a `div` 7)]],
--         y <- [ replicate x 3 | x <- [0..(a `div` 3)]],
--         x <- [ replicate x 2 | x <- [0..(a `div` 2)]],
--         change <- [x ++ y ++ z],
--         sum (change) == a
--     ] 

coins = [2, 3, 7]

change a
    | a < 0 = []
    | a == 0 = [[]]
    | otherwise = [x:y | x <- coins, y <- change (a-x)]

meanList :: [Double] -> Double
meanList = (uncurry (/)) . foldr (\x (s, n) -> (s + x, n +1)) (0, 0)

evenOnly :: [a] -> [a]
evenOnly = foldr ( \(n,x) acc -> if n `mod` 2 == 0 then x:acc else acc )  []  . zip [1..]

revRange :: (Char,Char) -> [Char]
revRange = unfoldr g 
  where g = (\ x@(a,b) -> if b >= a then Just ( b , (a, pred b) ) else Nothing)


data Color = Red | Green | Blue