-- doubleMe x = x + x
-- doubleUs x y = doubleMe x + doubleMe y

-- doubleSmallNumber x = if x > 100
--                       then x
--                       else x*2

-- boomBang xs = [if x < 10 then "BOOM!" else "BAH!" | x <- xs, odd x]

triangle = [
    (a,b,c) | 
    c <- [1..10],  
    a <- [1..c],  
    b <- [1..a], 
    a^2 + b^2 == c^2,
    a+b+c==24 ]

lucky:: Int -> String
lucky 7 = "Lucky number 7"
lucky x = "Sorry friend you will be lucky next time"

max' a b
   | a <= b = b
   | otherwise = a

maximum' :: (Ord a) => [a] -> a
maximum' [] = error "максимум в пустом списке" 
maximum' [x] = x
maximum' (x:xs) = max x (maximum' xs)

take' n _  
    | n <= 0   = []
take' _ []     = []
take' n (x:xs) = x: take' (n-1) xs

reverse' :: [a] -> [a]
reverse' [] = [] 
reverse' (x:xs) = reverse' xs ++ [x]
    
applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)