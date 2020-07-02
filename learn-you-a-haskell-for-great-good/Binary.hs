module Binary 
    (Bit (Zero, One)
    ,Sign (Minus, Plus)
    ,Z (Z)
    ,add
    ,mul
    ) 
where

import Data.List

data Bit = Zero | One deriving (Eq)
data Sign = Minus | Plus deriving (Eq)
data Z = Z Sign [Bit] deriving (Eq)

padRight toLenght xs = let diff = toLenght - (length xs) in xs ++ (replicate diff Zero)

removeLeadingZeros [] = []
removeLeadingZeros xs = let leadingBit = last xs in if leadingBit /= Zero then xs else removeLeadingZeros (init xs)

addBinaries a b = unfoldr sum' (a', b', Zero) where 
        toLenght =  max (length a)  (length b)

        a' = padRight toLenght a
        b' = padRight toLenght b

        sum' ([]  , []   , Zero) = Nothing
        sum' ([]  , []   , One ) = Just ( One, ([], [], Zero))
        sum' (a:as, b:bs , z   ) = 
                                  case (a, b, z) of 
                                    (Zero, Zero, Zero) -> Just (Zero, (as, bs, Zero))
                                    
                                    (Zero, Zero, One ) -> Just (One, (as, bs, Zero))
                                    (Zero, One,  Zero) -> Just (One, (as, bs, Zero))
                                    (One,  Zero, Zero) -> Just (One, (as, bs, Zero))

                                    (Zero, One,  One ) -> Just (Zero, (as, bs, One))
                                    (One,  Zero, One ) -> Just (Zero, (as, bs, One))
                                    (One,  One, Zero ) -> Just (Zero, (as, bs, One))
                                    
                                    (One,  One, One ) -> Just (One, (as, bs, One))

complement:: [Bit] -> [Bit]
complement [] = []
complement a = 
    addBinaries ones (reverse a)
    where 
          ones = (padRight (length a) [One])
          reverse [] = []
          reverse (a:as) = (if a == Zero then One else Zero) : reverse as

uncomplement a =
    if (last a) == Zero 
        then Z Plus  $ removeLeadingZeros a
        else Z Minus $ removeLeadingZeros $ complement a

substract (Z Plus a) (Z Minus b) = 
    if sum' == [] then (Z Plus []) else uncomplement sum' 
    where 
        toLenght =  1 + max (length a) (length b)

        a' = padRight toLenght a
        b' = padRight toLenght b

        complementaryB' = complement b'
        sum' = take toLenght (addBinaries a' complementaryB')

add :: Z -> Z -> Z
add (Z Plus  as) (Z Plus  bs) =  Z Plus $ addBinaries as bs
add (Z Minus as) (Z Minus bs) =  Z Minus $ addBinaries as bs
add  a@(Z Plus  _) b@(Z Minus _) =  substract a b
add  a@(Z Minus _) b@(Z Plus  _) =  substract b a

mul :: Z -> Z -> Z
mul (Z _ []) _ = Z Plus []
mul _ (Z _ [])  = Z Plus []
mul (Z signA as) (Z signB bs) = Z sign $ removeLeadingZeros $ snd $ foldl mult' (0, []) as 
    where 
        sign = if signA == signB then Plus else Minus
        mult' (n, acc) a = (n +1, if a == Zero then acc else (addBinaries (padRight n acc) ((replicate n Zero) ++ bs))) 
