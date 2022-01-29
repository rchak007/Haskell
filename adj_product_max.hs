

import Data.List
list = [1,5,3,7,2,1,8,5,2,1,3]
-- list = [1, 5, 7, 2, 8]

listTuples :: [Integer] -> [(Integer, Integer)]
listTuples [] = []
listTuples [x,x1] = [(x,x1)]
listTuples (x:x1:xs) = (x,x1) : listTuples (x1:xs)
ltp = listTuples list

listProduct :: [Integer] -> [Integer]
listProduct [] = []
listProduct [x,x1] = [x*x1]
listProduct (x:x1:xs) = x*x1 : listProduct (x1:xs)

lp = listProduct list

sortProduct :: [Integer] -> [Integer]
sortProduct [] = []
sortProduct [x] = [x]
sortProduct [x,x1] = if x > x1 
                        then [x, x1]
                        else [x1, x]
sortProduct (x:x1:xs) = if x > x1
                        then (x : sortProduct (x1:xs))
                        else (x1 : sortProduct (x:xs))

len = length (listProduct list)

finalSortedList = iterate sortProduct lp !! len
highestProd = head finalSortedList

indexHighestProduct = elemIndex highestProd lp

-- the "!!" operator return the n-th element from the list
-- *this is zero based
str2 = "string" !! 2 -- > "r"
num1 = [10, 20, 30, 40] !! 1 -- > 20

tupleHighest :: Maybe (Integer,Integer)
tupleHighest = case indexHighestProduct of
                 Just indexHighestProduct -> Just (ltp !! indexHighestProduct )
                 Nothing -> Nothing


main :: IO()
main = do
    -- putStrLn "*****   "
    putStrLn ("Intial List = " ++ show (list))
    putStrLn ("list Adjacent numbers as Tuples = " ++ show (listTuples list))
    putStrLn ("list all Product of Adjacent numbers = " ++ show (listProduct list))
    putStrLn ("Length listProduct = " ++ show (len))
    -- putStrLn ("sortProduct 1 call = " ++ show (sortProduct (lp)))
    putStrLn ("Final list sorted to find higest Product = " ++ show (finalSortedList))
    putStrLn ("Highes prod number = " ++ show (highestProd))
    -- putStrLn ("print 2nd element str = " ++ show(str2))
    -- putStrLn ("print 1st element int list = " ++ show(num1))
    putStrLn ("Index of highest Product = " ++ show(indexHighestProduct))
    putStrLn ("Adjacent number Tuple of highest Product = " ++ show(tupleHighest))