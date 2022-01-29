import qualified Data.List as L

myNumbers :: [Int]
myNumbers = [112, 45, 54, 125, 266, 1033, 5400, 25, 163, 30003, 567, 125, 112, 57, 1098, 16]



myHighest :: [Int] -> Maybe Int
myHighest numbers = 
    if length numbersSorted > 0
    then Just (head numbersSorted)
    else Nothing
  where numbersSorted
          = L.sortBy intComparer numbers
        intComparer no_1 no_2
          = compare no_2 no_1


myLowest :: [Int] -> Maybe Int
myLowest numbers = 
    if length numbersSorted > 0
    then Just (head numbersSorted)
    else Nothing
  where numbersSorted
          = L.sortBys intComparer numbers
        intComparer no_1 no_2
          = compare no_1 no_2

mySorted :: [Int] -> Maybe [Int]
mySorted numbers = 
    if length numbersSorted > 0
    then Just numbersSorted
    else Nothing
  where numbersSorted
          = L.sortBy intComparer numbers
        intComparer no_1 no_2
          = compare no_1 no_2

myHead :: [Int] -> Maybe Int
myHead numbers = 
    if length numbers > 1
    then Just (head numbers)
    else Nothing

myGt500 :: Int-> String
myGt500 a = assess
  where assess = if a > 500
                 then "True"
                 else "False"



mergeLists :: [Integer] -> [Integer] -> [Integer]
mergeLists (x:xs) (y:ys) = ( if x < y
                           then x : mergeLists (xs) (y:ys)
                           else y : mergeLists (x:xs) (ys)) 





mergeSort :: [Integer] -> [Integer]
mergeSort [] = []
mergeSort [x] = [x]
mergeSort [x,y] = if x < y
                  then [x,y]
                  else [y,x]
mergeSort a = mergeLists (mergeSort (take l a)) (mergeSort (drop l a)) 
                 where l = round( (length a) / 2)




assessNumbers :: [Int] -> [String]
assessNumbers [] = []
assessNumbers (x:xs) = myGt500 x : assessNumbers xs

-- assessNumbersKeep :: [Int] -> Maybe [Int]
-- assessNumbersKeep [] = Nothing
-- assessNumbersKeep (x:xs) = Just myGt500Keep x : assessNumbersKeep xs


-- https://www.haskelltutorials.com/guides/haskell-lists-ultimate-guide.html
x = takeWhile (\x -> x /= ',') ['H', 'e', 'l', 'l', 'o', ',', 'W', 'o', 'r', 'l', 'd']
y = filter (\x -> x > 500) myNumbers

main :: IO ()
main = do
  putStrLn (" **** myNumber " ++ show(myNumbers))
  putStrLn (" **** Highest number =  " ++ show( myHighest myNumbers))
  putStrLn (" **** Lowest number =  " ++ show( myLowest myNumbers))
  putStrLn (" **** Sorted numbers =  " ++ show( mySorted myNumbers))
  putStrLn (" **** Head numbers =  " ++ show( myHead myNumbers))
  putStrLn (" **** GT500 =  " ++ show( assessNumbers myNumbers))
  putStrLn (" *** takeWhile " ++ show (x))
  putStrLn (" *** takeWhileGT 500 " ++ show (y))
  --putStrLn (" **** GT500Keep =  " ++ show( assessNumbersKeep myNumbers))