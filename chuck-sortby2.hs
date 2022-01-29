
import qualified Data.List as L

--getCatFromHouse :: House -> Cat 
--getCatFromHouse (House _ (cat c)) = c
myNumbers :: [Int]
myNumbers = [112, 45, 54, 125, 266, 1033, 5400, 25, 163, 30003, 567, 125, 112, 57, 1098]




mySorted :: [Int] -> Maybe [Int]
mySorted numbers = 
    if length numbersSorted > 0
    then Just numbersSorted
    else Nothing
  where numbersSorted
          = L.sortBy intComparer numbers
        intComparer no_1 no_2
          = compare no_1 no_2


descendSorted :: [Int] -> Maybe [Int]
descendSorted numbers = 
    if length numbersSorted > 0
    then Just numbersSorted
    else Nothing
  where numbersSorted
          = L.sortBy lComparer numbers
        lComparer a1 a2
          = compare a2 a1

descendSortby :: [Int] -> Maybe [Int]
descendSortby numbers =
    if length numbersSorted > 0
    then Just numbersSorted
    else Nothing
  where numbersSorted 
          = L.sortBy lComparer numbers
        lComparer a1 a2
           = compare a2 a1

main :: IO ()
main = do
  putStrLn (" **** Sorted numbers =  " ++ show( mySorted myNumbers))
  putStrLn (" **** descendSortby =  " ++ show( descendSorted myNumbers))