
myList :: [Int]
myList = [1,2,3,4,5,100, 500, 600, 0, 3, 2000, 700, 97687]


band :: Int -> Int
band a =
    if a > 1000 
    then 1
    else if a > 100
         then 2
         else if a > 10
              then 3
              else 4

assessInt :: [Int] -> [Int]
assessInt = map band

assesedInt :: [Int]
assesedInt = assessInt myList

main :: IO ()
main = do
    putStrLn (" *****       myList = " ++ show(myList))
    putStrLn (" *****  mapped list =   " ++ show(assesedInt))