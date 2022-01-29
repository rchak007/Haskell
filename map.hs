list = ["chuck", "gowri", "venkat", "vish", "kishore", "uma", "Arun", "lalith", "chitrra", "kumar", "bobby"]

catList :: [String] -> [String]
catList [] = []
catList [x] = [categorizeLettter x]
catList (x:xs) = (categorizeLettter x : catList xs)


categorizeName :: String -> String
categorizeName [] = []
categorizeName c = "category"

-- categorizeName :: String -> String
-- categorizeName [] = []
-- categorizeName [c] = categorizeLettter c
-- categorizeName (c:cs) = (categorizeLettter c : categorizeName cs)

categorizeLettter ::[Char] -> String
categorizeLettter [] = []
categorizeLettter [c] = if c < 'g' 
          then "category 1"
          else if c < 'q'
               then "category 2"
               else "categoory 3" 
categorizeLettter (c:cs) = if c < 'g' 
          then "category 1"
          else if c < 'q'
               then "category 2"
               else "categoory 3" 

catMap = map categorizeLettter list

listNum = [100, 300, 2, 6, 75, 80]

listMap3 = map (*3) listNum

main :: IO ()
main = do
    putStrLn (" List =  " ++ show (list))
    putStrLn (" catlist not map  =  " ++ show (catList list))
    putStrLn (" catlist with map =  " ++ show (catMap))
    putStrLn (" Numbers =  " ++ show (listNum))
    putStrLn (" Numbers *3 =  " ++ show (listMap3))