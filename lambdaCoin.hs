
main :: IO ()


main = putStrLn (show newCoin)
-- main = putStrLn "Yay"

-- the function that always returns True no matter what Bool it gets
message :: String
lambdaCoin :: Bool -> Bool
lambdaCoin = \_ -> True

message = "Yay"

number :: Integer
myfunc :: Integer -> Integer
myfunc = \x -> x + 5

number = myfunc 3
--if lambdaCoin == True
 --then message "Yay"
 --else message "Nay"
-- the value True, by applying the above lambdaCoin
-- function to the value False
newCoin :: Bool
newCoin = lambdaCoin False
--
--if newCoin == True  message = "newCoin is true"

-- the value True, by applying the above lambdaCoin
-- function to newCoin which is itself arrived at by
-- applying lambdaCoin to False
newCoinAgain :: Bool
newCoinAgain = lambdaCoin newCoin

-- this is another way to write newCoinAgain,
-- but explicitly spelling out
-- all of the applications of lambdaCoin
newCoinAgain' :: Bool
newCoinAgain' = lambdaCoin (lambdaCoin False)

lambdaCoin'' :: Bool -> Bool
lambdaCoin'' True  = True
lambdaCoin'' False = True
