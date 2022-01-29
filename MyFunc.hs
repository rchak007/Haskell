
main :: IO ()



-- main = putStrLn "Yay"

-- the function that always returns True no matter what Bool it gets
--message :: String
--lambdaCoin :: Bool -> Bool
--lambdaCoin = \_ -> True

--message = "Yay"

number :: Integer
y :: Integer
y  = 10
myfunc :: Integer -> Integer
myfunc = \x -> x + 5 + y

number = myfunc 3

main = do 
 putStrLn (show number)
 print y

--if lambdaCoin == True
 --then message "Yay"
 --else message "Nay"
-- the value True, by applying the above lambdaCoin
-- function to the value False
--newCoin :: Bool
--newCoin = lambdaCoin False
--
--if newCoin == True  message = "newCoin is true"

-- the value True, by applying the above lambdaCoin
-- function to newCoin which is itself arrived at by
-- applying lambdaCoin to False
--newCoinAgain :: Bool
--newCoinAgain = lambdaCoin newCoin

-- this is another way to write newCoinAgain,
-- but explicitly spelling out
-- all of the applications of lambdaCoin
--newCoinAgain' :: Bool
--newCoinAgain' = lambdaCoin (lambdaCoin False)