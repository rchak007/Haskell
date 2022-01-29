-- Function magic

--simplest possible types has only two values. 
--True :: Bool
--False :: Bool



--(\x -> True) :: Bool -> Bool


--(\x -> True) True -- results in True


lambdaCoin :: Bool -> Bool
lambdaCoin = \_ -> True


-- the value True, by applying the above lambdaCoin
-- function to the value False
newCoin :: Bool
newCoin = lambdaCoin False

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


-- There’s another way to write this function in Haskell.
-- We don’t have lambda syntax here. This is regular function definition syntax. 
-- We’re using “_” to pattern-match on any value at all, and not use it.
lambdaCoin' :: Bool -> Bool
lambdaCoin' _ = True


--function that flips a boolean value to the other value.
not' :: Bool -> Bool
not' True = False
not' False = True

--Functions that Return Functions
lambdaCoinTakesTwo :: Bool -> (Bool -> Bool)
lambdaCoinTakesTwo = \_ -> lambdaCoin

-- Variouos ways to write the functions
lambdaCoinTakesTwo' :: Bool -> Bool -> Bool
lambdaCoinTakesTwo' = \_ -> (\_ -> True)

-- using two lambdas,
-- without parentheses
lambdaCoinTakesTwo'1 :: Bool -> Bool -> Bool
lambdaCoinTakesTwo'1 = \_ -> \_ -> True

-- using just one lambda
lambdaCoinTakesTwo'2 :: Bool -> Bool -> Bool
lambdaCoinTakesTwo'2 = \_ _ -> True

-- a more normal way of defining the function
lambdaCoinTakesTwo'' :: Bool -> Bool -> Bool
lambdaCoinTakesTwo'' _ _ = True




isEitherTrue :: Bool -> Bool -> Bool
isEitherTrue False False = False
isEitherTrue _     _     = True

tallEnough = False 
isAdult = False
canRideScaryRide = isEitherTrue tallEnough isAdult -- False

-- logical or operator
canRideScaryRide2 = tallEnough || isAdult

-- process of making a function that returns a function itself is called currying
plus :: Int -> Int -> Int
plus x y = x + y

plus' :: Int -> Int -> Int
plus' = \x -> \y -> x + y

increment :: Int -> Int
increment = plus 1

increment' :: Int -> Int
increment' = (\x -> \y -> x + y) 1

decrement :: Int -> Int
decrement = (\x -> \y -> x - y) 1      -- 1 is x and another value supplied becomes the y

additionResult :: Int
additionResult = plus 100 25



myfunc1 :: Int -> ( Int -> Int )
myfunc1 = \x -> \y -> x * y + x + y



main :: IO ()

main = do 
 putStrLn ("lambdaCoin = " ++ show (lambdaCoin False))  -- True
 putStrLn ("newCoin = " ++ show newCoin) -- True
 putStrLn ("newCoinAgain = " ++ show newCoinAgain) -- True
 putStrLn ("newCoinAgain' = " ++ show newCoinAgain') -- True
 putStrLn ("lambdaCoin' = " ++ show ( lambdaCoin' False) )  -- True
 putStrLn ("not' false = " ++ show ( not' False) )  -- True
 putStrLn ("not' true = " ++ show ( not' True) )  -- False

 putStrLn ("lambdaCoinTakesTwo F F = " ++ show ( lambdaCoinTakesTwo False False) )  -- True
 putStrLn ("lambdaCoinTakesTwo T F = " ++ show ( lambdaCoinTakesTwo True False) )  -- True
 putStrLn ("lambdaCoinTakesTwo F T = " ++ show ( lambdaCoinTakesTwo False True) )  -- True
 putStrLn ("lambdaCoinTakesTwo T T  = " ++ show ( lambdaCoinTakesTwo True True) )  -- True

 putStrLn ("lambdaCoinTakesTwo' T T  = " ++ show ( lambdaCoinTakesTwo' True True) )  -- True

 putStrLn ("lambdaCoinTakesTwo'' F F  = " ++ show ( lambdaCoinTakesTwo'' False False) )  -- True
 
 putStrLn ("canRideScaryRide = " ++ show (canRideScaryRide))
 putStrLn ("canRideScaryRide2 = " ++ show (canRideScaryRide2))

 putStrLn ("increment' = " ++ show (increment' 5))
 putStrLn ("increment = " ++ show (increment 7))
 putStrLn ("decrement = " ++ show (decrement 4))
 putStrLn ("myfunc = " ++ show (myfunc1 3 4))
 putStrLn ("myfunc more = " ++ show ( 5 `myfunc1` 6 `myfunc1` 7  ))       -- Using myFunc prefix function as Infix function

