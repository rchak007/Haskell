

number :: Integer
number = 100390 + 29389

-- Type variables
-- (+) :: Num a => a -> a -> a


-- both types must be the same,
-- so this will be fine:
goodNumber = (3 :: Integer) + (5 :: Integer)

-- this would cause a type error
--willNotWork = (3 :: Int) + (5 :: Integer)

-- Type Variables can be named anything
-- it's a lot smaller to write
-- 'a' than 'theNumber'
--(+) :: Num theNumber =>
--       theNumber ->
--       theNumber ->
--       theNumber


-- Typeclasses
-- A typeclass is not a concrete type like Integer, Int or String. 
-- It’s a way of tagging many types (a “class” of them, 
--     if you like) so that we can have functions or values that work with many similar types that do similar things, 
--     but that are actually different. 



-- a small int 5:
intFive = 5 :: Int

-- a "floating-point" value of 10.3
floatTenPointThree = 10.3 :: Float

-- add them together with (+). This will not work...
-- because both types are concretized (or specialized)
-- errorResult = intFive + floatTenPointThree

-- add them together with (+). This will work
result = (fromIntegral intFive) + floatTenPointThree

-- the result is 15.3
-- we’re using fromIntegral to build an “unspecialised” Num a => a 

 -- Num typeclass means there actually isn’t only one definition for the functions for addition: 
   -- (+), subtraction: (-), multiplication: (*), negation: negate, etc but rather that each type — that is, 
   --	each instance of Num — has its own definition for each of these functions.

-- The Show Typeclass

-- there’s a typeclass called Show (with a big S), and this provides a single function: 
   -- show (with a small s), that can take any instance of Show, and makes a String version of it.
-- takes a "showable" thing
-- and returns a String
-- show :: Show a => a -> String

-- Parentheses and Precedence

--  The print Function
number1 :: Num a => a
number1 = 1 + 5 + 7 + 3 + 2

number2 :: Num a => a
number2 = number1 * number1




main :: IO ()

main = do 
 putStrLn ("number = " ++ show (number))  
 putStrLn ("goodNumber = " ++ show (goodNumber))
 putStrLn ("result = " ++ show (result))
 print number1
 print number2




