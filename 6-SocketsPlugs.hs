-- 6. Sockets & Plugs

-- Defining Functions

plus5 :: Int -> Int
plus5 x = x + 5


-- variable name here (we called it number), rather than x.
plus6 :: Int -> Int
plus6 number = number + 6

--  infix operator such as (+), and you want to use it as a prefix function, 
--     you can just wrap it in parentheses. 

plus6' :: Int -> Int
plus6' number = (+) number 6

-- ***************************************  General Infix vs PreFix
-- Infix : An expression is called the Infix expression if the operator appears in between the operands in the expression.
-- Simply of the form (operand1 operator operand2). 
-- Example : (A+B) * (C-D)

-- Prefix : An expression is called the prefix expression if the operator appears in the expression before the operands. 
	-- Simply of the form (operator operand1 operand2).
	-- Example : *+AB-CD (Infix : (A+B) * (C-D) )
-- Computers usually does the computation in either prefix or postfix (usually postfix). 
-- But for humans, its easier to understand an Infix expression rather than a prefix. Hence conversion is need for human understanding.

-- https://wuciawe.github.io/functional%20programming/haskell/2016/07/03/infix-functions-in-haskell.html

-- Infix functions
--Functions in Haskell default to prefix syntax, meaning that the function being applied is at the beginning of the expression rather than the middle.
--Operators are functions which can be used in infix style. All operators are functions.
--The syntax between prefix functions and infix functions is interchangeable, with a small change:

--prefixFunc a b

--a `prefixFunc` b

--a infixFunc b

--(infixFunc) a b



-- Operator Sections
--   another identical function, but using what’s called a section.
plus6'' :: Int -> Int
plus6'' number = (+6) number

-- commutative property.

-- Here are four functions that are identical in result:


sevenPlus :: Int -> Int
sevenPlus number = (7+) number

sevenPlus' :: Int -> Int
sevenPlus' = (7+)

plusSeven :: Int -> Int
plusSeven number = (+7) number

plusSeven' :: Int -> Int
plusSeven' = (+7)



main :: IO ()

main = do 
  putStrLn ("plus5 = " ++ show (plus5 10))
  putStrLn ("plus6 = " ++ show (plus6 10))
  putStrLn ("plus6' = " ++ show (plus6' 10))
  putStrLn ("plus6'' = " ++ show (plus6'' 10))
  putStrLn ("sevenPlus = " ++ show (sevenPlus 10))
  putStrLn ("sevenPlus' = " ++ show (sevenPlus' 10))
  putStrLn ("plusSeven = " ++ show (plusSeven 10))
  putStrLn ("plusSeven' = " ++ show (plusSeven' 10))
