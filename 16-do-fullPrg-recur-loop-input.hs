-- 16. Fridge, The Game

-- http://becauseofgames.com 

-- 16.1. Do Blocks with IO

-- Firstly, all of the actions in an IO do block are executed in the order they’re written.

-- Secondly, using “<-”, you can connect an inner value from an IO action on its right to a variable on its left. 
-- It’s worth noting that you can only use this <- syntax within a do block, though.

-- case expression
-- We can use any Haskell expressions we like in a do block as long as they result in an action of the same type as the do block’s type.

-- getline
-- We can get input from the user in Haskell with getLine. 
--    once the first line has been output, it will wait for the user to put some text in and press return.
-- Once they do that, it will have bound that text into the getLine action, pulled that value out as theName, 
-- and printed out the last line which includes that text the user entered!
-- The type of getLine is IO String, which means it’s an IO action that “contains” a String when it’s executed.

-- 16.2. Do Block Nesting
-- Notice, also, that you can put do blocks within do blocks. This is called nesting. 

-- 16.3. Whole-Program Recursion
-- Next, it asks if they want to play again, and if they do, it runs the whole thing again by calling main. This is recursion of the whole program.

-- 16.4. Homework 
-- Homework is to go for a walk with a pad and pen and write a program to add up a few of the numbers on number plates of cars in your street 
--    (assuming there are lots of cars around, if not, pick something else where there are lots of numbers).
-- Add them up using the (+) function, and use either print or putStrLn with show. 
--    Do this as many times as you need to so that you know how to do it without looking it up.
-- You will probably need to do this in a few different sessions to fully anchor it in your memory. Also, write a function that prints out a greeting.




secondMain :: IO ()
secondMain = do
    putStrLn "What is your name? "
    theName <- getLine
    putStrLn ("You said your name is " ++ theName)



main :: IO ()
main = do
  putStrLn "You are in a fridge. What do you want to do?"
  putStrLn "1. Try to get out."
  putStrLn "2. Eat."
  putStrLn "3. Die."
  putStrLn "4. Get Name"
  command <- getLine
  case command of
    "1" ->
        putStrLn "You try to get out. You fail. You die."
    "2" ->
        do
          putStrLn "You eat. You eat some more."
          putStrLn "Damn, this food is tasty!"
          putStrLn "You eat so much you die."
    "3" ->
        putStrLn "You die."
    "4" ->
        secondMain
    _   ->
        putStrLn "Did not understand."
  putStrLn "Play again? write y if you do."
  playAgain <- getLine
  if playAgain == "y"
  then main       -- recurses back to main
  else putStrLn "Thanks for playing."