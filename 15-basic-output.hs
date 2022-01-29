
import Data.Time.Clock
import Data.Time.Calendar


-- 15.1. Setup Your Environment

-- GHCi
-- :load filename (or :l filename) command
-- then you can run main and other functions by typing main and pressing the return key.

 -- :type (or :t) command in front of an expression
 -- use :reload (or :r) to reload the file

 -- 15.2. putStrLn, print and String

-- 15.3. Ways To Solve Problems
-- You should not look at the example run-throughs when you’re doing your own programs, 
--     otherwise you won’t build your own real-world usage understanding.


-- 15.4. Guided Exercise 1: Display Hello

-- Running from GHCI
-- *Main> main
-- Hello real-world
-- *Main> :t str1
-- str1 :: String
-- *Main> :t main
-- main :: IO ()


-- 15.5. Guided Exercise 2: Display the Sum of Two Numbers

-- *Main> :t print
-- print :: Show a => a -> IO ()

-- 15.6. Guided Exercise 3: Display the Product of Two Numbers

-- 15.7. Reader Exercise 1
-- Task: Write a program that prints "This sentence is false" on the screen. Once you’ve done this, make it say "No it's not" instead. 
--        Once you’ve done this, change it to say "One plus two is not seven".

-- 15.8. Reader Exercise 2
-- Task: Write a program that prints the number 20938 on the screen.

-- 15.9. Reader Exercise 3
-- Task: Write a program that adds the number of whatever the current month is right now
--    (1 is for January, 2 is for February etc.) to the current year,and prints it on the screen. 
--    (Note: it needn’t actually generate or get the current month, you just write it in as a number yourself).


-- 15.10. Reader Exercise 4
-- Task: Write a program that multiplies your current age with your mother’s current age and prints it on the screen. 
--  (Just write the values you know for these ages in as numbers, you don’t need to make the computer actually get the numbers from the user).

-- 15.11. Reader Exercise 5
-- Task: Write a program that adds the numbers 5, 7, 8 and 9 together and prints the result on the screen.

-- 15.12. Reader Exercise 6
-- Task: Write a program that subtracts 999 from 1098 and prints it on the screen

current_month :: Int
current_month = 1

current_year :: Int
current_year = 2022

now1 = getCurrentTime
-- (year1, month1, day1) = toGregorian $ utctDay now1

date :: IO (Integer,Int,Int) -- :: (year,month,day)
date = getCurrentTime >>= return . toGregorian . utctDay


str1 :: String
str1 = "Hello real-world"

num1 :: Int
num1 = 1028 + 230

main :: IO ()
main = do
  putStrLn (str1)
  print num1
  print (5 * 10)
  print(current_month + current_year)
  now <- getCurrentTime
  let (year, month, day) = toGregorian $ utctDay now
  -- let yearPlus1 = year + month
  putStrLn $ "Year: " ++ show year
  putStrLn $ "Month: " ++ show month
  putStrLn $ "Day: " ++ show day
  print day
  print =<< getCurrentTime
  print (51 * 81)
  print (5 + 7 + 8 + 9)
  print (1098 - 999)