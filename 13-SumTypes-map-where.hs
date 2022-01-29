
import qualified Data.List as L
-- Firstly, we have a qualified import. Importing is how we can include other code to use in ours
-- Making it qualified means all the imports actually sit underneath a special name (we’re calling it L here),

-- small program that tells a Zoo owner what advice to take for each animal in the Zoo if it escapes.

-- awesome feature of Haskell: the ability to make your own data types, and values of those types.

-- data keyword which creates a new data type, and specifies all the values it can possibly be.
-- sum type, because the values of the type “summed together” make up the whole type.

-- 13.1. Sum Types
-- let’s see a sum type.

data Animal = Giraffe
            | Elephant
            | Tiger
            | Flea

-- we want Haskell to make a new type, named Animal. 
-- We’re also saying that the data for the Animal type can be any one of Giraffe, Elephant, Tiger or Flea, but nothing else. 

-- These values are also called value constructors
-- ****************   value constructors *************

type Zoo = [Animal]
-- Pretty simple. A Zoo is an Animal list. 

localZoo :: Zoo
localZoo = [ Elephant
           , Tiger
           , Tiger
           , Giraffe
           , Elephant
           ]
-- Ok, the Zoo named localZoo has some Animal values in it.

-- 13.2. Pattern Matching with Sum Types 
-- Below, we have a function that takes a single Animal, and returns a piece of advice as a String, for when that Animal escapes. 

adviceOnEscape :: Animal -> String
adviceOnEscape animal =
  case animal of
    Giraffe   -> "Look up"
    Elephant  -> "Ear to the ground"
    Tiger     -> "Check the morgues"
    Flea      -> "Don't worry"

-- ******************   CASE & is already TOTAL   ******************

-- Do you notice that there’s no default case, usually marked with an underscore? 
-- That’s because we know this function is total already, because it has one item for each of the possible data values of the Animal type, 
--     so there’s nothing left to catch for a default case.


-- 13.3. More Recursion

adviceOnZooEscapeR :: Zoo -> [String]
adviceOnZooEscapeR []      = []
adviceOnZooEscapeR (x:xs)  =
  adviceOnEscape x : adviceOnZooEscapeR xs
-- It’s a little bit different, though, because we can’t just use (:) as the folding function, 
--         as we’re also applying adviceOnEscape to each item as we fold them together into the new list.
-- In fact, in this case while we could think of it as folding the list into another list, we’re not really folding the list down to a single value, 
--      we’re just applying a function across all the elements of the list.


adviceOnZooEscape' :: Zoo -> [String]
adviceOnZooEscape' xs =
    foldr addAdviceForAnimal [] xs
  where addAdviceForAnimal animal adviceList =
            adviceOnEscape animal : adviceList
-- When we look at them together, we can see that we’re worse off that before! foldr was supposed to help us write less code, 
--    but it actually has us producing more! 
--  This should be telling us something: that foldr is not the right abstraction to use here.
-- We included a function called addAdviceForAnimal, which we used as our folding function.
-- *********************  local scoping ************************



-- So, it turns out that there is actually a function whose job it is to do what we want here: 
--    take a list of a and turn it into a list of b, using a function of type (a -> b) (which we could call the mapping function).
-- *********************  map ************************
-- map :: (a -> b) -> [a] -> [b]
adviceOnZooEscapeArg :: Zoo -> [String]
adviceOnZooEscapeArg xs = map adviceOnEscape xs

-- ******************** Higher Order Function
-- The map function is called a higher order function because it takes a function as an argument, 
--    so it’s an order higher than normal functions that just take values.

-- Also, we can simplify this by not mentioning the argument to the function. It still has one, but it’s implied by the types of the function and map.
adviceOnZooEscape :: Zoo -> [String]
adviceOnZooEscape = map adviceOnEscape
-- You give a 2-argument function only 1 argument, and this turns it into a 1-argument function! Haskell rocks!

-- 13.4. What is Currying?
-- You may remember that a function plus :: Int -> Int -> Int can be defined as plus x y = x + y
-- or it can be defined as plus = \x -> (\y -> x + y)
-- **************** because a 2-argument function is actually a function that returns a function of one argument.  ******
-- This way of defining multiple argument functions is called currying. 
-- ************************************* CURRYING ***********************************************

joinedWithCommasBetween :: [String] -> String
joinedWithCommasBetween []     = ""
joinedWithCommasBetween [x]    = x
joinedWithCommasBetween (x:xs) =
  x ++ ", " ++ joinedWithCommasBetween xs
-- second thing to note is that we’ve included a joinedWithCommasBetween function. We’re not actually using it here.
-- but it’s identical to the function obtained by providing the intercalate function from Data.List with a ", " value, 
--      except that it also works on any Lists, not just [String]
-- type of intercalate is [a] -> [[a]] -> [a]. Because the String type is actually just a synonym for [Char], this fits if “a” is Char. 
--	(It fits as [Char] -> [[Char]] -> [Char] which is the same as String -> [String] -> String).



main = do 
  putStrLn stringToPrint
  where
    stringToPrint = L.intercalate ", " advices
    advices = adviceOnZooEscape localZoo











