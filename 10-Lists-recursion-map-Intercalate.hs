

import qualified Data.List as L
-- Data.List package so we can use the intercalate function 
-- (which takes a joining list of a type, and a list of Lists the same type and builds a fresh list by joining each element of the list 
--        together with the joining item between).

-- we're only interested in movies whose first letter is in the first half of the alphabet

movies =
  [ "Aeon Flux"
  , "The Black Cat"
  , "Superman"
  , "Stick It"
  , "The Matrix Revolutions"
  , "The Raven"
  , "Inception"
  , "Looper"
  , "Hoodwinked"
  , "Tell-Tale"
  ]


  -- 10.1. Predicates, String, Char
  -- need is a function that decides if a movie is good:

isGood :: String -> Bool
isGood (x:_) = x <= 'M'
isGood _     = False

-- **********************   predicate function
-- This kind of function, one that returns a Bool value that is depending on another value, is called a predicate function
-- simply means something can be either affirmed or denied about something else.
-- uses the (<=) :: Ord a => a -> a -> Bool operator to check if it’s earlier in the alphabet than 'M'.

-- some new things here, obviously. 
--    First is that we’ve got single quotation marks around M. What is the type of this thing? Well it’s 'M' :: Char. 
--      In Haskell, single quotation marks are used to indicate that something is a Char value.
-- String is simply [Char]; "Hello" is the exact same thing as ['H', 'e', 'l', 'l', 'o'].

-- 10.2. The Ord typeclass
-- (<=) function - takes two values of type constrained by the Ord typeclass
-- This typeclass is for types whose values can be compared in some ordered way (hence the name).
-- Ord typeclass provides a type with the compare, (<), (<=), (>), (>=), max, and min functions.

-- 10.3. Transforming Titles

assess :: String -> String
assess movie = movie ++ " - " ++ assessment
  where assessment = if isGood movie
                     then "Good"
                     else "Bad"
-- ***********************   where clause.
-- This isn’t an expression, so we can’t embed where clauses anywhere we like, 
--       but rather they let us write definitions that are only “in effect” within the level above where they appear. 
--       This is called local scoping. 
-- This is why in the assess function above, we can use the definition for assessment within the function’s body expression.
-- We can have multiple definitions and even functions written in where clauses. 


assess' :: String -> String
assess' movie = movie ++ " - " ++ assessment
  where assessment = if movieIsGood
                     then "Good"
                     else "Bad"
        movieIsGood = isGood movie

-- Here we’ve just pulled isGood movie out into its own locally scoped definition. 
-- We don’t recommend it, but if you wanted to get crazy, you could even put where clauses within definitions within other where clause definitions.

-- 10.4. Building Up a Function to Rename the Movie List 
-- Next we're going to see a partial function so we can explain how it works to you in stages. 
-- Here, by partial we mean it's not total, because it’s missing some cases of values of its arguments. 
-- That is, the function doesn't cover every possibility. This is not a good thing to do, ordinarily. 

-- So, this function we want to write should take a list of movies and give us back the newly named movie list...
assessMoviesTmp :: [String] -> [String]
assessMoviesTmp []  = []
assessMoviesTmp [y] = [assess y]

-- ... but it doesn't, because it only works properly on lists of length 0 or 1
-- The [y] pattern only matches against lists of one item
-- Next, we’ll see that we add a pattern for lists of two items and change the whole thing to use the (:) operator as a pattern-matcher, 
--      because we know [x] and (x:[]) are the same thing, but also to use the (:) operator to make the new list rather than how we did above:

assessMoviesTmp2 :: [String] -> [String]
assessMoviesTmp2 []       = []
assessMoviesTmp2 (y:[])   = assess y : []
assessMoviesTmp2 (x:y:[]) = assess x : assess y : []
-- This is a little better, but what about lists with more than two items?

-- ************************* RECURSION
-- This process of using the function we're defining within that function definition itself is called recursion, and 
--     it's one of the foundational underpinnings of Haskell.

-- So these two duplicated patterns can be resolved with a single definition, as long as we keep our empty list as a base case, 
--     otherwise we'd end up in an endless cycle.


assessMoviesTmp3 :: [String] -> [String]
assessMoviesTmp3 []     = []
assessMoviesTmp3 (x:xs) = assess x : assessMoviesTmp3 xs

-- ************************* MAP
-- This pattern - applying a function (the function assess here) to each element of a list 
--   — is so common that there’s actually a function called map that extracts this mapping functionality.
-- Its type is map :: (a -> b) -> [a] -> [b]. 

-- The assess function’s type is String -> String. This fits into the pattern of the first argument of map: a -> b. 
-- In Haskell a -> b means the types can be different, but don’t have to.
-- So, assess :: String -> String fits the first argument to map.
-- If we supply the function map with the function assess as its first argument, 
--    that means we’re saying the type a in map’s type signature must be String, and also that the type b must be String, too.
-- That means the second argument and the return type must both be [String].




assessMovies :: [String] -> [String]
assessMovies = map assess

assessedMovies :: [String]
assessedMovies = assessMovies movies


main :: IO ()
main = do 
  putStrLn ("********* With Map ********")
  putStrLn (L.intercalate "\n" assessedMovies)
  putStrLn ("****** Without Map ***********")
  putStrLn (show (assessMoviesTmp3 movies))
  putStrLn ("**** just movies ******")
  putStrLn("movies = " ++ show (movies))
  putStrLn ("***** without map with new line *******")
  putStrLn (L.intercalate "\n" (assessMoviesTmp3 movies))   -- achieving the intercalate on list w/o map

















