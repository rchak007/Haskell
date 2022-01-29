
-- smallest list
aList = []
aList1 = [1]


-- The (:) Operator
aList2 = "sauce" : []
-- the (:) operator is just a function that makes new bigger lists out of old ones
-- It’s an infix function and like all operators, it takes two arguments: in this case, an element ("sauce") and a list ([]).
--remember, infix means it sits in between its arguments? 

-- Because (:) is an operator, we can also apply it as if it were a prefix function, by using parentheses:
-- a list with just pie on it
aList3 = (:) "pie" []
-- If we use parentheses around an operator, it works like a regular prefix function of two arguments.


-- List Syntax
--yet another way to make a list, which is the most usual way that you’ll see in Haskell programs:
-- a list with just napkin on it
aList4 = ["napkin"]

-- ***********************************   The List Types   ************************************
aList4' :: [String]
aList4' = ["napkin"]

-- another way to write this type (it’s a bit of a strange way to write it, though)
aList4'' :: [] String
aList4'' = ["napkin"]
-- We can put the type name (the brackets) on the left, or wrapped around the second type.

-- mixing diff type def with actual assignment
aList4Test :: [] String
aList4Test = "chuck" : []



-- list type a kind of container type: it has two pieces 
--    the container type (List), 
--    and its element type in the case of List (in the above, it’s String). 
-- That is, the list type is a parameterised type.

-- There are many container types in Haskell, and almost none of them besides this one have this “wrap-around-another-type” syntax. 
--    This can sometimes confuse people when they get to learning the other container types, which is why we’re introducing the normal syntax now.


-- Lists of Other Types
aList5 :: [Integer]
aList5 = [879]


-- you’ve probably noticed how using the list type is sort of similar to function application, but for types rather than values.
-- A function is a thing that "wraps" another value to turn it into another value.
-- Well, these “container types” like list, they actually take a variable, too! 
--          This is obviously not like the value variables that functions take, though, it's a type variable!
-- In the case of list, the parameter is the type of its elements. 

-- By itself, List doesn’t actually mean anything other than the potential for a list-like type of some variety.
-- That is, by itself, in the same way that functions don’t result in a concrete value until you apply them to another value, 
--            types with type variables are not a concrete type until you put a type with them.

aList6 :: [Integer]
aList6 = 879 : []

-- Lists with More Items
-- but we want to write a whole shopping list, not just a list with one item on it! We’re going to need more items.
-- list with two items:
aList7 :: Num a => [a]
aList7 = [1,3]
-- First, to have more items you put commas between items in this syntax. 
-- Second, we notice that we haven’t told Haskell a concrete type for our list. It’s a Num-constrained value called “a”!
aList7' :: Num a => [a]
aList7' = [1,3,4,5]       --- so works for more than 2 elements too



-- ********************************     Polymorphic Values and Types  ************************************
-- we’re letting Haskell work out what type the numbers are. 
-- => is for a type constraint, an arrow (->) is a type constructor for a function.
-- These kinds of values with typeclass-constrained types, like the type Num a => a, are called polymorphic values.
-- The empty list ([]) is actually a polymorphic value, too.
-- That’s how we’re able to write "sauce" : [] or 1 : [] and have Haskell still match the types properly:
-- the type of the empty list.
-- "t" could be any variable name
-- [] :: [t]

-- Interestingly, the List type is a polymorphic type.



-- ********************************* The (:) Operator Again, Binding & Associativity **********************
-- another definition for this same list, constructed using the (:) operator:
aList8 :: Num a => [a]
aList8 = 1 : 3 : []

-- aList8' :: []        -- error - cant do like this
aList8' = 1 : 3 : []    -- on its own it will work

-- The (:) operator is binding to the right; it has what’s called right-associativity. 
-- Binding can be described as when you apply a function to a value or variable, you’re binding the value as the function’s argument.
-- Associativity is just a fancy name meaning “the order functions evaluate their arguments in when there are no parentheses around”.
-- The (:) operator usually functions as though it were written like this:
-- we don't need these parentheses
aList8'' :: Num a => [a]
aList8'' = 1 : (3 : [])

-- Most functions we’ve seen so far bind to the left (they’re left-associative). So this one is right-associative
-- So Haskell looks at the 1, then looks at the first (:) and 
--    says “hey, let’s wait until we’ve seen what’s more to the right of this (:) before we do any binding or application here”.
-- here’s the type of the (:) operator:
-- (:) :: a -> [a] -> [a]


-- The Shopping List 
shoppingList :: [String]
shoppingList =
  [ "Carrots"
  , "Oats"
  , "Butter"
  , "Apples"
  , "Milk"
  , "Cereal"
  , "Chocolate"
  , "Bananas"
  , "Broccoli"
  ]

-- print, which uses the built in Show instance for List and prints a list on the screen in the usual program notation above.
-- print :: Show a => a -> IO ().

-- Counting The Items
-- function called length that will give us just that:
-- length :: Foldable t => t a -> Int
--   type constraint Foldable t =>, but that it’s using a typeclass that is new for us called Foldable. 
--         This is specified on the type variable “t”. 
--         Then there’s a type variable “a”, which is completely unconstrained, so it can be anything we like.

-- This “t a” might feel a little bit familiar, because it’s a generalised version of what we’ve just seen when we arrange our list’s type in that 
--    odd way we discussed
aList9 :: [] String
aList9 = ["Cat Food", "Lasagne"]
-- What about if we told you that list is an instance of the Foldable typeclass? 
     -- Well, it is. So, the type [] String could match Foldable t => t String.
-- So, thinking about the length function again, it takes a single value. 
--     The type of that value is “t a” where “t” is a wrappering type around “a”, and where t is constrained to types that are Foldable.
-- Foldable is a class of types that have a shape or structure that lets us reduce them to a single value in various ways. 
--    The length function reduces a Foldable structure down to a single Int value representing the item count of the “container of items”.


-- operator (++)
-- Well, (++) joins, or concatenates, two lists together. 
-- String type is itself just a list of type Char (the type of all written characters)

-- String is identical to [Char]. 
-- type of (++) is [a] -> [a] -> [a]

-- let’s look at another way to represent shoppingList
shoppingList2 :: [String]
shoppingList2 =
  "Carrots" :
  "Oats" :
  "Butter" :
  "Apples" :
  "Milk" :
  "Cereal" :
  "Chocolate" :
  "Bananas" :
  "Broccoli" : []

-- **************************************************** (:) Value Constructor ********************
-- Pattern-Matching with the (:) Value Constructor
-- The (:) operator is very handy. Because it’s a value constructor, 
--       we can also use it in pattern-matching to match parts of lists in arguments to functions

-- function that will get the first item from any list of strings (which includes our shoppingList obviously, because it’s just a list of strings).
firstOrEmpty :: [String] -> String
firstOrEmpty []     = ""             -- Covers totality
firstOrEmpty (x:_)  = x

-- **************************************************  TOTALITY 
-- Totality and More on Pattern-Matching with (:)
-- first definition: If we left it out and an empty list was passed, it would cause a runtime error when we ran our program. 
--      This would be bad. Including it makes the function total, which means it covers all possible values.
-- second definition is using the (:) value constructor operator as a pattern matcher. 
--    We know this value that we’re pattern matching on must be a list with at least one item on it, because to get to the second definition, 
--          the first “empty list match” must have been passed over (we can say that it failed matching).
-- These functions are called data constructors, or value constructors. 

-- Prefix Operator Pattern-Matching
-- You know that (:) is an operator, and operators can be changed from infix functions to prefix functions by using parentheses.
firstOrEmpty' :: [String] -> String
firstOrEmpty' []     = ""
firstOrEmpty' ((:) x _)  = x

-- ((:) x _) as a pattern means exactly the same thing as (x:_)

-- Let’s see another function. 
--    This takes the first two elements of a String list and joins them together with a comma if there are at least two items in the list

firstOnesOrEmpty :: [String] -> String
firstOnesOrEmpty []       = ""
firstOnesOrEmpty [x]      = x
firstOnesOrEmpty (x:y:_)  = x ++ ", " ++ y
-- we could rewrite the firstOnesOrEmpty [x] = x clause as firstOnesOrEmpty (x:[]) = x and it’d mean the exact same thing.

-- ************************************************* A Tiny Bit of Recursion

joinedWithCommas :: [String] -> String
joinedWithCommas []     = ""
joinedWithCommas [x]    = x
joinedWithCommas (x:xs) = x ++ ", " ++ joinedWithCommas xs

-- Notice the last definition actually refers to itself! This is called recursion

-- This function is actually already implemented more generally in a Module called Data.List as the function named intercalate.
-- It’s more general because you can put anything between the items, not just ", " as we have here.


-- The Final Shopping List Program







main :: IO ()

main = do 
 --putStrLn ("Smallest list = " ++ show (aList))  -- Errors i think since its empty
 putStrLn ("aList1 = " ++ show (aList1))
 print ("print - aList1 = " ++ show (aList1))
 putStrLn ("aList2 = " ++ show (aList2))
 putStrLn ("aList3 = " ++ show (aList3))
 putStrLn ("aList4 = " ++ show (aList4))
 putStrLn ("aList4' = " ++ show (aList4'))
 putStrLn ("aList4'' = " ++ show (aList4''))
 putStrLn ("aList4Test = " ++ show (aList4Test))
 putStrLn ("aList5 = " ++ show (aList5))
 putStrLn ("aList6 = " ++ show (aList6))
 putStrLn ("aList7 = " ++ show (aList7))
 putStrLn ("aList7' = " ++ show (aList7'))
 putStrLn ("aList8 = " ++ show (aList8))
 putStrLn ("aList8' = " ++ show (aList8'))
 putStrLn ("aList8'' = " ++ show (aList8''))
 print shoppingList
 putStrLn ("aList9 = " ++ show (aList9))
 putStrLn ("There are "
                ++ (show (length shoppingList))
                ++ " items on the shopping list.")
 putStrLn ("There are "
                ++ (show (length shoppingList))
                ++ " items on the shopping list."
                ++ " and the list is: "
                ++ joinedWithCommas shoppingList)
 






