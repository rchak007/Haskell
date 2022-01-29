import qualified Data.List as L

-- 17. The People Book
-- our own super-simple address book,
-- extremely useful variety of functions called higher order functions. 
-- bit deeper into sum and product types (or algebraic data types
-- and introduce records, another way to work with data within such data types.

-- 17.1. Models of Data
-- data model is a set of data types or data that describes something. 
-- a person can have many pieces of information about them (or we could call them fields or attributes), 
--    we need a way to build a single type out of a combination of other types.
-- o do this in Haskell, we use a product type, 
-- *******************   PRODUCT TYPE ********************************************

-- Here’s an example of one of these product types:

type Name = String
type Year = Int
data Person' = Person' Name Name Year
  deriving (Show)

--What is “deriving”? Why is data being used without the | symbol, and why is Name written twice?
-- Firstly, we can see that we have a type alias for Name as String. 
-- We also have one for Year as Int.


-- 17.2. More on Data Types

-- The data keyword tells Haskell we’re creating our own fresh new type. 

-- *******************   DATA TYPE DEFINITION ********************************************
-- data Person = Person Name Name Year   - breaking down the components:
-- data - is data type definition
-- Person - Type name
-- Person - Value constructor name  
-- Name Name Year -- Field types

-- part to the left of the = symbol is the type name
-- Once our type is created, this name can be used in places where types can go, for example in type annotations.
--      This name is Person in the example above. 
-- If we’d written data Muppet = HappyMuppet Name instead, then Muppet would be the type name instead.
--  ****************************  TYPE NAME ********************************************
-- Next we’ll look to the right of the = symbol. We see Person again. This is a value constructor. 
--  ****************************  VALUE CONSTRUCTOR / data constructor  (also called) ********************************************



--- **************************************************** sum and product types  *************************
-- If we’d created the more complicated data type of data 
-- SizedPerson = TallPerson Name | ShortPerson Name
-- then we would have two value constructors: 
--TallPerson :: String -> SizedPerson 
--ShortPerson :: String -> SizedPerson
-- ust to show you how it looks when there are sum and product types in the one algebraic data type.

-- Anyway, it’s called a product type because a single piece of this type of data is a product of more than one piece of data of these types. 
-- These pieces of data are often called fields of the data type. 

--  Person :: Name -> Name -> Year -> Person

-- 17.3. Making Our Types Show

-- deriving (Show) line after Person.
-- tell Haskell that we’d like it to create an easy to print version of this data type for us, 
--   automatically creating an instance of the Show typeclass for this data type. 

-- 17.4. Building Our First Value

-- the famous mathematician
-- Blaise Pascal
blaise' :: Person'
blaise' = Person' "Blaise" "Pascal" 1623

-- given name comes first, and the family name goes second


-- 17.5. Records
-- Don’t you wish there was some way we could see exactly what the types were supposed to mean inside of the product type itself?

-- It’s what’s called record syntax, which is simply a way to let us name the fields as we specify a data type.
--- **************************************************** record syntax  *************************

data Person = Person
    { personFirstName :: Name
    , personLastName :: Name
    , yearOfBirth :: Year }
  deriving (Show)

-- Also, Haskell automatically makes what is called a getter function for the fields,
-- and it also allows us to use something we’ll see called record update syntax for making new data based on existing data. 
-- ************************************** GETTER FUNCTON *************************


blaise :: Person
blaise =
  Person { personFirstName = "Blaise"
         , personLastName = "Pascal"
         , yearOfBirth = 1623 }
--Something new, though, is that we can also easily build new records out of others by doing the following:
traise :: Person
traise = blaise { personFirstName = "Traise" }
-- This is called record update syntax.
-- ************************************** RECORD UPDATE SYNTAX *************************
-- This one creates a person whose data looks like this:
-- Person {personFirstName = "Traise", personLastName = "Pascal", yearOfBirth = 1623}

-- Note that we can now “set” and “get” the data for fields in any order we like. We can set more than one field at once, too.
people :: [Person]
people =
  [ Person "Isaac" "Newton" 1643
  , Person "Leonard" "Euler" 1707
  , Person "Blaise" "Pascal" 1623
  , Person "Ada" "Lovelace" 1815
  , Person "Alan" "Turing" 1912
  , Person "Haskell" "Curry" 1900
  , Person "John" "von Neumann" 1903
  , Person "Lipot" "Fejer" 1880
  , Person "Grace" "Hopper" 1906
  , Person "Anita" "Borg" 1949
  , Person "Karen" "Sparck Jones" 1935
  , Person "Henriette" "Avram" 1919 ]

  -- 17.6. Finding a Person from the List
  -- the first person whose birthday is after 1900 in the list. 

-- imported the Data.List module, because we’ll be using the find function from that module.
firstAfter1900 :: Maybe Person
firstAfter1900 =
    L.find (\(Person _ _ year) -> year >= 1900) people

-- ************************************** FIND *************************

-- type signature L.find :: Foldable t => (a -> Bool) -> t a -> Maybe a.
-- This means it takes two arguments: firstly, a function from some value whose type is named a to a value of type Bool,
-- and secondly a value that is a Foldable of that same a type. 
-- A function from any type to Bool is often called a predicate because it’s making a statement of truth (the Bool value) 
-- which is based on, or predicated on, the input value.
-- Our Foldable instance will be on list, because we have people :: [Person] as our Foldable t => t a value.

-- Essentially what the function does is apply the predicate to each of the items until one returns True in which case 
--    it returns that particular item wrapped in Just,otherwise it returns Nothing. 
-- It’s not the most efficient way to find things because of the way lists are constructed, but it will be fine for our purposes here.
-- we’re using the Person constructor to pattern-match out the parts of the Person as it gets fed into the find function, 
--      with our predicate: (Person _ _ year) -> year >= 1900). 

-- we could also have written it like this, which is a bit more flexible because it doesn't depend on the field ordering in the data type:
firstAfter1900' :: Maybe Person
firstAfter1900' =
    L.find (\person -> yearOfBirth person >= 1900) people      --- field ordernig does not matter since we are not doing (Person _ _ year) 

-- 17.7. Filtering out People in a List

-- how we’d find the sub-list of people whose name begins with L, using recursion and the list above:

firstNameBeginsWithL :: Person -> Bool
firstNameBeginsWithL p =
  case personFirstName p of
    'L':_ -> True
    _     -> False

makeNewListWithOnlyLPeople :: [Person] -> [Person]
makeNewListWithOnlyLPeople [] = []
makeNewListWithOnlyLPeople (x:xs)
  | firstNameBeginsWithL x =
      x : makeNewListWithOnlyLPeople xs
  | otherwise =
      makeNewListWithOnlyLPeople xs

peopleThatBeginWithL' =
  makeNewListWithOnlyLPeople people

-- The firstNameBeginsWithL function takes a Person as the variable p, gets the first name with the personFirstName getter function,
-- then we have wrapped a case expression around that function application.

-- (:) is, as we know, a value constructor for lists that matches any list of one element or more, 
--         and using pattern-matching, splits it into its head and tail. 
-- ********************************************    (:) VALUE CONSTRUCTOR and PATTERN MATCHING ************************************

-- Next we’ll look at the makeNewListWithOnlyLPeople function
-- reasonably simple recursive function using guard patterns.
-- Remember that guard patterns work by matching the first expression that evaluates to True, 
--         then returning the expression to the right of that corresponding = symbol.
-- We pull the Person list into head and tail as x and xs respectively using pattern-matching again. 

-- You’ll notice that makeNewListWithOnlyLPeople is using the firstNameBeginsWithL function as a kind of testing function. 

-- What if we wanted more flexibility, to be able to swap out that function for any other predicate we liked,
-- to make a whole lot of different lists with people whose names started with letters other than L?



-- 17.8. A Note About List Efficiency

-- You might be thinking that this way of finding something is very inefficient. You’d be correct!
-- small amounts of data, the list type is handy, useful, and more than efficient enough.
-- However, for much larger amounts of data, we would want to use different functions and types to make our programs faster and more efficient.

-- Lists are very good for certain things, such as for representing data that will be added to at the front.
-- They are also a bit easier to write code for, and they show off a lot of the basics of Haskell, so they’re good for beginners to look at first.

-- 17.9. Higher Order Functions: filter 

-- What we just saw is a very common pattern in Haskell:
-- we take a testing function that returns a Bool value (otherwise known as a predicate) and 
--     a list of the same type of items that the predicate takes 
-- and we return a new list of items that resulted in True when “tested” against the predicate.
-- It’s so common that there’s already a built-in higher-order function for it in Haskell called filter:

-- ********************************************    FILTER ************************************
-- filter :: (a -> Bool) -> [a] -> [a]

-- A higher-order function is a function that takes one or more functions as argument(s). 
--    This term also applies to functions that return functions, but because of the way functions work in Haskell, 
--     it’s so easy and common to do that that we usually don’t count it as special in any way.

makeNewListWithOnlyLPeople' :: [Person] -> [Person]
makeNewListWithOnlyLPeople' xs =
  filter firstNameBeginsWithL xs

-- Here, xs is matched to the list of type Person, and we pass it to filter, along with our predicate (firstNameBeginsWithL). 
-- This version does exactly what our previous function does, but with a lot less code to read and write.

-- 17.10. Some Eta Reduction
-- further simplify the definition by removing the xs from both sides of the equals sign!
makeNewListWithOnlyLPeople'' :: [Person] -> [Person]
makeNewListWithOnlyLPeople'' =
  filter firstNameBeginsWithL

-- The technical name for this process of getting rid of these variables that are 
-- repeated on the right hand side of the inside and outside is called eta reduction
-- a name which comes from the basic building blocks that Haskell is built on.

-- ********************************************    ETA REDUCTION ************************************

-- Here’s another example of it:

plus num1 num2 = num1 + num2

-- the second argument "num2" gets "erased":
plus' num1 = (num1 +)
-- this is the same as:
plus'NonSectioned = \num1 -> (+) num1
plus'' = (+)

-- ********************************************    SECTION ************************************
-- Note that (num1 +) is technically actually what's called a section: that is, a partially applied operator.
-- However, because we're showing this eta reduction with the (+) operator,
-- plus' num1 = (num1 +) is effectively the same function as plus' num1 = (+) num1.

-- All three of these functions work in the same way. The (+) function already takes two arguments, 
-- which as we know in Haskell means it is actually two nested functions.
--  Let’s look at yet another way to do the same thing, this time with lambdas:

add = \x -> (\y -> x + y)

-- the second argument, "y" gets "erased":
add' = \x -> (x +)

-- this is the same as:
add'NonSectioned = \x -> (+) x

add'' = (+)

-- In each step, we’re simply removing one of the unnecessary variables from our function definition, 
-- because (+) is already a function itself, so by the end, all we’re doing is effectively saying that add'' is simply the (+) function.

--17.11. Using filter

-- let’s look at a different way to write the whole function:

-- don't get confused, c is not the letter c here
-- it's a variable name, holding the Char value
-- we're matching on
firstLetterIs :: Char -> String -> Bool
firstLetterIs c ""    = False
firstLetterIs c (x:_) = c == x

firstNameBeginsWith :: Char -> Person -> Bool
firstNameBeginsWith c p =
    firstLetterIs c firstName
  where firstName = personFirstName p

peopleThatBeginWithL :: [Person]
peopleThatBeginWithL =
  filter (firstNameBeginsWith 'L') people

-- We have firstLetterIs, a more general function that takes a Char and a String and 
--     returns True if the first letter of the String is the passed in Char value. 
-- The beauty of this function is if we decide we want to use a different letter, we just have to change the one spot in the code.

-- Then there’s the firstNameBeginsWith function that gets the first name of the passed in Person and
--    matches its first letter against a passed in Char value by using the firstLetterIs function.

-- Finally, we use filter along with the partially applied function firstNameBeginsWith 'L' and 
--    the people list to create a Person list value defined as peopleThatBeginWithL.


-- 17.12. Higher Order Functions: map

-- Well, what if we actually wanted to get a whole list of last names from a whole list of people? 
-- Well, given what we know about recursion, we’d probably write it something like this:

peopleToLastNames :: [Person] -> [String]
peopleToLastNames []     = []
peopleToLastNames (x:xs) =
  personLastName x : peopleToLastNames xs
-- we’re seeing is a function with two definitions, as usual. It’s looking like some standard recursion. 
-- The first definition simply says if the [Person] passed in is the empty list of Person, return the empty list of String.
-- The second definition is where most of the work takes place. This first pattern matches the head of the list into x and the tail into xs. 
--    So, x will be a Person, and xs will be a [Person].
-- It then returns applying personLastName to x which gives us a String, then prepends this using (:)
-- to the result of calling the whole function again on the tail of the list (recursively).


peopleToFirstNames :: [Person] -> [String]
peopleToFirstNames []     = []
peopleToFirstNames (x:xs) =
    personFirstName x : peopleToFirstNames xs

-- What if we made a general function and let the programmer using the function pass in their own function of type Person -> String, 
-- that way we could make this quite general:

mapPeople :: (Person -> String) -> [Person] -> [String]
mapPeople f []     = []
mapPeople f (x:xs) =
  f x : mapPeople f xs
-- Ok, notice we’ve added another function argument at the front — it’s f, which is the function of type Person -> String we’re passing in — 
--   and all our function definitions now have an added f parameter and variable.
-- Also notice we’re using it by appling it to x before using (:) with our recursive function application of mapPeople
--     at the end of the last line which also has to have the f argument, as it’s now a required argument to our function.
-- This is now quite general. We can use this with first names or last names, or any other function that takes a Person and gives back a String. 

peopleToLastNames' :: [Person] -> [String]
peopleToLastNames' people =
  mapPeople personLastName people

peopleToFirstNames' :: [Person] -> [String]
peopleToFirstNames' people =
  mapPeople personFirstName people

-- That’s starting to look very nice and compact. We now know, though, that we can eta reduce these functions by getting rid of the people argument,
peopleToLastNamesMpRed :: [Person] -> [String]
peopleToLastNamesMpRed = mapPeople personLastName

peopleToFirstNamesMpRed :: [Person] -> [String]
peopleToFirstNamesMpRed = mapPeople personFirstName

-- it’s time to let you in on a secret. The mapPeople function already exists in Haskell, as an even more general function called map.
-- type signature:
-- map :: (a -> b) -> [a] -> [b]
-- It takes two arguments: a function from anything a to anything b, a list of those a values, and returns a list of those b values. 

-- For us, this means we'd want these a and b values to be Person and String 
-- ************************ (we call this process of choosing specific types for our general functions specialisation) **********************

-- Pay careful attention here and note that where we write type variables like a and b in type signatures,
--    that doesn’t mean that those types have to be different, only that they can be different if you’d like.

-- if you want to map from, say, String to String; from the same type to the same type, then there’s nothing stopping you.

-- example, here’s a function that maps from a list of strings to their reversed string counterparts, using the reverse function:
-- reverseMap = map reverse :: [String] -> [String].

-- we could have just written our mapPeople function like this:
mapPeople' :: (Person -> String) -> [Person] -> [String]
mapPeople' = map

-- Or, we could just have used map instead of mapPeople.
-- So, given we have a list of people called people, we’d create a list of their last names like this:
lastNames :: [String]
lastNames = map personLastName people

-- ********************************************** higher order function map *****************************************************
-- So, the higher order function map takes a function and a list.
-- It gives a new list with the function applied to each element of the list. 
-- Let’s see how map could be implemented:
map' :: (a -> b) -> [a] -> [b]
map' f []     = []
map' f (x:xs) = f x : map' f xs

-- it’s very similar to the function for mapping a function over a list of people, just using type variables instead of specific (concrete) types.

-- You get a whole new list with new items that have been created by applying the function to the original elements. 
--     The original list and items are still there, unchanged.


-- This is important: Haskell values are almost never modified, they’re always created afresh, or referred to if they’re identical.
-- it’s looking at the original element and using the passed-in mapping function to make an entirely new element for the new list, 
--    leaving the original untouched. 


-- This is very good, because if two functions are referring to one value, 
-- the last thing you want is for that value to be changed by one of the functions without the other one realising it.

-- This is because Haskell has purity, which means functions cannot work on things other than their arguments, 
--    and also because it has immutability which is a fancy word meaning data can’t be changed! 
-- You might worry that this would make it inefficient, but that’s not the case. In many cases it actually makes it more efficient.

-- 17.13. Higher Order Functions: sortBy

-- *********************************** Ord typeclass ************************************************************
-- This is for types that can be ordered.
-- Ord provides the functions compare, (<), (<=), (>), (>=), min and max.

--These functions allow comparison between two values in various ways. You should investigate their types using hoogle. 
-- If you use a web search for “haskell hoogle” it will find it. It’s a very handy tool for looking at functions.
-- https://hoogle.haskell.org
-- *********************************** hoogle **********************************************************************
-- Hoogle is a Haskell API search engine, which allows you to search the Haskell libraries on Stackage by either function name, 
--      or by approximate type signature.

-- There is also a sum type called Ordering that this typeclass uses that has the values LT, GT, and EQ,

-- The compare function returns this type, and sorting functions use the compare function and the Ordering type to do their work.

sortedLastNames :: [String]
sortedLastNames = L.sort lastNames

-- This will be sorted alphabetically.
-- What if we wanted it in reverse? Well, there is a reverse function in Haskell whose type is [a] -> [a] that will reverse any list at all.

-- Firstly, sortBy has a type of (a -> a -> Ordering) -> [a] -> [a] and compare has type Ord a => a -> a -> Ordering,
--   so you’ll notice that the sort function is effectively the same thing as sortBy compare. 
--  To reverse the order, we can simply provide a function to sortBy that returns the opposite Ordering that compare would return,
-- and we can do that by swapping the arguments to compare:
-- **************************************** compare to reverse sort *****************************************************
reverseSortedLastNames :: [String]
reverseSortedLastNames =
  L.sortBy (\x y -> compare y x) lastNames
-- This definition is more efficient than doing reverse (sortBy compare lastNames), because it only has to go through the list once.
-- For our small data set, this is not going to be a problem. It would matter with a very large list, though. 


-- There’s an arguably better way to do this than use a lambda, though. 
-- Haskell has a commonly-used function called flip that works with any function of two or more arguments.
-- Pass it any 2-argument function, and it’ll return you a function that works exactly the same, but has its arguments swapped.
reverseSortedLastNames' :: [String]
reverseSortedLastNames' =
    L.sortBy reverseCompare lastNames
  where reverseCompare = flip compare

-- **************************************** Flip *****************************************************

-- get the list of firstNames should appear as no surprise.
firstNames :: [String]
firstNames =
  map personFirstName people
-- but what if we wanted to sort the people list itself by some field of each person?

-- **************************************** sortOn function *****************************************************

-- we want to create a list of people sorted by their first names.
-- The function personFirstName fits the a -> b type perfectly for our purposes, as its type is Person -> String and 
--     we want to sort on the first name String.
sortedPeopleByFirstName :: [Person]
sortedPeopleByFirstName =
  L.sortOn personFirstName people

-- Now let’s see a function that takes a year, and a person, and works out how many years ago from that year that person was born.
yearsSinceBirthAtYear :: Year -> Person -> Int
yearsSinceBirthAtYear y p = y - yearOfBirth p

-- my own tests
myPeopleSinceYear :: Year -> [Person] -> [Int]
myPeopleSinceYear y [] = []
myPeopleSinceYear y [p] = [yearsSinceBirthAtYear y p]
myPeopleSinceYear y (x:xs) = yearsSinceBirthAtYear y x : myPeopleSinceYear y xs

myBirthYear :: [Person] -> [Int]
myBirthYear [] = []
myBirthYear [p] = [yearOfBirth p]
myBirthYear (x:xs) = yearOfBirth x : myBirthYear xs

myMappedBirthYear :: [Int]      -- Doing above myBirthYear using MAP
myMappedBirthYear = 
    map (yearOfBirth) people


-- We map y to the comparison year, and p to the passed in person, then apply the yearOfBirth function to the person
-- and subtract that from the comparison year. 
-- If we wanted to get this across all the people, we could map it as a part-applied function, say for 2012:
allYearsSinceBirthAt2012 :: [Int]
allYearsSinceBirthAt2012 =
  map (yearsSinceBirthAtYear 2012) people

-- And now a function that shows the earliest year of birth for the people on our list. 
--    This uses the minimum function which will work on lists containing instances of Ord. 
-- Actually it’s a very general function, because it will work not only on list, but any instance of Foldable. 
-- There’s also a maximum function that gets the highest ordered value, too. Let’s see their type signatures before we proceed:
-- minimum :: (Ord a, Foldable t) => t a -> a
-- maximum :: (Ord a, Foldable t) => t a -> a
-- ********************************** type signature MIN/MAX ****************************************************************
-- These functions have two typeclass constraints on them.
-- This says that t must be an instance of the Foldable typeclass, but also that a must be an instance of the Ord typeclass.
-- Note that you cannot pass an empty list into these functions. You must only pass a list that has at least one item in them.

earliestYearOfBirth :: [Person] -> Year
earliestYearOfBirth people =
  minimum (L.map yearOfBirth people)

-- 17.14. Removing Parentheses With The ($) Function
-- In Haskell, we prefer not to use so many parentheses.
-- here is a higher order function called ($) that will take a function on the left of it, 
--     and some expression on the right, and apply the function to the expression.
-- ********************************** ($) Function ****************************************************************
-- It has an extremely low precedence, which means it will pretty much be applied last of all. 
-- basically the same effect as having parentheses wrapped around the expression on the right.
earliestYearOfBirth' :: [Person] -> Year
earliestYearOfBirth' people =
  minimum $ L.map yearOfBirth people

-- Note also that the ($) function is only for the cases where the parentheses would go right to the very end of the expression on the right.

-- 17.15. Using minimumBy 
-- Last of all, we want to find out which Person was born first out of our list of people.

bornFirst :: [Person] -> Person
bornFirst people =
    L.minimumBy compareBirthYears people
  where compareBirthYears x y =
          compare (yearOfBirth x) (yearOfBirth y)

-- From GHCI:
--*Main> :t L.minimumBy
-- L.minimumBy :: Foldable t => (a -> a -> Ordering) -> t a -> a
-- ms another higher-order function: that is, it takes a function which is a comparing function (a -> a -> Ordering). 

-- ********************************** minimumBy Function ****************************************************************
-- ********************************** Higher-Order Function ****************************************************************
--  takes a Foldable instance wrapping some “a” values of any type,
-- and gives us the minimum one by using the comparing function on successive pairs of items.
-- Notice that the “a” type doesn’t have to be an instance of Ord here. 
-- So long as the comparing function returns an Ordering and its two arguments are the same type,
-- *********************************** where clause to locally scope ***********************************************
-- compare has the type Ord a => a -> a -> Ordering
-- means compareBirthYears x y returns an Ordering, which means it’s the correct type that minimumBy requires.


-- higher-order functions such as map, filter, fold and now minimumBy and maximumBy might seem complicated at first, 
--  but with lots of practice in thinking through all the types of the functions concerned, 
--        they will become second nature to read, and then later, to write.





main :: IO ()
main = do
    putStrLn (" **** ")
    putStrLn (" Sort last names = " ++ show (sortedPeopleByFirstName))
    putStrLn (" Show all people " ++ show people)
    putStrLn ("people Recursioon birthYears = " ++ show (myBirthYear people))
    putStrLn (" Mapped Years of Birth " ++ show (myMappedBirthYear))
    putStrLn (" myPeopleSinceYear 2021 is " ++ show (myPeopleSinceYear 2021 people))
    putStrLn (" all years since 2012 is " ++ show allYearsSinceBirthAt2012)
    putStrLn (" Min - oldest person = " ++ show (earliestYearOfBirth people))
    putStrLn (" Min - oldest person using minimum = " ++ show (earliestYearOfBirth people))
