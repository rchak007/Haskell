import qualified Data.List as L

-- 14.1. Another Sum Type
-- data type for cat breeds
data CatBreed =
    Siamese | Persian | Bengal | Sphynx
  | Burmese | Birman | RussianBlue
  | NorwegianForest | CornishRex | MaineCoon
-- *************************************** SUM type **********************************
-- this sum type means we’re declaring a new type of data called CatBreed, and all the possible values are listed above.

-- 14.2. Product Types and Algebraic Data Types
-- *************************************** Product type **********************************
-- data type for Cat. This is a new kind of data type called a product type. 
-- This lets us make values that combine more than one type.

--   *********** 
-- When we saw tuples, you may remember we said that there are better ways to combine multiple values into one value. Well, this is that better way. 
-- A Cat can have an Age, and a Name, and a breed (CatBreed).
type Name = String
type Age = Integer
data Cat = Cat Name CatBreed Age

-- catBengal = Bengal
-- ******************** Both sum types and product types are examples of ALGEBRAIC data types. 
-- You can have combinations of these types in the one type, which means you can have types that are sums of product types.

-- Cat is the name of the value constructor, as well as the name of the type. 
-- So how does Haskell know when we’re talking about the type, and when we’re talking about the value?
--This actually tells Haskell to make the value constructor function Cat :: Name -> CatBreed -> Age -> Cat
-- and this can be used to make Cat values (which is why it’s called a data constructor),

-- 14.3. Pattern-Matching Product Types
-- product type for House:
type HouseNumber = Int
data House = House HouseNumber Cat
-- function to work out how old a cat is in human years:
-- this is a commonly agreed upon
-- way to work out cat ages
humanAge :: Cat -> Age
humanAge (Cat _ _ catAge)
  | catAge <= 0 = 0
  | catAge == 1 = 15
  | catAge == 2 = 25
  | otherwise   = 25 + (catAge - 2) * 4
-- We’re using a guard pattern to match all the possibilities here.
-- Notice that the first argument to humanAge is a Cat, a single value of the Cat type, 
--    but it’s being kind of “pulled apart” by the pattern match using the value constructor. 
--        This takes the first two fields of the Cat data type and ignores them
--         (by matching them to “_” which as you might know by now, basically throws them away)
-- ********************  BINDS 
-- and then binds a variable name to the age of the Cat called catAge, so the rest of the function can compare and do math with it.

-- Next we’ll see some data for a street, which will be a list of houses, and a couple of functions for working with that data:

street :: [House]
street =
  [ House 1 (Cat "George" Siamese 10)
  , House 2 (Cat "Mr Bigglesworth" Persian 5)
  , House 3 (Cat "Mr Tinkles" Birman 1)
  , House 4 (Cat "Puddy" Burmese 3)
  , House 5 (Cat "Tiger" Bengal 7)
  , House 6 (Cat "The Ninja" RussianBlue 12)
  , House 7 (Cat "Mr Tinklestein"
                 NorwegianForest
                 8)
  , House 8 (Cat "Plain Cat" MaineCoon 9)
  , House 9 (Cat "Shnooby" Sphynx 7)
  , House 10 (Cat "Crazy Ears Sam"
                   CornishRex
                   3)
  ]

getCatFromHouse :: House -> Cat
getCatFromHouse (House _ c) = c

getHumanAgeOfCatFromHouse :: House -> Age
getHumanAgeOfCatFromHouse =
  humanAge . getCatFromHouse               -- we go from House to Cat w getCatFromHouse and then that Cat to Age with humanAge


-- So, street is a value whose type is [House].
-- we see there’s a simple function that extracts the Cat value from a House value by using pattern matching (called getCatFromHouse).

-- 14.4. **********************************      Function Composition
-- new operator in it named (.) which kind of glues two functions together

-- The type of getHumanAgeOfCatFromHouse is annotated to be House -> Age. 
--    As we just saw, getCatFromHouse takes a House and gives a Cat,
--    and humanAge takes a Cat and gives an Age. 
--  So, if we somehow could chain or pipe them together (glue them at the Cat, so to speak!),
--  then giving this new function a House value would give us back an Age. 
-- This is exactly what the (.) operator does to two functions:
-- it chains them together to form a new one that does the work of both, as long as they have a common type between them

-- Here’s another way to write 
getHumanAgeOfCatFromHouse' :: House -> Age
getHumanAgeOfCatFromHouse' h =
  humanAge (getCatFromHouse h)

-- Sometimes it makes more sense to use normal function application, like the above: humanAge (getCatFromHouse h) and 
--        other times it makes more sense to use function composition like this: humanAge . getCatFromHouse, 
--  but they mean the same thing. We’ll see more of (.) later.

-- 14.5. Importing a Module
-- we’ll see a function from the Data.List module called find that can be used to get a particular item from a List.
-- We’re “aliasing” it (or locally renaming it) to L by using the as keyword.
-- The qualified keyword makes sure when it imports all the function names, it doesn’t load them directly into our local namespace. 
-- That way, if we already have things named the same thing as the module we’re importing, there won’t be any conflicts.

-- 14.6. Maybe An Answer
-- ********************************************    FIND      *******************
-- type signature for the find function,
-- L.find :: Foldable t => (a -> Bool) -> t a -> Maybe a
-- We can see this function takes two arguments; firstly a function of type a -> Bool
-- and secondly a value of type Foldable t => t a. 
-- This function then returns another wrapper type. 
-- This one is called Maybe, and it is wrapping the same type that our Foldable t => t is wrappering.

-- What does this function do, though? 
-- Well, it takes that function of a -> Bool and applies it to each of the items in the Foldable t => t a.
-- If it can’t find any that return True, it returns the Nothing value, which comes from the Maybe a type.
-- If it does find any that return True, it returns the first one, wrapped in the Just value constructor from the Maybe a type.



names = ["Harry", "Larry", "Barry"]

result1 = L.find isHarry names
  where isHarry name = name == "Harry"
-- result1 will be:
-- Just "Harry"

result2 = L.find isJake names
  where isJake name = name == "Jake"
-- result2 will be:
-- Nothing

--*Main> :t result1
--result1 :: Maybe [Char]



-- 14.7. A Little Finding & Sorting

findOldestCat'' :: [House] -> Maybe Cat
findOldestCat'' []     = Nothing
findOldestCat'' houses = Just oldestCat
  where
    oldestCat
      = getCatFromHouse houseWithOldestCat
    houseWithOldestCat
      = head housesSortedByCatAge
    housesSortedByCatAge
      = L.sortBy catAgeComparer houses
    catAgeComparer (House _ (Cat _ _ age1))
                   (House _ (Cat _ _ age2))
      = compare age2 age1


-- Let’s go from the bottom up.
-- catAgeComparer is a function that takes two houses and compares the ages of the cats contained within. 
-- It does this by pattern matching the ages out of the cats, and the cats out of each house all at once (its type is House -> House -> Ordering).

--  ********************************* ORDERING    &    SORTBY
-- An Ordering is a built-in sum data type which has values of LT, EQ and GT which stand for less than, equal to and greater than respectively.
-- Ordering values are used by sorting functions in Haskell.
-- The sortBy :: (a -> a -> Ordering) -> [a] -> [a] function from Data.List takes a function whose type is (a -> a -> Ordering) 
--      to sort its second argument: a list.
-- That fits the type of catAgeComparer, which is why we’re using sortBy in our definition of housesSortedByCatAge in the line above that. 
-- Put another way, the sortBy function takes a comparing function (that is, one that returns an Ordering), and a list, and 
--    returns that list sorted by using the comparing function on adjacent elements.

-- Because we want to sort oldest to youngest, our application of the compare function in catAgeComparer has age2 first. 
--      If age1 was first, it’d sort youngest to oldest.


-- *********************************** HEAD function
-- Next, we have houseWithOldestCat which takes the housesSortedByCatAge value obtained by sorting the houses, 
--   and picks off the first one with the head function.
-- It’s only safe to use the head function when we can be absolutely sure there is at least one item in the list we’re applying it to.
--     Otherwise it will cause your program to crash (crashing is a term that means the program unexpectedly stopped working). 
-- We’re sure that there is at least one item in the list we’re applying head to because we have a clause 
--       that matches on the empty list and returns Nothing.
--Finally, oldestCat is obtained by applying getCatFromHouse to houseWithOldestCat, which will obviously just get the cat out of the house.

-- 14.8. More About Maybe
-- Now we can talk some more about the Maybe Cat type that you can see in the findOldestCat function’s type signature.
-- Maybe is a type that takes another type to make types with (this is called a type constructor). 
-- means you can have values of type Maybe Int, Maybe Cat, Maybe House, or Maybe any other concrete type you like.
-- It’s a sort of wrapper for other types, and it’s what we use in Haskell when we want to make a type’s value optional.
-- In our case, we can’t be 100% sure if the list we pass to findOldestCat will contain something. 
-- If it is empty, then we obviously can’t pass a Cat back at all. 

-- The way Maybe values work is there’s a value called Nothing which represents “no value” of the wrapped type, 
-- and there’s a value called “Just a” which represents any other value of our wrapped type.

Just 5 :: Num a => Maybe a
Just "Heya" :: Maybe String
Just (Cat "YOOBEY" Sphynx 8) :: Maybe Cat
Nothing :: Maybe Cat
Nothing :: Maybe Integer
Nothing :: Maybe a
--You might be surprised to know that Nothing :: Maybe Cat can’t be compared to Nothing :: Maybe Integer. 
-- This is because Cat is a different type than Integer and you can’t compare differently typed values 
-- (unless they’re polymorphic values — that is, values whose types have type variables like Nothing :: Maybe a, or 5 :: Num a => a).

-- So, Nothing :: Maybe Cat means “there are no cats”, and Just (Cat "YOOBEY" Sphynx 8) means “a value of the optional-Cat type that has a Cat in it). 
-- This is different than the values whose type is Cat, because a value of the type Cat must be a Cat as defined by the type - it can’t be empty at all.


-- This is a very nice property for a programming language to have. If there is a value whose type is Integer, 
-- you can be sure it won’t have anything other than exactly that in it, which makes reading and reasoning about programs much much easier.


-- However, every positive side has a negative side, too, 
--     and the negative side of this is that it makes working with empty things slightly more complicated 
--        than if there were just a general value that means an empty thing.
-- We definitely think the complexity is worth it, though, 
-- because these optional types are some of the biggest sources of errors in programming languages 
-- that don’t have this feature of “typed optionality”.


findOldestCat :: [House] -> Maybe Cat
findOldestCat []     = Nothing
findOldestCat houses = maybeOldestCat
  where
    maybeOldestCat
      = case findOldestCatHouse houses of
          Just house ->
            Just (getCatFromHouse house)
          Nothing ->
            Nothing


findOldestCatHouse :: [House] -> Maybe House
findOldestCatHouse houses =
    if length housesSortedByCatAge > 0
    then Just (head housesSortedByCatAge)
    else Nothing
  where housesSortedByCatAge
          = L.sortBy catAgeComparer houses
        catAgeComparer (House _ (Cat _ _ age1))
                       (House _ (Cat _ _ age2))
          = compare age2 age1

getCatName :: Cat -> String
getCatName (Cat name _ _) = name

getHouseNumber :: House -> String
getHouseNumber (House number _) = show number






main :: IO ()
main = putStrLn oldest
  where
    oldest =
      case findOldestCatHouse street of
        Nothing ->
          "There is no oldest cat!"
        Just house ->
          "The oldest cat is "
          ++ getCatName (getCatFromHouse house)
          ++ ", is "
          ++ show (getHumanAgeOfCatFromHouse house)
          ++ " equivalent human years old"
          ++ " and it lives in Number "
          ++ getHouseNumber house


















