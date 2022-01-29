-- 11.1. Tuples
-- A Tuple lets you keep some number of items of potentially different types together as one item.
-- We can have 2-tuples, 3-tuples, and so on.

aShoppingListItemTmp1 :: (String, Int)
aShoppingListItemTmp1 = ("Bananas", 300)
-- This is a single shopping list item: a 2-tuple value. 
-- best to just stick to two, or maybe three at the very most. 
--   There are better ways to build up composed data types that we’ll see later on if you need to do that.

-- In the same way as we know that [String] is a type that can be expressed as [] String, we can express the 2-tuple (String, Int) as (,) String Int. 
-- In the same way, the 3-tuple (Int, String, Int) could be expressed as (,,) Int String Int, and so on.

-- So, tuple types are created with the (,) style type constructors, there is actually an identically named value constructor for tuples.
-- So you could just as easily write your tuple values as ("Bananas", 300), or as (,) "Bananas" 300.

-- 11.2. Type Aliases (or Type Synonyms)
-- new name for our (String, Int) tuple type 
type ShoppingListItem2 = (String, Int)

aShoppingListItem2 :: ShoppingListItem
aShoppingListItem2 = ("Bananas", 300)

-- We use “"type"” to tell Haskell we’re defining a type alias (or type synonym).
-- This is just to make our programs more readable for us.
-- in Haskell, all types must start with a capital letter, and all variable names must start with a lowercase letter.


type Name = String
type PriceInCents = Int
type ShoppingListItem = (Name, PriceInCents)
type ShoppingList = [ShoppingListItem]

shoppingList :: ShoppingList
shoppingList = [ ("Bananas", 300)
               , ("Chocolate", 250)
               , ("Milk", 300)
               , ("Apples", 450)
               ]
-- same thing as [(Name,PriceInCents)], which is a list of tuples, and is the same thing as [(String,Int)].

-- 11.3. The Final Program (rest)
sumShoppingList :: ShoppingList -> PriceInCents
sumShoppingList []     = 0
sumShoppingList (x:xs) = getPriceFromItem x +
                         sumShoppingList xs

getPriceFromItem :: ShoppingListItem -> PriceInCents
getPriceFromItem (_, price) = price

-- 11.4. More Recursion Explained
-- The second new function is sumShoppingList. This is using recursion to go over each item in the list, and
--     apply the getPriceFromItem function to them, adding the prices together as it does so.

-- 11.5. Folding
-- Going from the expanded recursion form to the single number is an example of what’s called folding, 
--     and it involves reducing a list to a single value.

-- more recursive Functions
joinStrings :: [String] -> String
joinStrings []     = ""
joinStrings (x:xs) = x ++ joinStrings xs

sumIntegers :: [Integer] -> Integer
sumIntegers []     = 0
sumIntegers (x:xs) = x + sumIntegers xs

-- subtracts all subsequent numbers from
-- the first number
subtractNums :: Num a => [a] -> a
subtractNums []     = 0
subtractNums (x:xs) = x - subtractNums xs

productOfIntegers :: [Integer] -> Integer
productOfIntegers []     = 1
productOfIntegers (x:xs) = x * productOfIntegers xs


-- If you look across all of those functions, and try to see what’s similar about them, you may notice some interesting things:

-- *********************************   BASE VALUE   /   BASE CASE 
-- Firstly, there is a single value for the empty list case. This is called the base value. 
--    The case is called the base case, because it’s where the recursion ends.

-- Secondly, there is an operation being applied between each element of the list. For joinStrings, it’s (++). For productOfIntegers, 
--    it’s (*). This is called the folding function, because it’s what Haskell uses to do the folding after the recursion has been fully expanded.

-- *********************************  Recursvice Functions fold to the right - RIGHT FOLDS 
-- Thirdly, and a little more subtle, is that all of these functions fold to the right, 
--    which means the recursive function application happens at the right. This is why they’re called right folds.
--    If we had our recursion on the left, it works differently.

-- 11.6. Using foldr  **************  foldr   ****************
-- If you remember, functions are values as well, which means we can pass a function into another function as a value. 
-- We can also return a function as a result.
-- In fact, all functions of more than one argument do this.
-- Let’s look at all the examples of the above, rewritten using the generalised fold right function
joinStrings2 :: [String] -> String
joinStrings2 xs = foldr (++) "" xs

sumIntegers2 :: [Integer] -> Integer
sumIntegers2 xs = foldr (+) 0 xs

subtractNums2 :: Num a => [a] -> a
subtractNums2 xs = foldr (-) 0 xs

productOfIntegers2 :: [Integer] -> Integer
productOfIntegers2 xs = foldr (*) 1 xs

-- So, it seems foldr takes three arguments! 
-- The first is a function of two arguments: the folding function. The second is the base value, and the third is the list we’re folding over.
-- Ex in joinStrings2 xs = foldr (++) "" xs
--     1) folder (++) is the function with 2 arg
--     2) "" is the base value
--     3) xs is the list we are folding over


-- type signature of foldr 
-- foldr :: Foldable t => (a -> b -> b) -> b -> t a -> b
--   here 1st function with 2 arg is (a -> b -> b) 
--        2nd base part is -> b
--        3rd list part is: t a
--        The last -> b is the final return value 


-- Just like length is generalised to work on any kind of Foldable, so is foldr. Actually, foldr and length are part of the Foldable typeclass.


-- First, the Foldable instance we’re dealing with is the one for List, so we could replace the “Foldable t => t” part with List like this:
-- foldr :: (a -> b -> b) -> b -> [] a -> b
-- but we can just write [] a as [a], so:
-- foldr :: (a -> b -> b) -> b -> [a] -> b

-- So we can rewrite the type for foldr as specialised inside joinStrings like this:
-- foldr :: (String -> String -> String)
--      -> String
--      -> [String]
--      -> String


-- Next, though, we’ll look at sumShoppingList again, and how to make it use foldr.

sumShoppingList' :: ShoppingList -> PriceInCents
sumShoppingList' xs = foldr getPriceAndAdd 0 xs

getPriceAndAdd :: ShoppingListItem ->
                  PriceInCents ->
                  PriceInCents
getPriceAndAdd item currentTotal =
  getPriceFromItem item + currentTotal





main :: IO ()
main = do 
  putStrLn ("Recursion - Price of shopping list is "
                ++ show (sumShoppingList shoppingList)
                ++ " cents.")
  putStrLn ("FOLDR Price of shopping list is "
                ++ show (sumShoppingList' shoppingList)
                ++ " cents.")













