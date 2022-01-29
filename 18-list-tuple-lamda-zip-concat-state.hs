-- 18. Times-Table Train of Terror
-- introduce you to a tiny toy educational game that introduces quite a few new functions and features of Haskell.
-- We won’t be using any special algebraic data types in this program.
-- 

-- -- a game Level is simply a pair
-- of Integer values
type Level = (Integer, Integer)

-- 18.1. Tuples or Pairs
-- A Level is simply a pair that has two Integer values.A Level is simply a pair that has two Integer values.

-- As this is a simple math game, each level will be a pair of numbers that represent the two numbers for a multiplication question.

-- example, a level value of (7,9) would represent a question something like “What is 7 times 9?”).

levels :: [Level]
levels =
    concat $ map pairsForNum [3,5..12]
  where
    pairsForNum num = zip [2..12] $ repeat num

l = length levels

x = [1,3..100]
y = [2,4..100]
z = zip [1,2,3,7,8,9,10] [4,5,6]
z1 = zip [3,5..12] [2..12]
q = concat [[1],[2],[3]]
a = [[1],[2],[3]]
u = concat a
-- c1 = concat z1

r1 = zip [2..12] $ repeat 3
no_r = zip [2..12] [3]


--   r2 = zip repeat 3 $ [2..12]   repeat first does not work

mult3 :: [Int]
mult3 = map (3*) [2..12]

levels1 :: [[Level]]
levels1 =
    map pairsForNum [3,5..12]
  where
    pairsForNum num = zip [2..12] $ repeat num


-- num = [3,5..12]
-- pairsForNum num = zip [2..12] $ repeat num

--18.2. Ranges and the zip function
-- Let’s pull it apart piece by piece.
-- The levels value is a list of Level values.

-- ******************************************** ($) function   *******************************************************
-- It’s defined using the ($) function, which takes a function on the left, and applies it to the value or expression on the right.

-- In our case here, the value expression on the right is map pairsForNum [3,5..12].
-- The main new thing in this expression for us is [3,5..12], which is what’s called a range.

-- ******************************************** Range  *******************************************************
-- The range of numbers between 1 and 100, for example, would be represented as [1..100].
-- The range above, though, is every number between 3 and 12, in odd numbers. (So, 3,5,7,9,11).

-- So what we’re doing, then, is mapping the pairsForNum function across this range.
-- This function takes a number, calling it num, and “zips” the range [2..12] together with repeat num.
-- Zipping is creating pairs from one item each of a number of lists as we’ll see. 
-- ******************************************** zip  *******************************************************
-- The repeat function gives an infinite list of whatever its argument is.
-- ******************************************** repeat function  *******************************************************

-- The zip function takes two lists, and builds pairs out of those lists, taking one item from each as it does.
-- So, zip [1,2,3] [4,5,6] will create [(1,4),(2,5),(3,6)].
-- However, the zip function will stop when the first list runs out of values, 
--     so zipping an infinite list of repeated items with another that has only a few is just fine, as we’re doing.

-- So what we’ll end up with by using map pairsForNum [3,5,..12] is a list of lists of pairs of Integer values.
-- This basically means we’re combining each of the elements with each of the others. 
-- Then, we use concat to concatenate all the lists into one list. For example, concat [[1],[2],[3]] results in [1,2,3].
-- ******************************************** concat function  *******************************************************
-- In math, this process of creating a big set out of two smaller sets is called a cartesian product.
-- ******************************************** cartesian product  *******************************************************
-- The end result of this expression is that we have a list of 55 levels for our train of terror!

main :: IO ()
main = do
    putStrLn (" **** x = " ++ show(x))
    putStrLn (" **** y = " ++ show(y))
    putStrLn (" **** z = " ++ show(z))
    putStrLn (" **** q = " ++ show(q))
    putStrLn (" **** z1 = " ++ show(z1))
    putStrLn (" **** u = " ++ show(u))
    putStrLn (" r1 = " ++ show (r1))
    putStrLn (" Levels = " ++ show (levels))
    putStrLn (" no_repeat = " ++ show (no_r))
    putStrLn (" Mult3 = " ++ show(mult3))
    putStrLn (" length Levels = " ++ show (l))