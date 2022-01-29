import qualified Data.List as L

type Name = String
type Year = Int
data Person = Person
    { personFirstName :: Name
    , personLastName :: Name
    , yearOfBirth :: Year }
  deriving (Show)


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
  , Person "Henriette" "Avram" 1919
  , Person "Fikret" "Pascal" 1950  ]

firstLetterIs :: Char -> String -> Bool
firstLetterIs c ""    = False
firstLetterIs c (x:_) = c == x


-- My own exercise i did ths below without calling another Firstletter is but of course - 
firstNameBeginsWith :: Char -> Person -> Bool
-- firstNameBeginsWith c p =
firstNameBeginsWith c (Person _ "" _) = False
firstNameBeginsWith c (Person _ (x:_) _) = c == x
--    firstLetterIs c firstName
--  where firstName = personFirstName p

peopleThatBeginWithL :: [Person]
peopleThatBeginWithL =
  filter (firstNameBeginsWith 'L') people

peopleThatBeginWithP :: [Person]
peopleThatBeginWithP =
  filter (firstNameBeginsWith 'P') people

myNo :: [Int]
myNo = [112, 45, 54, 125, 266, 1033, 5400, 25, 163, 30003, 567, 125, 112, 57, 1098, 16]

gt100 :: Int -> Int -> Bool
gt100 a b = if b < a
      then False
      else True

numbersGT100 :: [Int]
numbersGT100 = filter (gt100 100) myNo     -- Filter is Function with 2 arguments returning bool



takeL = length people

divModx :: (Int, Int)
divModx  = divMod takeL 2

lastNameList :: [Person] -> [Name]
lastNameList [] = []
lastNameList [a] = [personLastName a]
lastNameList (x:xs) = personLastName x : lastNameList xs


x = Person "Isaac" "Newton" 1643


yearsSince :: Year -> Person -> Int 
yearsSince y (Person _ _ b) = y - b 

allYearsSince :: Year -> [Person] -> [Int]
allYearsSince y [] = []
allYearsSince y [a] = [yearsSince y a]
allYearsSince y (x:xs) = yearsSince y x : allYearsSince y xs

dob :: [Person] -> [Year]
dob [] = []
dob [p] = [yearOfBirth p]
dob (x:xs) = yearOfBirth x : dob xs

main :: IO ()
main = do
  putStrLn (" **** with L  " ++ show (peopleThatBeginWithL))
  putStrLn (" **** with P " ++ show (peopleThatBeginWithP))
  putStrLn (" Length = " ++ show (length people))
  putStrLn (" DivMod = " ++ show (divModx))
  putStrLn (" Numbers GT 100 " ++ show (numbersGT100))
  putStrLn (" Only lastName is " ++ show(personLastName x))
  putStrLn (" lastName List is " ++ show(lastNameList people))
  putStrLn (" Years since 2000 = " ++ show (yearsSince 2000 x))
  putStrLn (" DOB list = " ++ show (dob people))
  putStrLn (" All persons age to 2000 = " ++ show (allYearsSince 2000 people))
