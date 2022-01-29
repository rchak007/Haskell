


aList8 :: Num a => [a]
aList8 = 1 : 3 : []



-- The (:) operator usually functions as though it were written like this:
-- we don't need these parentheses
aList8a :: Num a => [a]
aList8a = 1 : (3 : [])

main :: IO ()

main = do 
  print aList8
  print aList8a
