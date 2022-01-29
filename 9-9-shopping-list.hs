


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


shop2 ::  [] String
shop2 = "Carrots" : "Oats" :  "Buttter" : []

shop3 ::  [] String
shop3 = "Carrots" : ( "Oats" : ( "Buttter" : [] ) )

main :: IO ()
main = do 
  print shoppingList
  print shop2
  print shop3


