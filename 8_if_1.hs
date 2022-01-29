message :: String -> String
message name = if name == "Dave"
               then "I can't do that."
               else "Hello."

-- (==) operator
-- (==) :: Eq a => a -> a -> Bool


-- Nesting if Expressions
message1 :: String -> String
message1 name =
  if name == "Dave"
  then "I can't do that."
  else if name == "Sam"
       then "Play it again."
       else "Hello."


-- Case Expressions
message2 :: String -> String
message2 name =
  case name of
    "Dave" -> "I can't do that."
    "Sam"  -> "Play it again."
    _      -> "Hello."



-- Guard Patterns

message3 :: String -> String
message3 name
  | name == "Dave" = "I can't do that."
  | name == "Sam"  = "Play it again."
  | otherwise      = "Hello."

-- The otherwise identifier is defined very simple, as True
message4 :: String -> String
message4 name
  | name == "Dave" = "I can't do that."
  | name == "Sam"  = "Play it again."
  | True      = "Hello."


-- Argument Pattern Matching
--    still one more way we can write this 
       --just a simple regular function definition, but with many definitions, and using pattern matching on values in the argument list:

message5 :: String -> String
message5 "Dave" = "I can't do that."
message5 "Sam"  = "Play it again."
message5 _      = "Hello."



main :: IO ()
main = do 
    putStrLn (message "Dave")
    putStrLn (message1 "Dave")
    putStrLn (message1 "Sam")
    putStrLn (message2 "Dave")
    putStrLn (message2 "Sam")
    putStrLn (message3 "Dave")
    putStrLn (message3 "Sam")
    putStrLn (message4 "Dave")
    putStrLn (message4 "Sam")
    putStrLn (message5 "Dave")
    putStrLn (message5 "Sam")












