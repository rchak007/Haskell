
import qualified Data.List as L
bytecode =
  [ "LOAD_VAL 1"
  , "WRITE_VAR x"
  , "LOAD_VAL 2"
  , "WRITE_VAR y"
  , "READ_VAR x"
  , "LOAD_VAL 1"
  , "ADD"
  , "READ_VAR y"
  , "MULTIPLY"
  , "RETURN_VALUE"
  ]


type Linex = [String]
line1 = "LOAD_VAL 1"

-- words1 = words "Hello world from Haskell"
-- words2 = words line1
-- words3 = words "LOAD_VAL 1"


-- isLoad :: String -> Bool
-- isLoad x = x == "LOAD_VAL"   -- takes first letter from a String 

-- load_flg = isLoad "LOAD_VAL"


-- load1 :: String -> String
-- load1 arg = arg ++ " - " ++ assessment
--    where assessment = if isLoad arg
--                      then "this is load_val"
--                      else "this is NOT load_val"

wordC :: String -> Linex
wordC w = words w ++ ["EOL"] 


lineLoad :: [String] -> Linex
lineLoad []     = []
--lineLoad [x]    = "Line read " ++ x ++ wordC x
--lineLoad (x:xs) = "Line read " ++ x ++ wordC x ++ lineLoad xs
lineLoad [x]    = wordC x
lineLoad (x:xs) = wordC x ++ lineLoad xs


--bytecodeParsed :: [String] -> [String]
bytecodeParsed = lineLoad bytecode


type Varname = String
type Var_val = Int
type Variable_item = (Varname, Var_val)
type Wa_variables = [Variable_item]


isLoad :: [String] -> String
isLoad [] = []
isLoad [name] 
  | name == "LOAD_VAL" = name ++ " load_val found"
  | otherwise      = name ++ " Load_val not found."
isLoad (name:nameXs) 
  | name == "LOAD_VAL" = name ++ " load_val found" ++ isLoad nameXs
  | otherwise = name ++ " Load_val not found." ++ isLoad nameXs



-- addList :: [String]  -> [String]
-- addList x = x : []

-- assessWords :: [String]
-- assessWords = words w

-- assessLine :: String -> String
-- assessLine  arg = arg ++ "  load Yes " ++ assessment
--   where assessment = if lineLoad assessWords arg
--                      then "this "



-- separate piece to read all the byte code
-- assessByte :: [String] -> [String]
-- assessByte = map assessLine

-- assessedBytes :: [String]
-- assessedBytes = assessByte bytecode


main :: IO ()
main = do 
  --putStrLn (show words1 )
  --putStrLn (show words2)
  --putStrLn (show words3)
  --putStrLn (show load_flg)
  --putStrLn (show (lineLoad  bytecode))
  putStrLn (show bytecodeParsed)
  --putStrLn (show bytecode)
  -- putStrLn (L.intercalate "\n" assessedBytes)

  --putStrLn (show (wordC line1))
  putStrLn (show (isLoad bytecodeParsed))
