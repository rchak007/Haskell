
import Data.List
lp = [10,20,30,100, 500, 700, 1000, 50]

elem1 = 500

indexElem = elemIndex elem1 lp -- -- Just 4

-- https://www.codegrepper.com/code-examples/haskell/haskell+list+get+element+at+index


-- Retrieve data back
indexData :: Maybe Int
indexData = case indexElem of 
              Just indexElem -> Just (lp !! indexElem )
              Nothing -> Nothing

main :: IO()
main = do
    putStrLn (" index = " ++ show(indexElem))   -- Just 4
    putStrLn (" index = " ++ show(indexElem) ++ " Data = " ++ show(indexData))

