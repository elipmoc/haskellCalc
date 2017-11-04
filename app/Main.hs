module Main where

import Control.Monad

readMaybe :: (Read a) => String -> Maybe a
readMaybe st = case reads st of [(x,"")] -> Just x
                                _ -> Nothing

foldingFunction :: [Double] -> String -> Maybe [Double]
foldingFunction (x:y:ys) "*" = return $ (y*x):ys
foldingFunction (x:y:ys) "/" = return $ (y/x):ys
foldingFunction (x:y:ys) "mul" = return $ (y*x):ys
foldingFunction (x:y:ys) "div" = return $ (y/x):ys
foldingFunction (x:y:ys) "+" = return $ (y+x):ys
foldingFunction (x:y:ys) "-" = return $ (y-x):ys
foldingFunction xs numberString = liftM (:xs) (readMaybe numberString)

solveRPN :: String -> Maybe Double
solveRPN st =do 
    [result]<- foldM foldingFunction [] (words st)
    return result

main :: IO ()
main = do 
    solveRPN <$> getLine >>= print
