module Main where

import Control.Monad

readMaybe :: (Read a) => String -> Maybe a
readMaybe st = case reads st of [(x,"")] -> Just x
                                _ -> Nothing

foldingFunction :: [Double] -> String -> Maybe [Double]
foldingFunction (x:y:ys) op =return $ (case op of
                                    "*" -> y*x
                                    "/" -> y/x
                                    "mul" -> y*x
                                    "div" -> y/x
                                    "+" -> y+x
                                    "-" -> y-x
                                    "max" -> max y x
                                    "min" -> min y x
                                    ):ys
foldingFunction xs numberString = (:xs) <$> (readMaybe numberString)

solveRPN :: String -> Maybe Double
solveRPN st =do 
    [result]<- foldM foldingFunction [] (words st)
    return result

main :: IO ()
main = do 
    solveRPN <$> getLine >>= print
