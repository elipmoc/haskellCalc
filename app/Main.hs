module Main where

import Control.Monad
import Control.Applicative

readMaybe :: (Read a) => String -> Maybe a
readMaybe st = case reads st of [(x,"")] -> Just x
                                _ -> Nothing

binaryExpr :: Double -> Double -> String -> Maybe Double
binaryExpr x y op = 
    case op of
        "*" -> return $ y*x
        "/" -> return $ y/x
        "mul" -> return $ y*x
        "div" -> return $ y/x
        "+" -> return $ y+x
        "-" -> return $ y-x
        "max" -> return $ max y x
        "min" -> return $ min y x
        _ -> Nothing

foldingFunction :: [Double] -> String -> Maybe [Double]
foldingFunction s@(x:y:ys) op = ((:ys) <$> binaryExpr x y op) <|> ((:s) <$> readMaybe op)
foldingFunction xs numberString = (:xs) <$> readMaybe numberString

solveRPN :: String -> Maybe Double
solveRPN st =do 
    [result]<- foldM foldingFunction [] (words st)
    return result

main :: IO ()
main = do 
    solveRPN <$> getLine >>= print
