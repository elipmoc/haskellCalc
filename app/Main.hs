module Main where

readMaybe :: (Read a) => String -> Maybe a
readMaybe st = case reads st of [(x,"")] -> Just x
                                _ -> Nothing

solveRPN :: String -> Double
solveRPN =head . foldl foldingFunction [] . words
    where 
        foldingFunction (x:y:ys) "*" = (y*x):ys
        foldingFunction (x:y:ys) "/" = (y/x):ys
        foldingFunction (x:y:ys) "mul" = (y*x):ys
        foldingFunction (x:y:ys) "div" = (y/x):ys
        foldingFunction (x:y:ys) "+" = (y+x):ys
        foldingFunction (x:y:ys) "-" = (y-x):ys
        foldingFunction xs numberString = read numberString:xs

main :: IO ()
main = do 
    solveRPN <$> getLine >>= print
