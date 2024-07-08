-- Simple Calculator
import Text.Read (readMaybe)

main :: IO ()
main = do
    putStrLn "Welcome to the Haskell Calculator!"
    calculatorLoop

calculatorLoop :: IO ()
calculatorLoop = do
    putStrLn "Enter an expression (e.g., 3 + 4):"
    input <- getLine
    if input == "exit"
        then putStrLn "Goodbye!"
        else do
            let result = evaluate input
            case result of
                Just value -> putStrLn $ "Result: " ++ show value
                Nothing -> putStrLn "Invalid expression!"
            calculatorLoop

evaluate :: String -> Maybe Double
evaluate input = case words input of
    [x, "+", y] -> liftA2 (+) (readMaybe x) (readMaybe y)
    [x, "-", y] -> liftA2 (-) (readMaybe x) (readMaybe y)
    [x, "*", y] -> liftA2 (*) (readMaybe x) (readMaybe y)
    [x, "/", y] -> liftA2 (/) (readMaybe x) (readMaybe y)
    _           -> Nothing
