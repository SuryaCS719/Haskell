-- Unit Converter
main :: IO ()
main = do
    putStrLn "Welcome to the Haskell Unit Converter!"
    unitConverterLoop

unitConverterLoop :: IO ()
unitConverterLoop = do
    putStrLn "Choose a conversion (e.g., in to cm, lb to kg, f to c):"
    input <- getLine
    if input == "exit"
        then putStrLn "Goodbye!"
        else do
            putStrLn "Enter the value to convert:"
            valueInput <- getLine
            let value = readMaybe valueInput :: Maybe Double
            case value of
                Just v -> do
                    let result = convert input v
                    case result of
                        Just res -> putStrLn $ "Converted value: " ++ show res
                        Nothing -> putStrLn "Invalid conversion type!"
                Nothing -> putStrLn "Invalid value!"
            unitConverterLoop

convert :: String -> Double -> Maybe Double
convert "in to cm" = Just . (* 2.54)
convert "cm to in" = Just . (/ 2.54)
convert "lb to kg" = Just . (* 0.453592)
convert "kg to lb" = Just . (/ 0.453592)
convert "f to c"   = Just . (\f -> (f - 32) * 5 / 9)
convert "c to f"   = Just . (\c -> c * 9 / 5 + 32)
convert _          = Nothing
