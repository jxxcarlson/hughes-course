import System.IO
 
main :: IO()
main = 
     do 
         putStr "Bonds: " >> hFlush stdout
         bonds <- getLine
         putStr "US Equities: " >> hFlush stdout
         usEquities <- getLine
         putStr "Foreign Equities: " >> hFlush stdout
         foreignEquities <- getLine
         putStrLn bonds
         putStrLn . show . allocation $ [bonds, usEquities, foreignEquities]



allocation :: [String] -> [Float]
allocation assets_ = 
    let  
        assets = read <$> assets_ :: [Float]
        total = sum assets
    in
    (\x -> roundTo 1 (100*x/total)) <$> assets
     
roundTo :: Int -> Float -> Float
roundTo k x = 
    let   
        factor :: Float
        factor = 10.0 ** (fromIntegral k)

        x' :: Int
        x' = round (factor * x)
    in 
       (1.0 * fromIntegral x')/factor
