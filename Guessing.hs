guess :: Int -> Int -> IO ()
guess l u 
    = do    let m = (u + l) `div` 2
            putStr ("Is it " ++ show m ++ "?")
            putStrLn "(g = greater, l = less, c = correct)"
            k <- getChar
            case k of
                'g' -> guess (m + 1) u
                'l' -> guess l (m-1)
                'c' -> putStrLn "Guessed"
                _   -> do   putStrLn "Press type g/l/c!"
                            guess l u