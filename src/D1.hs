module D1 where

solve :: String -> IO (Integer)
solve f = do
    handle <- readFile f
    return $ foldr (\x a -> a + (((read x) `div` 3) - 2)) 0 (lines handle)

solve2 f = do
    handle <- readFile f
    return $ foldr (\x a -> a + (calcFuel (read x))) 0 (lines handle)
        where calcFuel n = case (((n `div` 3) - 2 ) <= 0) of
                                True    -> 0
                                False   -> (calc n) + calcFuel (calc n)
                                            where calc x = ((x `div` 3) - 2)
