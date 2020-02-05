


rabbits months litter = res !! (months - 1) 
    where
        res = 1 : 1 : zipWith f res (tail res)
        f n2 n1 = n1 + (litter * n2)


main = do
    print $ rabbits 5 3 == 19
    print $ rabbits 32 4
