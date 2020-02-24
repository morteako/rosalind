
fibs = 1:1:zipWith (+) fibs (tail fibs)

fibN = (fibs !!) . subtract 1

main = do
    print $ fibN 6 == 8
    print $ fibN 24