import Data.Set (Set)


data Suffix = Node [Paths]
data Path = Path String Suffix | LeafPath String Suffix (Set String)

getSuffs (Node paths) = map f paths
    where
        f (Path s _) = s
        f (LeafPath s _ _) = s

makeSuffix str = 
    where
        suffixes = tails (str ++ "$")

make 


main = do
    print $ makeSuffix "BANANA"