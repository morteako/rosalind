import Text.Printf
import Data.List.Extra
import Data.Bifoldable

data Avg = Avg Double Double deriving (Show,Eq)

instance Semigroup Avg where
    Avg xc xs <> Avg yc ys = Avg (xc+yc) (xs+ys)

instance Monoid Avg where
    mempty = Avg 0 0




calc (('>':ros):xs) = (ros,foldMap f $ concat xs) : calc rest
    where
        (cur,rest) = span (not . isPrefixOf ">") xs
        f 'C'   = Avg 1 1
        f 'G'   = Avg 1 1
        f _     = Avg 0 1
calc [] = []
calc xs = error $ show xs

pp :: Avg -> String
pp (Avg x y) = printf "%.5f" (100 * x / y)

main = do
    xs <- lines <$> readFile "gc.txt"
    bitraverse_ putStrLn putStrLn $ maximumOn snd $ map (\(x,y) -> (x,pp y)) $ calc xs
    