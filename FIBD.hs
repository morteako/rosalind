import           Data.Bifoldable
import qualified Data.Map.Strict               as Map

rabbits months lives = (x + y, (x, y))
 where
  (x, y, _) = f init months

  init      = Map.fromList $ zip [1, 2, 3] [(1, 0), (0, 1), (1, 1)]

  f dict n
    | Just (a, b) <- Map.lookup n dict
    = (a, b, dict)
    | lives >= n
    = let (py, po, dp) = f dict (n - 1)
      in  (po, po + py, Map.insert (n - 1) (py, po) dp)
    | otherwise
    = let (py , po , dp ) = f (Map.insert (n - lives) (ppy, ppo) dpp) (n - 1)
          (ppy, ppo, dpp) = f dict (n - lives)
      in  (po, po + py - ppy, Map.insert (n - 1) (ppy, ppo) dp)



main = do
--   print $ rabbits 4 3
--   print $ rabbits 5 3
  print $ rabbits 92 18
--   print $ map (flip rabbits 3) [1 .. 6]

