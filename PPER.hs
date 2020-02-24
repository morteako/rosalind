{-# LANGUAGE DataKinds #-}

import Data.Map as Map
import GHC.TypeLits hiding (Mod)
import Data.Mod


type M = Mod 1000000

comb :: M -> M -> M
comb n k = product [n-k+1 .. n]

main =
    print $ comb 93 8