{-# LANGUAGE DataKinds #-}

import Data.Map as Map
import GHC.TypeLits hiding (Mod)
import Data.Mod

s = "MVNSQHQKTCVAVMANKPQNQQVGYWRMVLMWWQSNDECQCQVPEGPGPRHTWVVKRRAFCPKQWQLCTVRDWSIPCGRRLHLLCCPNEGPAYCCAGVMLVNFKQKMFMALATGDIKHIGLPKEPYGSAIKLWFTKNEQHMMMIMQEDENIEHVATKCNYRAHKVTCPHYMFTWMQHVICHFPVPCEIWAKLFDTEEDQNVIAGIALPWCVFWGLCWDDDFEDAFCSNVCYRQAPDAAMSHEKMCDSQVNEHRYQLCAYFPWVVARMQSTSDLYMGCCCHEWKCPDEPERKHPHFFWEMYWGVRHHIECTSHMEQETHITNMISYTVEDNNIGVYTICAGLACCSSGGQKVADVDVGEIFGPRCANASSIYCPMNAHMCMPQCHAPLPSHDEIVWIHCTTNRVTTYCEDVYFLNKCMHMTDSINVLEINRMKCFWRRSLMNEVKNSRMLRIHDGTSFMKYRMEHDGWDWFFNYWQKAWRATTIPFYDPDMYSLFWHRDFHHIWTKTFMICLCWPQSAMSHKSDPSAYISTTKNGKLMWDAYNIWDIVAQYENNNYPWVLWGSNRQNMWQHTKPLICHFPETYSYRSTIDGEYPWVWEWWTNMNFAYMFPVKAWATMWWCWYFFWHSQGVGFFGRDACSYPYNMFLRACRKIGISYNTRLTEVHMFVEKKQTMNRHWRDWDIIQGVLAIQWANQNYSNLAWYTYRYDVLMGSHPPLDHFFMRASMCAQMSYLHLTHIMHYHQGDHLFKWWYGWPQFAWMKLLEYACFVDAVNLFSVFPVFHAYDLVILKIPGVKVTWCNNSTCLIPHSVKDFINTVCCNINWSDNPERAGRIDHPYYAVQGAKNYESTEQPKWQLVWGHECTNIRGPARQNDQNHCCPEGGGVFPSDSALDAPFVGHCSSRAHEDVNEGATAAKTFNCMADVNISWTHDFSLNRNGVVASNEYMMYVQHPTHPQLNWMIFILLMLNIKFGFNCFWIRKHQRVDSALRAKLRMCQCEKMAN"

calc str = 3 * product (fmap ((xs !) . pure) str)

main = do
    print $ calc "MA"
    print $ calc s

f = flip (,)

xs :: Map String (Mod 1000000)
xs = Map.fromListWith (+) $ (fmap.fmap) (const 1) [
    f "ACA" "T"      
    ,f "ACC" "T"      
    ,f "ACG" "T"      
    ,f "ACU" "T"      
    ,f "AGA" "R"      
    ,f "AGC" "S"      
    ,f "AGG" "R"      
    ,f "AGU" "S"      
    ,f "AUA" "I"      
    ,f "AUC" "I"      
    ,f "AUG" "M"      
    ,f "AUU" "I"      
    ,f "CAC" "H"      
    ,f "CAG" "Q"      
    ,f "CAU" "H"      
    ,f "CCA" "P"      
    ,f "CCC" "P"      
    ,f "CCG" "P"      
    ,f "CCU" "P"      
    ,f "CGA" "R"      
    ,f "CGC" "R"      
    ,f "CGG" "R"      
    ,f "CGU" "R"      
    ,f "CUA" "L"      
    ,f "CUC" "L"      
    ,f "CUG" "L"      
    ,f "CUU" "L"    
    ,f "CAA" "Q"      
    ,f "GAC" "D"
    ,f "GAG" "E"
    ,f "GAU" "D"
    ,f "GCA" "A"
    ,f "GCC" "A"
    ,f "GCG" "A"
    ,f "GCU" "A"
    ,f "GGA" "G"
    ,f "GGC" "G"
    ,f "GGG" "G" 
    ,f "GGU" "G"
    ,f "GUA" "V"
    ,f "GUC" "V"
    ,f "GUG" "V"
    ,f "GUU" "V"
    ,f "GAA" "E"
    ,f "UAC" "Y"      
    ,f "UAG" ""   
    ,f "UAU" "Y"      
    ,f "UCA" "S"      
    ,f "UCC" "S"      
    ,f "UCG" "S"      
    ,f "UCU" "S"      
    ,f "UGA" ""   
    ,f "UGC" "C"      
    ,f "UGG" "W"      
    ,f "UGU" "C"      
    ,f "UUA" "L"      
    ,f "UUC" "F"      
    ,f "UUG" "L"      
    ,f "UUU" "F"     
    ,f "UAA" ""   
    ,f "AAA" "K"      
    ,f "AAC" "N"      
    ,f "AAG" "K"      
    ,f "AAU" "N"      
    ]