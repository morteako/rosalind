(defn (:: main (IO ()))
    (do (print (==  7
                    (hamDist 
                        "GAGCCTACTAACGGGAT" 
                        "CATCGTAATGACGGCCT")))
        (print (hamDist 
            a 
            b))))

(defn a "GGAAGTACAAGGGGCCGACCCCGCCCTAAACTCGGAGCCTGGCATTAGTCTCCGAACGCCGGCAGTAGAATACCGTGGCAATACAAGTAACATCGGACCGGGCTTTACGGGCTACCTGCTTTCGCCACACCTGGGCTCCTAGCAAAATCCTTATGGTGTCTTAGCGGCGCCACGTTCGGCACATTTATTGCTCGTCCCAGGTGCCCTTCATCGGTTCTCTACAGTTTAGTTGTCCATGGCTACTTGCTGCTGCGTGTTCAGCAGGATTAGTAGGGAGAAGTCTGTTTCTGAAAATGTGGTTGACTCAGTCCTGGGATTCTCATACAATAGCAATACCTTGGCGGTATAACCGTGGTACTCTCAGGATCAGTGGCTCCCAAGGGGGGTTGCCATCGTATTGTATAACCAGCTAGTGTAAAATATCTGACCCAACATGTAAGTATCCGTTGCCGTGCTAACCTGAGCGATATGACCCTATTGCCGAACCTCGTAGGGATTTGGGACTCAATGTGACGAACGATACTTGGGAAGAACGCCCACCTCTTGCGGGTTAGCGATACGGCGGGATAAGAGATGGGGCTGTAACGCACCCAAGTTGACAAAGGAACGCTCAGATTGGTCTTTTATACCGTCCGAAGAGCCTTGGAACACCATGGGACTAGTTCAGCTTATAAGGTAACCTCACTCGTGGCTTATTGGAATGTTATCGAGAAGAGAATGTCCGCGCCGGGAGACCTGTAGAAAGAGGTCTGGGTCGATAGTTATGTCTCCAGCCCTTTGGAGGCCTGCCCGGCAAGGAGAGGCGTGGGCTAGTAAGTAGCTCGATTTTAGACCATTGCTGTACCACTATGGCTAATTTACGTTTATCTGTAGAAAAACACGGATTGACAACTGGTCCATACAATGGTTTATCCTTTTTA")
(defn b "CAGTGATTAGGTGACCGTGCCACCTCTTAAATCGCATCTTCATCTAGCCTTTCAAAGGGTGCCAGTGGAACACTGTTGGTCACGAACGATCGTTGCACTGGTCTCTAAGTGCTACCAATCCTCAAACTAGATCCTTAAGCTCCAAAAACCTTATGCTGACTTGGGGGAAGTACGATCAGTACTTCCCTTCCTTCTGCTAGAAGCCTCTCGCCGCTCCTGTACCCCTTACTTGGGGCACCTTCTTGGCGAATCGGTGCTCTGTGTGTTTCGTAGCGTGATGACGGATTCTGAAATTGGCGTTCTATCGTCCGTGGGATGGCCATACCAGAGGGACGCGTTGACTCTTGTAGCGTGGTCCCCACTTTTATAGTTACACGTCAGAGGGGCGGCTGCCTTTTTCCACACCCAATCTGATTAAGAACTTTTGCCAACCATCTCGTATCACGGTGCCATTAATTTTTGTGGCATTGGACTCAATCCGCAAACGGAGTCCGATCACTGGATGCAATGTTCCAGGCGGTCAAGAGATCGACCTACTCAGTACTGCGCTTCACTGAGAAGAGGTCATTCTCGGTGGGTATGTATAGTAGCAAACGTGATTAGAAATCGTCGAAATCGGTATTGTTCATGTATTCATGATCCCTGAAATCAGATCTTATGACTTCACCGCAGTTTGGGTTGTCGGTATTGTCATAGTGCTATGTTGGAGTCCCGGGTACGTCATAGGCTGTACCGATATAAAGACACGTCCGTTTCGATATATATTTCAGCAGCTCGACGGAGGCTTCCCCTCGGAGGCTTAGGTACTCCAATGTGACTTCTTGCACTTACACAACTTGTATCAGCCTCTGACACCTTTAGTGTCCTCTATAGCAAGACTTTGACCCATGTCTGGTTGTTAATATGCCCTATCCTTGGCT")

(defn (:: hamDist (-> String String Int))
    [xs ys] 
        ($  length
            (filter id)
            (zipWith (/=) xs ys)))
