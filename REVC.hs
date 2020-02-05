
sample = "AAAACCCGGT"
sampleOutput = "ACCGGGTTTT"

str = "GCGGCTACAAAGGTGTTATGGTAGGTCCACGGGGAGAGACGGTACTATTATCGGTGTAGCGGAAGTCGATAGCCCAATCGTCACGAGCTGGGTTAGGAGGCTGACACAGGTGCCAACTTTAAGAGAGAACAAGCATGAGCCCAACTTATGTAACTGCCATCTCTCTCGTGATGGGTTCCATACGGAATCGACGATCTAGATTGACCCCGTTAGCGATTATCCATAAGAGAGTTCCCCTTTTAACACTATCTGCAAGTTTTCCCCAGCTCGACGCGCGGATTGTATGTAGAACACTTCTCATCCTCTCCGATAGTGCTGTGCTGGACCTTAGCTCGGTCTCTAATTAATGTCGGTACGCTGCGACGGATCGGACCGGCCGCCCTGAAGAAGTGGCATTCCCGCTATCGTCTGACAACGTAGCACCTAACGCGCCTTCCGGCCGCAAGCCCAGCACCTTTCTAACAACCCCCGTATACGACCTTTCCCCCGTCTCGATCTCTCAGTGCCCGACCGTCTCTTACTGTCGTTAACGTCCGAATTCTAACTCTCTAGCAGTCCAGCTCCGGCCACGTCCGCTTGTAGAAATAAATCCGCGAGAGGTCCTGACCGTACAAACGCTAATAACCATCTCTCCTGGCCACAAGTAGGTATGCAGTCGTCACTCCGGGTCTCGCTCAGCTGCCCATGCTCTCAGTGAACCGTTATGCAGGGGCTGTGGGCCCATTTACGGCTTTGAAGTTAATCTAGGGTATCCCACAGAACTCCAGAGAGCCTAGCTCTCTTTTGCGTACACAGGGAGTTCGTCTCAAAGAGGGTGACCTAGTCTACGATAACTTCCTGTGAACGGCACATCCTGGCCCCAAACAGCGCGGGAACCAGAGACTTTAGGAAGTACGTTTGATATATCGCATATTTTATGGCCAAGTACAGTTTCTACCGGCAGCCAAGGGCAAGGCTTTCCGGCTATGGTAACGATTACACAGACCCGAATA"

compl 'C' = 'G'
compl 'G' = 'C'
compl 'A' = 'T'
compl 'T' = 'A'
compl x = error $ show x

revComp = fmap compl . reverse

main = do
    print $ revComp sample == sampleOutput
    putStrLn $ revComp str
