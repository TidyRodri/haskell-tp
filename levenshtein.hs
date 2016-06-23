levenshtein :: String -> String -> Integer
levenshtein str1 str2 | (min (len str1) (len str2)) == 0 = max (len str1) (len str2)
                      | (last str1) == (last str2)       = minimum [lev1 + 1,lev1 + 1, levenshtein (init str1) (init str2)]
                      | otherwise                        = minimum [lev1 + 1,lev1 + 1, levenshtein (init str1) (init str2) + 1]
                      where lev1 = levenshtein (init str1) str2
                            lev2 = levenshtein str1 (init str2)
