import Data

aplicarModificacion :: String -> Modificacion -> String
aplicarModificacion1 str (Insertar index char) 
    | index > (len str) || index < 0 = error "Out of range"
    | index == (len str) = str ++ [char]
    | otherwise = aplicarModificacion (init str) (Insertar index char) ++ [last str]
aplicarModificacion str (Borrar index) 
    | index > (len str) || index < 1 = error "Out of range"
    | index == (len str) = init str
    | otherwise = aplicarModificacion (init str) (Borrar index) ++ [last str]
aplicarModificacion str (Substituir index char) 
    | index > (len str) || index < 0 = error "Out of range"
    | index == (len str) = init str ++ [char]
    | otherwise = aplicarModificacion (init str) (Substituir index char) ++ [last str]


aplicarPaqueteModificaciones :: String -> PaqueteModificaciones -> String
aplicarPaqueteModificaciones str modif  | (length modif) == 0 = str
                                        | (length modif) == 1 = (aplicarModificacion1 str (head modif))
                                        | otherwise = (aplicarPaqueteModificaciones (aplicarModificacion1 str (head modif)) (tail modif))
                                    
--contamos cada paquete de modificaciones
cantVersiones :: Archivo -> Integer
cantVersiones ArchivoVacio = 0 --no consideramos el empty file como version
cantVersiones (NuevaVersion _ file) = 1 + cantVersiones file

--calcado del algo del pddf
--no sé si puedo usar minimum, sino hacer min a (min b c)
levenshtein :: String -> String -> Integer
levenshtein str1 str2 | (min (len str1) (len str2)) == 0 = max (len str1) (len str2)
                      | (last str1) == (last str2)       = minimum [lev1 + 1,lev1 + 1, levenshtein (init str1) (init str2)]
                      | otherwise                        = minimum [lev1 + 1,lev1 + 1, levenshtein (init str1) (init str2) + 1]
                      where lev1 = levenshtein (init str1) str2
                            lev2 = levenshtein str1 (init str2)
