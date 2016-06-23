import Data

aplicarModificacion1 :: String -> Modificacion -> String
aplicarModificacion1 str (Insertar index char) 
    | index > (len str) || index < 0 = error "Out of range"
    | index == (len str) = str ++ [char]
    | otherwise = aplicarModificacion1 (init str) (Insertar index char) ++ [last str]
aplicarModificacion1 str (Borrar index) 
    | index > (len str) || index < 1 = error "Out of range"
    | index == (len str) = init str
    | otherwise = aplicarModificacion1 (init str) (Borrar index) ++ [last str]
aplicarModificacion1 str (Substituir index char) 
    | index > (len str) || index < 0 = error "Out of range"
    | index == (len str) = init str ++ [char]
    | otherwise = aplicarModificacion1 (init str) (Substituir index char) ++ [last str]


aplicarPaqueteModificaciones :: String -> PaqueteModificaciones -> String
aplicarPaqueteModificaciones str modif  | (length modif) == 0 = str
                                        | (length modif) == 1 = (aplicarModificacion1 str (head modif))
                                        | otherwise = (aplicarPaqueteModificaciones (aplicarModificacion1 str (head modif)) (tail modif))