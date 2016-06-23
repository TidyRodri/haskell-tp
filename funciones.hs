data Modificacion = Insertar Integer Char | Borrar Integer | Substituir Integer Char deriving (Show, Eq)

type PaqueteModificaciones = [Modificacion]

data Archivo = ArchivoVacio | NuevaVersion PaqueteModificaciones Archivo
instance Show Archivo where
     show ArchivoVacio = "Archivo vacio"
     show file = "Archivo: " ++ obtenerUltimaVersion file

len :: [a] -> Integer
len xs = fromIntegral (length xs)

archivo1 = NuevaVersion [Insertar 0 'd', Insertar 1 'a', Insertar 2 't',Insertar 3 'o'] ArchivoVacio

archivo2 = NuevaVersion [Insertar 0 'd'] archivo1
--------------------------------------------------------------------------------------------------------------------------------------------------------------------

aplicarModificacion :: String -> Modificacion -> String
aplicarModificacion str (Insertar index char) 
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
                                        | (length modif) == 1 = (aplicarModificacion str (head modif))
                                        | otherwise = (aplicarPaqueteModificaciones (aplicarModificacion str (head modif)) (tail modif))
                                    
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

--es como aplicar una gran modificacion, la logica es igual al del ej. 4 del parcial
obtenerUltimaVersion :: Archivo -> String
obtenerUltimaVersion (NuevaVersion package ArchivoVacio) =  aplicarPaqueteModificaciones "" package
obtenerUltimaVersion (NuevaVersion package version) = aplicarPaqueteModificaciones (obtenerUltimaVersion version) package
