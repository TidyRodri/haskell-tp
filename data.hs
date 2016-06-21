module Data where

data Modificacion = Insertar Integer Char | Borrar Integer | Substituir Integer Char deriving (Show, Eq)

-- type PaqueteModificaciones = [Modificacion]

-- data Archivo = ArchivoVacio | NuevaVersion PaqueteModificaciones Archivo
-- instance Show Archivo where
--     show ArchivoVacio = "Archivo vacio"
--     show file = "Archivo: " ++ obtenerUltimaVersion file

-- data SCV = NuevoSCV | AgregarArchivo Archivo SCV
-- instance Show SCV where
--     show NuevoSCV = "SCV vacio"
--     show scv = verArchivos scv

-- verArchivos :: SCV -> String
-- verArchivos NuevoSCV = ""
-- verArchivos (AgregarArchivo file scv) = "- " ++ (show file) ++ "\n" ++ (verArchivos scv)

len :: [a] -> Integer
len xs = fromIntegral (length xs)