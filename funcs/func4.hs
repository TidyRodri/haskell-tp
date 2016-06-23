--contamos cada paquete de modificaciones
cantVersiones :: Archivo -> Integer
cantVersiones ArchivoVacio = 0 --no consideramos el empty file como version
cantVersiones (NuevaVersion _ file) = 1 + cantVersiones file
