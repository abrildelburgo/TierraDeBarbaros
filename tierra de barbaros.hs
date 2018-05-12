-- PUNTO 1
type Habilidad = String
type Accion = Barbaro -> Barbaro
type EventoDeAventura = Barbaro -> Bool

data Barbaro = Barbaro { nombre :: String, fuerza :: Int, habilidades :: [String], objetos :: [Accion] }

espadas :: Int -> Accion
espadas peso unBarbaro = unBarbaro { fuerza = fuerza unBarbaro + 2 * peso }

amuletosMisticos :: String -> Habilidad -> Accion
amuletosMisticos tipoDeAmuletoMistico unaHabilidad unBarbaro | tipoDeAmuletoMistico == "puerco-marranos" = anadirHabilidad unaHabilidad unBarbaro
															 | otherwise = unBarbaro

anadirHabilidad :: Habilidad -> Accion
anadirHabilidad unaHabilidad unBarbaro = unBarbaro { habilidades = unaHabilidad : (habilidades unBarbaro) }

varitasDefectuosas :: Accion
varitasDefectuosas = eliminarObjetos . anadirHabilidad "magia"

eliminarObjetos :: Accion
eliminarObjetos unBarbaro = unBarbaro { objetos = [] }

ardilla :: Accion
ardilla = id

cuerda :: Accion -> Accion -> Accion
cuerda = (.)

dave = ("Dave", 100, ["tejer", "escribir Poesia"], [ardilla, libroPedKing])
jeff = ("Jeff", 30, ["cocinar", "programar", "luchar"], [espadas, ardilla])


--PUNTO 2
megafono :: Accion
megafono unBarbaro = unBarbaro { habilidades = (map toUpper.concat) habilidades unBarbaro } 

megafonoBarbarico :: Accion -> Accion -> Accion
megafonoBarbarico = megafono . ardilla . cuerda
 

--PUNTO 3
invasionDeSuciosDuendes :: EventoDeAventura
invasionDeSuciosDuendes = elem "Escribir Poesia Atroz". habilidades

cremalleraDelTiempo :: EventoDeAventura
cremalleraDelTiempo = noTienePulgares

noTienePulgares :: EventoDeAventura
noTienePulgares unBarbaro = (nombre unBarbaro == "Faffy") || (nombre unBarbaro == "Astro")

ritualDeFechorias :: EventoDeAventura
ritualDeFechorias = saqueo unBarbaro || gritoDeGuera poderDeGrito unBarbaro || caligrafia unBarbaro

saqueo:: EventoDeAventura
saqueo unBarbaro = (tieneHabilidad robar unBarbaro) && (tieneFuerza 80 unBarbaro)

tieneHabilidad :: Habilidad -> Barbaro -> Bool
tieneHabilidad unaHabilidad =  elem unaHabilidad . habilidades

tieneFuerza :: Int -> Barbaro -> Bool
tieneFuerza cantidadDeFuerza unBarbaro = fuerza unBarbaro > cantidadDeFuerza

gritoDeGuera :: Int -> Accion
gritoDeGuera poderDeGrito unBarbaro = ( gritoIgualALetrasDeHabilidades poderDeGrito (habilidades unBarbaro) ) && ( relacionGritoObjetos poderDeGrito (objetos unBarbaro) 4 )

gritoIgualALetrasDeHabilidades :: Int -> [Habilidad] -> Bool
gritoIgualALetrasDeHabilidades poderDeGrito listaDeHabilidades = poderDeGrito == (length.head.concat) listaDeHabilidades

relacionGritoObjetos :: Int -> [Accion] -> Int -> Bool
relacionGritoObjetos poderDeGrito listaDeObjetos numeroDeCondicion = poderDeGrito == ((*numeroDeCondicion).length) listaDeObjetos

caligrafia :: EventoDeAventura
caligrafia unBarbaro = habilidadesTienenTresVocales habilidades unBarbaro && habilidadesComienzanMayuscula habilidades unBarbaro

habilidadesTienenTresVocales :: [Habilidad] -> Bool
habilidadesTienenTresVocales = all tieneTresVocales

tieneTresVocales :: Habilidad -> Bool
tieneTresVocales unaHabilidad = (length . filter esVocal) unaHabilidad > 3

vocales = ['a', 'e', 'i', 'o', 'u']

esVocal :: Char -> Bool
esVocal letra :: elem letra vocales

habilidadesComienzanMayuscula :: [Habilidad] -> Bool
habilidadesComienzanMayuscula listaDeHabilidades = map (toUpper.head) listaDeHabilidades == map head listaDeHabilidades

sobrevivientes :: EventoDeAventura -> [Barbaro] -> [Barbaro]
sobrevivientes aventura = filter aventura
