-- PUNTO 1
type Habilidad = String
type Accion = Barbaro -> Barbaro
type EventoDeAventura = Barbaro -> Bool

data Barbaro = Barbaro { nombre :: String, fuerza :: Int, habilidades :: [String], objetos :: [String] }

espadas :: Int -> Accion
espadas peso unBarbaro = unBarbaro { fuerza = fuerza unBarbaro + 2 * peso }

amuletosMisticos :: String -> Habilidad -> Accion
amuletosMisticos tipoDeAmuletoMistico unaHabilidad unBarbaro | tipoDeAmuletoMistico == "puerco-marranos" = anadirHabilidad unaHabilidad unBarbaro
															 | otherwise = unBarbaro

anadirHabilidad :: Habilidad -> Accion
anadirHabilidad unaHabilidad unBarbaro = unBarbaro { habilidades = unaHabilidad : (habilidades unBarbaro) }

varitasDefectuosas :: Habilidad -> Accion
varitasDefectuosas unaHabilidad = eliminarObjetos . anadirHabilidad "magia"

eliminarObjetos :: Accion
eliminarObjetos unBarbaro = unBarbaro { objetos = [] }

ardilla :: Accion
ardilla unBarbaro = unBarbaro

cuerda :: Accion -> Accion -> Accion
cuerda objeto1 objeto2 = objeto2 . objeto1

dave = ("Dave", 100, ["tejer", "escribir Poesia"], ["ardilla", "libroPedKing"])
jeff = ("Jeff", 30, ["cocinar", "programar", "luchar"], ["espada", "cuchara"])


--PUNTO 2
megafono :: Accion
megafono unBarbaro = unBarbaro { habilidades = map (\habilidadConcatenada -> toUpper habilidadConcatenada) (concat habilidades unBarbaro) } 

megafonoBarbarico :: Accion -> Accion -> Accion
megafonoBarbarico objeto1 objeto2 = megafono . ardilla . cuerda objeto1 objeto2 
 

--PUNTO 3
invasionDeSuciosDuendes :: EventoDeAventura
invasionDeSuciosDuendes unBarbaro = elem "Escribir Poesia Atroz" (habilidades unBarbaro)

cremalleraDelTiempo :: EventoDeAventura
cremalleraDelTiempo = noTienePulgares

noTienePulgares :: EventoDeAventura
noTienePulgares unBarbaro = (nombre unBarbaro == "Faffy") || (nombre unBarbaro == "Astro")

ritualDeFechorias :: EventoDeAventura
ritualDeFechorias = saqueo unBarbaro || gritoDeGuera poderDeGrito unBarbaro || caligrafia unBarbaro

saqueo:: EventoDeAventura
saqueo unBarbaro = (tieneHabilidad robar unBarbaro) && (tieneFuerza 80 unBarbaro)

tieneHabilidad unaHabilidad unBarbaro = ( length . filter (==unaHabilidad) ) (habilidades unBarbaro) > 0

tieneFuerza cantidadDeFuerza unBarbaro = fuerza unBarbaro > cantidadDeFuerza

gritoDeGuera :: Int -> Accion
gritoDeGuera poderDeGrito unBarbaro = ( gritoIgualALetrasDeHabilidades poderDeGrito (habilidades unBarbaro) ) && ( relacionGritoObjetos poderDeGrito (objetos unBarbaro) 4 )

gritoIgualALetrasDeHabilidades poderDeGrito listaDeHabilidades = poderDeGrito == (length.head.concat) listaDeHabilidades

relacionFuerzaObjetos poderDeGrito listaDeObjetos numeroDeCondicion = poderDeGrito == ((*numeroDeCondicion).length) listaDeObjetos

caligrafia :: EventoDeAventura
caligrafia unBarbaro = habilidadesTienenTresVocales habilidades unBarbaro && habilidadesComienzanMayuscula habilidades unBarbaro

habilidadesTienenTresVocales listaDeHabilidades = all tieneTresVocales listaDeHabilidades

tieneTresVocales unaHabilidad = (length . filter vocales) unaHabilidad > 3

habilidadesComienzanMayuscula listaDeHabilidades = map (toUpper.head) listaDeHabilidades == map head listaDeHabilidades

sobrevivientes :: [Barbaro] -> EventoDeAventura -> [Barbaro]
sobrevivientes listaDeBarbaros aventura = filter aventura listaDeBarbaros