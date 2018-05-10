-- PUNTO 1
type Habilidad = String
type Barb = Barbaro -> Barbaro
type Sobreviviente = Barbaro -> Bool

data Barbaro = Barbaro { nombre :: String, fuerza :: Int, habilidades :: [String], objetos :: [String] }

espadas :: Int -> Barbaro -> Barbaro
espadas peso unBarbaro = unBarbaro { fuerza = fuerza unBarbaro + 2 * peso }

amuletosMisticos :: String -> Habilidad -> Barbaro -> Barbaro
amuletosMisticos tipoDeAmuletoMistico unaHabilidad unBarbaro | tipoDeAmuletoMistico == "puerco-marranos" = anadirHabilidad unaHabilidad unBarbaro
															 | otherwise = unBarbaro

anadirHabilidad :: Habilidad -> Barbaro -> Barbaro
anadirHabilidad unaHabilidad unBarbaro = unBarbaro { habilidades = unaHabilidad : (habilidades unBarbaro) }

varitasDefectuosas :: Habilidad -> Barbaro -> Barbaro
varitasDefectuosas unaHabilidad = eliminarObjetos . anadirHabilidad "magia"

eliminarObjetos :: Barb
eliminarObjetos unBarbaro = unBarbaro { objetos = [] }

ardilla :: Barb
ardilla unBarbaro = unBarbaro

cuerda :: (Barb) -> (Barb) -> Barbaro -> Barbaro
cuerda objeto1 objeto2 = objeto2 . objeto1

dave = ("Dave", 100, ["tejer", "escribir Poesia"], ["ardilla", "libroPedKing"])
jeff = ("Jeff", 30, ["cocinar", "programar", "luchar"], ["espada", "cuchara"])


--PUNTO 2
-- megafono :: Barb
--                          megafono unBarbaro = unBarbaro { habilidades = (map.map) toUpper (concat habilidades unBarbaro) } 

megafonoBarbarico :: (Barb)-> (Barb) -> Barbaro -> Barbaro
megafonoBarbarico objeto1 objeto2 = megafono . ardilla . cuerda objeto1 objeto2 
 

--PUNTO 3
invasionDeSuciosDuendes :: Sobreviviente
invasionDeSuciosDuendes unBarbaro = elem "Escribir Poesia Atroz" (habilidades unBarbaro)

cremalleraDelTiempo :: Sobreviviente
cremalleraDelTiempo = noTienePulgares

noTienePulgares :: Sobreviviente
noTienePulgares unBarbaro = (nombre unBarbaro == "Faffy") || (nombre unBarbaro == "Astro")

ritualDeFechorias :: Sobreviviente
ritualDeFechorias = sobrevive

sobrevive:: Sobreviviente
sobrevive unBarbaro = saqueo unBarbaro || gritoDeGuera poderDeGrito unBarbaro || caligrafia unBarbaro

saqueo:: Sobreviviente
saqueo unBarbaro = (tieneHabilidad robar unBarbaro) && (tieneFuerza 80 unBarbaro)

tieneHabilidad unaHabilidad unBarbaro = ( length . filter (==unaHabilidad) ) (habilidades unBarbaro) /= 0

tieneFuerza cantidadDeFuerza unBarbaro = fuerza unBarbaro > cantidadDeFuerza

gritoDeGuera :: Int -> Barbaro -> Barbaro
gritoDeGuera poderDeGrito unBarbaro = ( gritoIgualALetrasDeHabilidades poderDeGrito (habilidades unBarbaro) ) && ( relacionFuerzaObjetos (fuerza unBarbaro) (objetos unBarbaro) 4 )

gritoIgualALetrasDeHabilidades poderDeGrito listaDeHabilidades = poderDeGrito == (length.head.concat) listaDeHabilidades

relacionFuerzaObjetos unaFuerza listaDeObjetos numeroDeCondicion = unaFuerza == ((*4).length) listaDeObjetos

caligrafia :: Sobreviviente
caligrafia unBarbaro = habilidadesTienenTresVocales habilidades unBarbaro && habilidadesComienzanMayuscula habilidades unBarbaro

habilidadesTienenTresVocales listaDeHabilidades = all tieneTresVocales listaDeHabilidades

--                        tieneTresVocales unaHabilidad = (length . filter vocales) unaHabilidad > 3

habilidadesComienzanMayuscula listaDeHabilidades = map (toUpper.head) listaDeHabilidades == map head listaDeHabilidades

-- sobrevivientes :: [Barbaro] -> (Sobreviviente) -> [Barbaro]
--                        sobrevivientes listaDeBarbaros aventura = filter aventura listaDeBarbaros