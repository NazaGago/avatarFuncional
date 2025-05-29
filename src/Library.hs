module Library where
import PdePreludat

doble :: Number -> Number
doble numero = numero + numero

data Personaje = UnPersonaje {
    elementos :: [String]
}deriving (Show,Eq)

losElementos :: [String]
losElementos = ["Fuego","Aire","Tierra","Agua","Rayo","Metal","Lava","Sangre"]

-- algunos Personajes

toph :: Personaje
toph = UnPersonaje ["Tierra"]

katara :: Personaje
katara = UnPersonaje ["Agua"]

zuko :: Personaje
zuko = UnPersonaje ["Fuego"]

aang :: Personaje
aang = UnPersonaje ["Agua","Tierra","Fuego"]

sokka :: Personaje
sokka = UnPersonaje []


-- Cuando alguien tiene el control de uno de ellos, le llamamos "maestro".

esMaestro :: Personaje -> Bool
esMaestro = not . null . elementos

-- Una determinada persona es maestra de un determinado elemento

esMaestroDe :: String -> Personaje -> Bool
esMaestroDe elemento personaje = elemento `elem` (elementos personaje)

-- Qué elementos domina un determinado maestro.

deQueEsMaestro :: Personaje -> [String]
deQueEsMaestro = elementos

-- Quiénes son maestros de un determinado elemento.

quienesSonMaestrosDe :: String -> [Personaje] -> [Personaje]
quienesSonMaestrosDe elemento = filter (esMaestroDe elemento)

-- Es cierto que cierto elemento existe (Debo determinar la lista de elementos existentes para poder realizarlo)

existeElemento :: String -> Bool
existeElemento elemento = elemento `elem` losElementos

-- no se puede determinar todos los maestros existentes debido a que no hay principio de Universo Cerrado.