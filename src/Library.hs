{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
module Library where
import PdePreludat

doble :: Number -> Number
doble numero = numero + numero

data Personaje = UnPersonaje {
    nombre :: Nombre,
    elementos :: [Elemento]
} deriving (Show,Eq)

type Nombre = String
type Elemento = String


todosLosPersonajes :: [Personaje]
todosLosPersonajes = [UnPersonaje "Toph" ["Tierra"], UnPersonaje "Katara" ["Agua"], UnPersonaje "Zuko" ["Fuego"],
                      UnPersonaje "Aang" ["Tierra","Fuego","Agua"], UnPersonaje "Sokka" [] ]


-- Cuando alguien tiene el control de uno de ellos, le llamamos "maestro".
esMaestro' :: Personaje -> Bool
esMaestro' pers = length (elementos pers) > 0



esMaestro :: Nombre -> Bool
esMaestro unNombre = any (\maestro -> unNombre == nombre maestro && elementos maestro /= []) todosLosPersonajes

-- Una determinada persona es maestra de un determinado elemento

esMaestroDe :: Nombre -> Elemento -> Bool
esMaestroDe unNombre unElemento = any (tieneNombreYElemento unNombre unElemento) todosLosPersonajes

tieneNombreYElemento :: Nombre -> Elemento -> Personaje -> Bool
tieneNombreYElemento unNombre unElemento personaje = unNombre == nombre personaje && unElemento `elem` elementos personaje

-- Qué elementos domina un determinado personaje.

queElementosDomina :: Nombre -> [Elemento]
queElementosDomina unNombre =  (elementos . head . filter ((==unNombre) . nombre)) todosLosPersonajes

--Quiénes son maestros de un determinado elemento.

quienesSonMaestrosDe :: Elemento -> [Personaje]
quienesSonMaestrosDe elemento = filter (elem elemento . elementos) todosLosPersonajes

-- Es cierto que cierto elemento existe 

existeElemento :: String -> Bool
existeElemento elemento = elemento `elem` concatMap elementos todosLosPersonajes

--todos los maestros

todosLosMaestros :: [Nombre]
todosLosMaestros = (filter esMaestro . map nombre) todosLosPersonajes

--todos los elementos

todosLosElementos :: [Elemento]
todosLosElementos = (filter existeElemento . concatMap elementos) todosLosPersonajes

