module Lib where
import Text.Show.Functions
laVerdad = True

data Persona = UnaPersona {
    nombre ::String,
    dinero:: Float,
    suerte:: Int,
    factores :: [(String,Int)] 
}deriving(Show)
nico = (UnaPersona "Nico" 100.0 30 [("amuleto", 3), ("manos magicas",100)])
maiu = (UnaPersona "Maiu" 100.0 42 [("inteligencia",55), ("paciencia",50)])

----------------------------- Punto 1 -----------------------------
suerteTotal :: Persona->Int
suerteTotal persona |tieneObjeto "amuleto" (factores persona) = incrementoSuerte persona
                    |otherwise = (suerte persona)

tieneObjeto :: String->[(String,Int)]->Bool
tieneObjeto objeto listaFactores = (elem objeto.map fst) listaFactores && (all (>0).map snd) listaFactores

incrementoSuerte :: Persona->Int
incrementoSuerte persona = (suerte persona) * (tomoValorObjeto "amuleto" persona)

tomoValorObjeto :: String->Persona->Int
tomoValorObjeto objeto  = snd.head.filter (tieneObjeto objeto).factores

----------------------------- Punto 2 -----------------------------
data Juego = UnJuego{
    nombreJuego :: String,
    dineroAGanar :: (Float->Float),
    criterios :: [Persona->Bool]
}deriving (Show)

jackpot = undefined -- se deberia definir

ruleta ::Juego
ruleta  = UnJuego "ruleta"  (*37) [(>80).suerteTotal]

maquinita ::Juego
maquinita  = UnJuego "maquinita" (+jackpot) [(>95).suerteTotal, tieneObjeto "paciencia".factores]

dineroQueGana :: (Float->Float)->Float->Float
dineroQueGana funcion  = funcion 

----------------------------- Punto 3 -----------------------------
puedeGanar :: Persona->Juego->Bool
puedeGanar persona = all (==True).map persona.criterios

----------------------------- Punto 4 -----------------------------
juega :: Float->Juego->Float
juega apuesta juego = dineroQueGana (dineroAGanar juego) apuesta

cuantoDineroConsigue :: Persona->Float->[Juego]->Float
cuantoDineroConsigue persona apuesta = sum.map (juega apuesta).filter (puedeGanar persona)

{-
cuantoDineroConsigue :: Persona->Float->[Juego]->Float
cuantoDineroConsigue _ _ [] = []
cuantoDineroConsigue persona apuesta (x:xs) |puedeGanar persona x = sum(juega x apuesta :cuantoDineroConsigue persona apuesta xs)
-}

----------------------------- Punto 5 -----------------------------
noGananNinguno :: [Persona]->[Juego]->[Persona]
noGananNinguno listaJugador listaJuego = filter (n1 listaJuego) listaJugador--tomo un jg y se lo paso a la siguiente para q devuelva
--bool.a los que no ganan los toma y los devuelve

n1:: [Juego]->Persona->Bool
n1 listaJuego persona = (all (==False).map (puedeGanar persona)) listaJuego
----------------------------- Punto 6 -----------------------------
apuesta :: Persona->Float->Juego->Persona
apuesta persona apuesta juego |puedeGanar persona juego = persona{dinero = (dinero persona)+(juega apuesta juego)}
                              |otherwise = persona{dinero = (dinero persona) - apuesta}

----------------------------- Punto 7 -----------------------------
-- elCocoEstaEnLaCasa :: (a,[Int])->(Int->Int)->(Int->Bool)->Bool