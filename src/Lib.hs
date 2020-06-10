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
incrementoSuerte persona = (suerte persona) * (tomoValorObjeto "amuleto" (factores persona))

tomoValorObjeto :: String->[(String,Int)]->Int
tomoValorObjeto objeto  = soloElValor.head.filter (tieneObjeto objeto)

soloElValor ::(String,Int) ->Int
soloElValor = snd

----------------------------- Punto 2 -----------------------------
data Juego = UnJuego{
    nombreJuego :: String,
    dineroGanado :: Float,
    criterios :: [(Persona->Bool)]
}deriving (Show)

jackpot = undefined -- se deberia definir

ruleta ::Float->Juego
ruleta dineroApostado = UnJuego "ruleta" (dineroApostado* 37) [(>80).suerteTotal]

maquinita ::Float->Juego
maquinita dineroApostado = UnJuego "maquinita" (jackpot + dineroApostado) [(>95).suerteTotal, tieneObjeto "paciencia".factores]

