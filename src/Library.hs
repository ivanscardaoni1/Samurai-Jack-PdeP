module Library where
import PdePreludat

doble :: Number -> Number
doble numero = numero + numero

data Elemento = UnElemento { 
                  tipo :: String,
                  ataque :: (Personaje-> Personaje),
                  defensa :: (Personaje-> Personaje) } deriving (Eq, Show)

data Personaje = UnPersonaje { 
                    nombre :: String,
                    salud :: Number,
                    elementos :: [Elemento],
                    anioPresente :: Number } deriving (Eq, Show)

katanaMagica :: Elemento
katanaMagica = UnElemento "Salvador" (modificarSalud (+ (-10))) (modificarSalud (+5)) 
espadaMaldita :: Elemento
espadaMaldita = UnElemento "Maldad" (modificarSalud (+ (-8))) (modificarSalud (+7))
rayoLaser :: Elemento
rayoLaser = UnElemento "Salvador" (modificarSalud (+ (-20))) (modificarSalud (+0))

aku :: Personaje
aku = UnPersonaje "Aku" 20 [espadaMaldita] 2020
jack :: Personaje
jack = UnPersonaje "Samurai Jack" 50 [katanaMagica, rayoLaser] 2025

mandarAlAnio :: Personaje -> Number -> Personaje
mandarAlAnio personaje anio = personaje {anioPresente = anio}

modificarSalud ::(Number -> Number) -> Personaje -> Personaje
modificarSalud funcion personaje = personaje {salud = ((max 0.funcion) (salud personaje))}

meditar :: Personaje -> Personaje
meditar personaje = modificarSalud ((+ salud personaje).(/ 2)) personaje

causarDanio :: Number -> Personaje -> Personaje
causarDanio danio = modificarSalud (+ (-danio))

esMalvado :: Personaje -> Bool
esMalvado = ((any (== "Maldad")).(map tipo).elementos)

danioQueProduce :: Personaje -> Elemento -> Number
danioQueProduce personaje elemento = abs (salud personaje - (salud.ataque elemento) personaje)

esMortal :: Personaje -> Personaje -> Bool
esMortal personaje enemigo = (any (>= salud personaje) . map (danioQueProduce enemigo) . elementos) enemigo

enemigosMortales :: Personaje -> [Personaje] -> [Personaje] 
enemigosMortales personaje = filter (esMortal personaje)



