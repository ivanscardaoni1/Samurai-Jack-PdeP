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

mandarAlAnio :: Number -> Personaje -> Personaje
mandarAlAnio anio personaje = personaje {anioPresente = anio}

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

aplicarVeces :: Number -> (a -> a) -> a -> a
aplicarVeces 0 funcion valor = valor
aplicarVeces cantidadDeVeces funcion valor = foldl (\valor funcion -> funcion valor) valor (replicar cantidadDeVeces funcion)

replicar :: Number -> a -> [a]
replicar 0 _ = []
replicar cantidadDeVeces valor = map (\numero -> valor) [1..cantidadDeVeces]

concentracion :: Number -> Elemento
concentracion nivel = UnElemento "Magia" (modificarSalud (+0)) (aplicarVeces nivel meditar)

esbirrosMalvados :: Number -> [Elemento]
esbirrosMalvados cantidad = replicar cantidad (UnElemento "Maldad" (modificarSalud (+ (-1))) (modificarSalud (+0)))

katanaMagica :: Elemento
katanaMagica = UnElemento "Magia" (modificarSalud (+ (-1000))) (modificarSalud (+0)) 
espadaMaldita :: Elemento
espadaMaldita = UnElemento "Maldad" (modificarSalud (+ (-8))) (modificarSalud (+7))
rayoLaser :: Elemento
rayoLaser = UnElemento "Tecnologia Avanzada" (modificarSalud (+ (-20))) (modificarSalud (+0))

jack :: Personaje
jack = UnPersonaje "Samurai Jack" 1000 [concentracion 3, katanaMagica] 200
aku :: Number -> Number -> Personaje
aku anio salud = UnPersonaje "Aku" salud (concentracion 4 : portalAlFuturo anio : esbirrosMalvados (anio*1)) anio

portalAlFuturo :: Number -> Elemento
portalAlFuturo anio = UnElemento "Magia" (mandarAlAnio (anio + 2800)) (mandarAlAnio (anio + 2800))

atacar :: Personaje -> Personaje -> Personaje
atacar atacado atacante = foldl (\atacado elemento -> (ataque elemento) atacado) atacado (elementos atacante)

defender :: Personaje -> Personaje 
defender atacado = foldl (\atacado elemento -> (defensa elemento) atacado) atacado (elementos atacado)

estaMuerto :: Personaje -> Bool
estaMuerto (UnPersonaje _ 0 _ _) = True
estaMuerto _ = False

luchar :: Personaje -> Personaje -> (Personaje, Personaje) 
luchar atacante defensor
                         | (estaMuerto.atacar defensor) atacante = (atacante,defensor)
                         | otherwise = luchar (atacar defensor atacante) (defender atacante)
                        