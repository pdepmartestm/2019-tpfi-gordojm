import Data.List  

data Pirata = UnPirata{
nombre :: String,
botin :: [Tesoro]
} deriving(Eq)

type Tesoro = (String, Int)

data Barco = UnBarco{
nombreBarco :: String,
tripulacion :: [Pirata]
} deriving(Eq)

cantidadTesoros::Pirata->Int
cantidadTesoros pirata = length (botin pirata)

valorBotin :: Pirata->Int
valorBotin pirata = sum (map snd (botin pirata))

valoresBotin :: Pirata->[Int]
valoresBotin pirata = map snd (botin pirata)

esAfortunado::Pirata->Bool
esAfortunado pirata = (valorBotin pirata) > 1000

--mismoTesoro pirata1 pirata2 tesoro = elem Tesoro pirata1.botin && elem tesoro pirata1.botin
--mismoDistinto pirata1 pirata2 tesoro = mismoTesoro pirata1 pirata2 tesoro && (snd )



tesoroMasValioso::Pirata->Int
tesoroMasValioso pirata = maximum (valoresBotin pirata)

adquirirTesoro::Pirata->Tesoro->Pirata
adquirirTesoro pirata tesoro = pirata {botin = botin pirata ++ [tesoro]}

esValioso::Tesoro->Bool
esValioso tesoro = (snd tesoro) > 100

perderTodosLosTesorosValiosos :: Pirata->Pirata
perderTodosLosTesorosValiosos pirata = pirata {botin = filter(not.esValioso) (botin pirata)}

esIgualA::String->String->Bool
esIgualA nombre1 nombre2 = nombre1==nombre2

tieneDeNombre::String->Tesoro ->Bool
tieneDeNombre nombre tesoro = (esIgualA nombre (fst tesoro))

perderTodosLosTesorosSegunNombre::Pirata->String->Pirata
perderTodosLosTesorosSegunNombre pirata nombre = pirata {botin = filter(not.tieneDeNombre nombre) (botin pirata)}

jackSparrow = UnPirata{
nombre = "Jack Sparrow",
botin = [("brujula",10000), ("frasco de arena",0)]
}

davidJones = UnPirata{
nombre = "David Jones",
botin=[("cajita musical",1)]
}

anneBonny = UnPirata{
nombre="Anne Bonny",
botin=[("doblones",100),("otro frasco de arena",1)]
}

elizabethSwann = UnPirata{
nombre = "Elizabeth Swann",
botin = [("moneda del cofre muerto",100),("espada de hierro",50)]
}

willTurner = UnPirata{
nombre = "Will Turner",
botin = [("cuchillo",5)]
}

----------------
soloTesorosValiosos :: Pirata->Tesoro->Pirata
soloTesorosValiosos pirata tesoro |esValioso tesoro = adquirirTesoro pirata tesoro
								  |otherwise = pirata


--tesorosConNombreEspecifico pirata tesoro | tieneDeNombre (nombreTesoro pirata) tesoro = adquirirTesoro pirata tesoro
-- | otherwise = id

tieneCorazon pirata tesoro = pirata

--cumpleAlguna | soloTesorosValiosos || tesorosConNombreEspecifico = adquirirTesoro pirata tesoros
-- | otherwise = id

--Saquear

saquear formaDeSaqueo pirata tesoro = formaDeSaqueo pirata tesoro

--3

incorporarPirata :: Barco->Pirata ->Barco
incorporarPirata barco pirata = barco {tripulacion = tripulacion barco ++ [pirata]}

sacarPirata :: Barco->Pirata ->Barco
sacarPirata barco pirata = barco {tripulacion = delete pirata (tripulacion barco)}

-- anclarEnIsla::Barco->String->[Pirata]
-- anclarEnIsla barco isla = map (adquirirTesoro tesoro) (tripulacion barco)

--Ataque

--atacarUnaCiudad barco tesoros | lenght tesoros > lenght (tripulacion barco) = agregarTesorosAPiratas barco tesoros
-- | otherwise = (tirarPiratas tesoros).(agregarTesorosAPiratas barco) tesoros

-- agregarTesorosAPiratas barco tesoros = zipWith (saqueo barco) (tripulacion barco) tesoros

-- saqueo barco tupla = saquear(formaDeSaqueo barco) (fst tupla) (snd tupla)

-- tirarPiratas barco tesoros = take (lenght tesoros) (barco tripulacion)

-- abordarOtroBarco barco = barco {tripulacion = []}

perlaNegra = UnBarco{
nombreBarco = "Perla Negra",
tripulacion = [jackSparrow,anneBonny]
}

holandesErrante = UnBarco {
nombreBarco = "Holandés Errante",
tripulacion = [davidJones]
}
