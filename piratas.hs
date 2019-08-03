import Data.List  
import Text.Show.Functions

data Pirata = UnPirata{
nombre :: String,
botin :: [Tesoro]
} deriving(Eq, Show)

data Tesoro = UnTesoro{
nombreTesoro :: String,
valor :: Int
} deriving(Eq, Show)

data Barco = UnBarco{
nombreBarco :: String,
tripulacion :: [Pirata],
formaSaqueo :: FormaDeSaqueo
} deriving(Show)

type FormaDeSaqueo = Tesoro -> Bool

cantidadTesoros :: Pirata -> Int
cantidadTesoros pirata = length (botin pirata)

valoresBotin :: Pirata -> [Int]
valoresBotin pirata = map valor (botin pirata)

valorBotin :: Pirata -> Int
valorBotin pirata = sum (valoresBotin pirata)

esAfortunado :: Pirata -> Bool
esAfortunado pirata = (valorBotin pirata) > 10000

tieneTesoro :: Pirata -> Tesoro -> Bool
tieneTesoro pirata tesoro = elem tesoro (botin pirata)

distintoValor :: Tesoro -> Tesoro -> Bool
distintoValor unTesoro otroTesoro = valor unTesoro /= valor otroTesoro 

mismoNombre :: Tesoro -> Tesoro -> Bool
mismoNombre unTesoro otroTesoro = nombreTesoro unTesoro == nombreTesoro otroTesoro

mismoNombreDistintoValor :: Tesoro -> Tesoro -> Bool
mismoNombreDistintoValor unTesoro otroTesoro = distintoValor unTesoro otroTesoro && mismoNombre unTesoro otroTesoro

mismoTesoro::Pirata->Pirata->Tesoro->Bool
mismoTesoro pirata1 pirata2 tesoro = tieneTesoro pirata1 tesoro && tieneTesoro pirata2 tesoro

--mismoDistinto :: Pirata -> Pirata -> Tesoro -> Bool
--mismoDistinto pirata1 pirata2 tesoro = 

tesoroMasValioso::Pirata->Int
tesoroMasValioso pirata = maximum (valoresBotin pirata)

adquirirTesoro::Tesoro->Pirata->Pirata
adquirirTesoro tesoro pirata = pirata {botin = botin pirata ++ [tesoro]}

esValioso::FormaDeSaqueo
esValioso tesoro = valor tesoro > 100

noEsValioso::FormaDeSaqueo
noEsValioso tesoro = not (esValioso tesoro)

perderTodosLosTesorosValiosos :: Pirata->Pirata
perderTodosLosTesorosValiosos pirata = pirata {botin = filter(not.esValioso) (botin pirata)}

esIgualA::String->String->Bool
esIgualA nombre1 nombre2 = nombre1==nombre2

tieneDeNombre::String->Tesoro->Bool
tieneDeNombre nombreBuscado tesoro = nombreBuscado == nombreTesoro tesoro

perderTodosLosTesorosSegunNombre::Pirata->String->Pirata
perderTodosLosTesorosSegunNombre pirata nombre = pirata {botin = filter(not.tieneDeNombre nombre) (botin pirata)}

jackSparrow = UnPirata{
nombre = "Jack Sparrow",
botin = [brujulaQueApunta,frascoDeArenaConValorCero,doblonesDeOro]
}

davyJones = UnPirata{
nombre = "Davy Jones",
botin=[cajitaMusical]
}

anneBonny = UnPirata{
nombre="Anne Bonny",
botin=[doblonesDeOro,frascoDeArenaConValorUno]
}

elizabethSwann = UnPirata{
nombre = "Elizabeth Swann",
botin = [monedaDelCofreDelMuerto,espadaDeHierro]
}

willTurner = UnPirata{
nombre = "Will Turner",
botin = [cuchilloDelPadre]
}

brujulaQueApunta = UnTesoro "Brujula que apunta" 10000
frascoDeArenaConValorCero = UnTesoro "Frasco de arena" 0
cajitaMusical = UnTesoro "Cajita musical" 1
doblonesDeOro = UnTesoro "Doblones de oro" 1000
frascoDeArenaConValorUno = UnTesoro "Frasco de arena" 1
monedaDelCofreDelMuerto = UnTesoro "Moneda del cofre del muerto" 100
espadaDeHierro = UnTesoro "Espada de hierro" 50
cuchilloDelPadre = UnTesoro "Cuchillo del padre" 5
ron = UnTesoro "Ron" 25
monedaDePlata = UnTesoro "Moneda de plata" 15
mapaDeBarbarroja = UnTesoro "Mapa de Barbarroja" 15000
catalejoMagico = UnTesoro "Catalejo Magico" 500
sombreroDeAvestruz = UnTesoro "Sombrero de avestruz" 199

----------------
soloTesorosValiosos :: Pirata->Tesoro->Pirata
soloTesorosValiosos pirata tesoro |esValioso tesoro = adquirirTesoro tesoro pirata
                                  |otherwise = pirata


--tesorosConNombreEspecifico pirata tesoro | tieneDeNombre (nombreTesoro pirata) tesoro = adquirirTesoro pirata tesoro
-- | otherwise = id

tieneCorazon pirata tesoro = pirata

--cumpleAlguna | soloTesorosValiosos || tesorosConNombreEspecifico = adquirirTesoro pirata tesoros
-- | otherwise = id

--Saquear

saquear :: FormaDeSaqueo -> Pirata -> Tesoro -> Pirata
saquear formaDeSaqueo pirata tesoro |formaDeSaqueo tesoro = adquirirTesoro tesoro pirata
                                    |otherwise = pirata

saqueoEspecifico :: String -> FormaDeSaqueo
saqueoEspecifico = tieneDeNombre

saqueoValiosos::FormaDeSaqueo
saqueoValiosos = esValioso

saqueoComplejo :: [FormaDeSaqueo]->FormaDeSaqueo
saqueoComplejo saqueos tesoro = any (verificaTesoro tesoro) saqueos

verificaTesoro:: Tesoro -> FormaDeSaqueo -> Bool
verificaTesoro tesoro saqueo = saqueo tesoro

--3

incorporarPirata :: Barco->Pirata ->Barco
incorporarPirata barco pirata = barco {tripulacion = tripulacion barco ++ [pirata]}

sacarPirata :: Barco->Pirata ->Barco
sacarPirata barco pirata = barco {tripulacion = delete pirata (tripulacion barco)}

type Isla = Tesoro
islaDelRon = ron
islaTortuga = frascoDeArenaConValorUno

anclarEnIsla::Barco->Isla->Barco
anclarEnIsla barco isla = barco {tripulacion = map (adquirirTesoro isla) (tripulacion barco)}

--Ataque

type Ciudad = [Tesoro]

atacarCiudad :: Ciudad -> Barco -> Barco
atacarCiudad ciudad barco = barco {tripulacion = zipWith (saquear (formaSaqueo barco)) ciudad (tripulacion barco)}

-- agregarTesorosAPiratas barco tesoros = zipWith (saqueo barco) (tripulacion barco) tesoros

-- saqueo barco tupla = saquear(formaDeSaqueo barco) (fst tupla) (snd tupla)

-- tirarPiratas barco tesoros = take (lenght tesoros) (barco tripulacion)

-- abordarOtroBarco barco = barco {tripulacion = []}

perlaNegra = UnBarco{
nombreBarco = "Perla Negra",
tripulacion = [jackSparrow,anneBonny],
formaSaqueo = saqueoComplejo [esValioso, saqueoEspecifico "sombrero"]
}

holandesErrante = UnBarco {
nombreBarco = "Holandés Errante",
tripulacion = [davyJones],
formaSaqueo = saqueoEspecifico "cajita"
}

barcos = [perlaNegra, holandesErrante]
