module Lib where
import Text.Show.Functions

laVerdad = True

-- PUNTO 1 --
{-Sabemos de cada país el "ingreso per cápita" que es el promedio de lo que cada habitante necesita para subsistir, 
también conocemos la población activa en el sector público y la activa en el sector privado, la lista de recursos naturales 
(ej: "Minería", "Petróleo", "Industria pesada") y la deuda que mantiene con el FMI. -}

-- a) Representar el TAD País.

type Dinero = Float

data Pais = UnPais {
    ingresoPerCapita :: Float,
    poblacionSectorPublico :: Float,
    poblacionSectorPrivado :: Float,
    recursosNaturales :: [String],
    deudaFMI :: Float
} deriving Show

{- b) Dar un ejemplo de cómo generar al país Namibia, cuyo ingreso per cápita es de 4140 u$s, 
    la población activa del sector público es de 400.000, la población activa del sector privado es de 650.000, 
    su riqueza es la minería y el ecoturismo y le debe 50 (millones de u$s) al FMI.-}

namibia = UnPais {
    ingresoPerCapita = 4140, 
    poblacionSectorPublico = 400000, 
    poblacionSectorPrivado = 650000, 
    recursosNaturales = ["mineria", "ecoturismo"], 
    deudaFMI = 50
    }

-- PUNTO 2 --
{-Implementar las ESTRATEGIAS que forman parte de las recetas del FMI. -}

-- a) prestarle n millones de dólares al país, esto provoca que el país se endeude en un 150% de lo que el FMI le presta (por los intereses)
type Estrategia = Pais -> Pais

prestamoDirecto :: Float -> Estrategia
prestamoDirecto cantidadAprestar = cargarDeuda (cantidadAprestar * 150 / 100)

cargarDeuda :: Dinero -> Pais -> Pais
cargarDeuda nuevaDeuda pais = pais {deudaFMI = deudaFMI pais + nuevaDeuda}




