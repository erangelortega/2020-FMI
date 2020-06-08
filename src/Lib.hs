module Lib where
import Text.Show.Functions

laVerdad = True

-- PUNTO 1 --
{-Sabemos de cada país el "ingreso per cápita" que es el promedio de lo que cada habitante necesita para subsistir, 
también conocemos la población activa en el sector público y la activa en el sector privado, la lista de recursos naturales 
(ej: "Minería", "Petróleo", "Industria pesada") y la deuda que mantiene con el FMI. -}

-- a) Representar el TAD País.

type Dinero = Float
type PuestosDeTrabajo = Float
type RecursoNatural = String

data Pais = UnPais {
    ingresoPerCapita :: Float,
    poblacionSectorPublico :: Float,
    poblacionSectorPrivado :: Float,
    recursosNaturales :: [String],
    deudaFMI :: Float
} deriving (Show, Eq)

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

-- FUNCIONES QUE MODIFICAN AL PAIS

modificarIPC :: Float -> Pais -> Pais
modificarIPC porcentaje pais = pais {ingresoPerCapita = ingresoPerCapita pais * (100 - porcentaje) / 100}

reducirPuestosSP :: PuestosDeTrabajo -> Pais -> Pais
reducirPuestosSP puestosReducir pais = pais {poblacionSectorPublico = poblacionSectorPublico pais - puestosReducir }

actualizarRecursos :: RecursoNatural -> Pais -> Pais
actualizarRecursos recurso pais = pais { recursosNaturales = filter (/= recurso) . recursosNaturales $ pais}

actualizarDeuda :: (Dinero -> Dinero -> Dinero) -> Dinero -> Pais -> Pais
actualizarDeuda funcion monto pais = pais {deudaFMI = funcion (deudaFMI pais ) monto }




    {- a) prestarle n millones de dólares al país, esto provoca que el país se endeude en un 150% de lo que el FMI le presta (por los intereses)-}
type Estrategia = Pais -> Pais

prestamoDirecto :: Float -> Estrategia
prestamoDirecto cantidadAprestar = actualizarDeuda (+) (cantidadAprestar * 150 / 100)


    {- b) reducir x cantidad de puestos de trabajo del sector público, lo que provoca que se reduzca la cantidad de activos en el sector público 
    y además que el ingreso per cápita disminuya en 20% si los puestos de trabajo son más de 100 ó 15% en caso contrario -}

reduccion :: PuestosDeTrabajo -> Estrategia
reduccion puestosReducir pais
    | puestosReducir > 100 = modificarIPC 20 . reducirPuestosSP puestosReducir $ pais
    | otherwise = modificarIPC 15 . reducirPuestosSP puestosReducir $ pais 


    {-c) darle a una empresa afín al FMI la explotación de alguno de los recursos naturales, esto disminuye 2 millones de dólares la deuda que el país mantiene 
    con el FMI pero también deja momentáneamente sin recurso natural a dicho país. No considerar qué pasa si el país no tiene dicho recurso.-}

cederAEmpresaDelFMI :: RecursoNatural -> Estrategia
cederAEmpresaDelFMI recurso = actualizarRecursos recurso .actualizarDeuda (-) 2 


    {-establecer un “blindaje”, lo que provoca prestarle a dicho país la mitad de su Producto Bruto Interno (que se calcula como el ingreso per cápita multiplicado 
    por su población activa, sumando puestos públicos y privados de trabajo) y reducir 500 puestos de trabajo del sector público. Evitar la repetición de código.-}

blindaje :: Estrategia
blindaje pais = reducirPuestosSP 500 . actualizarDeuda (+) (pbi pais / 2) $ pais

pbi :: Pais -> Float
pbi pais = (/ 1000000) .(* ingresoPerCapita pais) .  (+ poblacionSectorPrivado pais) . poblacionSectorPublico $ pais


-- PUNTO 3 --
    {-Modelar una receta que consista en prestar 200 millones, y darle a una empresa X la explotación de la “Minería” de un país.-}

type Receta = Pais -> Pais

recetaPrestamoDirectoRecursoCedido :: Receta
recetaPrestamoDirectoRecursoCedido  = cederAEmpresaDelFMI "mineria" . prestamoDirecto 200

{-Ahora queremos aplicar la receta del punto 3.a al país Namibia (creado en el punto 1.b). Justificar cómo se logra el efecto colateral.
    *Lib> recetaPrestamoDirectoRecursoCedido namibia
    UnPais {ingresoPerCapita = 4140.0, poblacionSectorPublico = 400000.0, poblacionSectorPrivado = 650000.0, recursosNaturales = ["ecoturismo"], deudaFMI = 348.0}

    *Lib> namibia
    UnPais {ingresoPerCapita = 4140.0, poblacionSectorPublico = 400000.0, poblacionSectorPrivado = 650000.0, recursosNaturales = ["mineria","ecoturismo"], deudaFMI = 50.0}

    no hay efecto colateral que en en funcional no se modifican los datos originales, por lo tanto cada vez que se aplica una funcion se crea un pais equivalente al original 
    no las modificaciones propias de la funcion
-}

-- PUNTO 4 --
    {- a) Dada una lista de países conocer cuáles son los que pueden zafar, aquellos que tienen "Petróleo" entre sus riquezas naturales.-}

paisesQueZafan :: [Pais] -> [Pais]
paisesQueZafan = filter (elem "petroleo" . recursosNaturales) {-uso de composicion aplicacion parcial y orden superior al filter le paso otra funcion que retonra un Bool-}

{- *Lib> paisesQueZafan [namibia, UnPais 300 1000 2000 ["petroleo", "mineria"] 10, UnPais 5000 5000 5000 ["ecoturismo", "mineria", "petroleo"] 30]
        [UnPais {ingresoPerCapita = 300.0, poblacionSectorPublico = 1000.0, poblacionSectorPrivado = 2000.0, recursosNaturales = ["petroleo","mineria"], deudaFMI = 10.0},UnPais {ingresoPerCapita = 5000.0, poblacionSectorPublico = 5000.0, poblacionSectorPrivado = 5000.0, recursosNaturales = ["ecoturismo","mineria","petroleo"], deudaFMI = 30.0}]
        *Lib> paisesQueZafan [namibia]
        []
    -}


-- b) Dada una lista de países, saber el total de deuda que el FMI tiene a su favor.
deudaAfavorDelFMI :: [Pais] -> Dinero
deudaAfavorDelFMI = foldl (\ acc pais -> acc + deudaFMI pais)  0 


-- PUNTO 5 --
{-Debe resolver este punto con recursividad: dado un país y una lista de recetas, saber si la lista de recetas está ordenada de “peor” a “mejor”, 
en base al siguiente criterio: si aplicamos una a una cada receta, el PBI del país va de menor a mayor. Recordamos que el Producto Bruto Interno surge de 
multiplicar el ingreso per cápita por la población activa (privada y pública). 
-}

ordenadaPorPBI :: Pais -> [Receta] -> Bool
ordenadaPorPBI _ [] = True
ordenadaPorPBI _ [x] = True
ordenadaPorPBI pais (x:y:xs) = pbi (x pais) > pbi (y pais) && ordenadaPorPBI pais xs 


-- PUNTO 6 --
{-(1 punto) Si un país tiene infinitos recursos naturales, modelado con esta función
        recursosNaturalesInfinitos :: [String]
        recursosNaturalesInfinitos = "Energia" : recursosNaturalesInfinitos

    a) ¿qué sucede si evaluamos la función 4a con ese país?
    nunca termina la evaluacion, ya que siempre estará buscando en la lista de rescursos esperando encontrar "petroleo", infinitamente o hasta que consuma los 
    recursos de la compu

    b) ¿y con la 4b?
    como en la lista solo habrá un país retorna el valor de aplicar la funcion, no ingresa a la lista infinita de recursos del data
-}