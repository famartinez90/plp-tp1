module Tp where

import Data.List

type Texto = String
type Feature = Float
type Instancia = [Feature]
type Extractor = (Texto -> Feature)

type Datos = [Instancia]
type Etiqueta = String
type Modelo = (Instancia -> Etiqueta)
type Medida = (Instancia -> Instancia -> Float)

tryClassifier :: [Texto] -> [Etiqueta] -> Float
tryClassifier x y = let xs = extraerFeatures ([longitudPromedioPalabras, repeticionesPromedio] ++ frecuenciaTokens) x in
    nFoldCrossValidation 5 xs y

mean :: [Float] -> Float
mean xs = realToFrac (sum xs) / genericLength xs

-- Ejercicio 1
split :: Eq a => a -> [a] -> [[a]]
split = flip tilps

tilps :: Eq a => [a] -> a -> [[a]]
tilps = foldr (\x r -> \n -> if x==n then []:(r n) else ((x:head(r n)):(tail(r n)))) (const[[]])

-- Ejercicio 2
longitudPromedioPalabras :: Extractor
longitudPromedioPalabras = (\t ->  mean (map genericLength (split ' ' t)) )

-- Ejercicio 3
cuentas :: Eq a => [a] -> [(Int, a)]
cuentas = (\xs -> zip (map (\x -> length (filter (==x) xs)) (nub xs)) (nub xs))

-- Ejercicio 4
repeticionesPromedio :: Extractor
repeticionesPromedio = (\t -> mean (map (\x -> fromIntegral (fst x)) (cuentas (split ' ' t))))

-- Ejercicio 5
tokens :: [Char]
tokens = "_,)(*;-=>/.{}\"&:+#[]<|%!\'@?~^$` abcdefghijklmnopqrstuvwxyz0123456789"

frecuenciaTokens :: [Extractor]
frecuenciaTokens = [ (\t -> frecuenciaChar c t) | c <- tokens ]

frecuenciaChar :: Eq a => a -> [a] -> Float
frecuenciaChar c xs =  fromIntegral (length (filter (==c) xs)) / fromIntegral (length xs)

-- Ejercicio 6
normalizarExtractor :: [Texto] -> Extractor -> Extractor
normalizarExtractor = (\ts ex -> (\t -> (ex t) / maximoAbsoluto (aplicarExtractor ts ex)))

aplicarExtractor :: [Texto] -> Extractor -> [Float]
aplicarExtractor = (\ts ex -> map ex ts)

maximoAbsoluto :: (Num a , Ord a) => [a] -> a
maximoAbsoluto = (\xs -> maximum (sort (map abs xs))) 

-- Ejercicio 7
extraerFeatures :: [Extractor] -> [Texto] -> Datos
extraerFeatures = \ex ts -> [crearInstancias ex ts t|t <- ts]

crearInstancias :: [Extractor] -> [Texto] -> Texto -> Instancia
crearInstancias ex ts t = [(normalizarExtractor ts e) t |e <- ex] 

-- Ejercicio 8.1
distEuclideana :: Medida
distEuclideana = undefined

-- Ejercicio 8.2
distCoseno :: Medida
distCoseno = undefined

-- Ejercicio 9
knn :: Int -> Datos -> [Etiqueta] -> Medida -> Modelo
knn = undefined

-- Ejercicio 11
accuracy :: [Etiqueta] -> [Etiqueta] -> Float
accuracy = undefined

-- Ejercicio 10
separarDatos :: Datos -> [Etiqueta] -> Int -> Int -> (Datos, Datos, [Etiqueta], [Etiqueta])
separarDatos = undefined

-- Ejercicio 12
nFoldCrossValidation :: Int -> Datos -> [Etiqueta] -> Float
nFoldCrossValidation = undefined
