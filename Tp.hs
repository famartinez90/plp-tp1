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
cuentas xs = zip (map (\x -> length (filter (==x) xs)) (nub xs)) (nub xs)

-- Ejercicio 4
repeticionesPromedio :: Extractor
repeticionesPromedio = (\t -> mean (map (\x -> fromIntegral (fst x)) (cuentas (split ' ' t))))

-- Ejercicio 5
tokens :: [Char]
tokens = "_,)(*;-=>/.{}\"&:+#[]<|%!\'@?~^$` abcdefghijklmnopqrstuvwxyz0123456789"

frecuenciaTokens :: [Extractor]
frecuenciaTokens = [ (\t -> frecuenciaChar chr t) | chr <- tokens ]

frecuenciaChar :: Eq a => a -> [a] -> Float
frecuenciaChar c xs =  fromIntegral (length (filter (==c) xs)) / fromIntegral (length xs)

normalizarExtractor :: [Texto] -> Extractor -> Extractor
normalizarExtractor = undefined

extraerFeatures :: [Extractor] -> [Texto] -> Datos
extraerFeatures = undefined

distEuclideana :: Medida
distEuclideana = undefined

distCoseno :: Medida
distCoseno = undefined

knn :: Int -> Datos -> [Etiqueta] -> Medida -> Modelo
knn = undefined

accuracy :: [Etiqueta] -> [Etiqueta] -> Float
accuracy = undefined

separarDatos :: Datos -> [Etiqueta] -> Int -> Int -> (Datos, Datos, [Etiqueta], [Etiqueta])
separarDatos = undefined

nFoldCrossValidation :: Int -> Datos -> [Etiqueta] -> Float
nFoldCrossValidation = undefined
