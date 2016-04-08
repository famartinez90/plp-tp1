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
distEuclideana = \xs ys -> sqrt (sum (zipWith restaYCuadrado xs ys))

restaYCuadrado :: Float -> Float -> Float
restaYCuadrado f1 f2 = (f1-f2)*(f1-f2)

-- Ejercicio 8.2
distCoseno :: Medida
distCoseno = \xs ys -> (productoVectorial xs ys) / ((norma xs) * (norma ys))

productoVectorial :: Instancia -> Instancia -> Float
productoVectorial xs ys = sum (zipWith (*) xs ys)

norma:: [Float] -> Float
norma xs = sqrt (productoVectorial xs xs)
-- Ejercicio 9
knn :: Int -> Datos -> [Etiqueta] -> Medida -> Modelo
knn n datos listEti  med = \inst ->  snd (last(sort(cuentas[snd(y) | y <- (take n (sort(zip (map (med inst) datos) listEti) ))])))
-- 

-- Ejercicio 11 
accuracy :: [Etiqueta] -> [Etiqueta] -> Float
accuracy xs ys = (genericLength([y|y<-(zip xs ys),fst(y)==snd(y) ]))/(genericLength(zip xs ys))   

-- Ejercicio 10
separarDatos :: Datos -> [Etiqueta] -> Int -> Int -> (Datos, Datos, [Etiqueta], [Etiqueta])
separarDatos xs ys n p = ( (part1 xs n p) ++ (part3 xs n p) ,
							(part2 xs n p)					,
						   (part1 ys n p) ++ (part3 ys n p) ,
							(part2 ys n p)
							)

--cantXPart: devuelve la cantidad de elementos que habría en cada partición si se divide la lista em "n" partes iguales
--sin contar los elementos que sobren
cantXPart :: [a] -> Int -> Int
cantXPart xs n = div (length xs) n

--podarLista: Dada una Lista y una cantidad "n" de particiones a la lista, 
--devuelve la lista ignorando los elementos que sobren de esa partición 
podarLista:: [a] -> Int -> [a]
podarLista xs n = take ((cantXPart xs n)*n) xs

--devuelve desde el comienzo de la lista hasta justo antes de comenzar la partición "p"
part1:: [a] -> Int -> Int -> [a]
part1 xs n p = take	 ((p-1)*(cantXPart xs n))	xs 

--devuelve la partición "p" de la lista
part2:: [a] -> Int -> Int -> [a]
part2 xs n p = drop 	((p-1)*(cantXPart xs n))	(take	 (p*(cantXPart xs n))	xs)

--devuelve la lista desde el elemento después de la partición p hasta la última partición valida
--(no toma en cuenta los elementos que sobran)
part3:: [a] -> Int -> Int -> [a]
part3 xs n p = drop	 (p*(cantXPart xs n))	(podarLista xs n) 

-- Ejercicio 12
nFoldCrossValidation :: Int -> Datos -> [Etiqueta] -> Float
nFoldCrossValidation = undefined
