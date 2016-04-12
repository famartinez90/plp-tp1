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
-- Simplemente cambiamos el orden para usar tilps que usa foldr y lo filtramos para el caso de listas vacias, como se mencionó en clase.
split :: Eq a => a -> [a] -> [[a]]
split = \x xs -> filter (/=[]) (tilps xs x)

-- Para usar foldr hacemos un cambio de parametros y usamos esta funcion tilps.
tilps :: Eq a => [a] -> a -> [[a]]
tilps = foldr (\x r -> \n -> if x==n then []:(r n) else ((x:head(r n)):(tail(r n)))) (const[[]])

-- Ejercicio 2
-- Dado un string separamos todas las palabras que lo componen con split y luego calculamos la longitud de cada una de esas palabras aplicandoles genericLength. Finalmente, devolvemos el promedio de esas longitudes.
longitudPromedioPalabras :: Extractor
longitudPromedioPalabras = (\t ->  mean (map genericLength (split ' ' t)) )

-- Ejercicio 3
-- Calculamos la cantidad de apariciones de cada elemento de la lista aplicando filter sobre la lista comparando contra este elemento y luego calculando la longitud de la lista que se genera con estas apariciones del elemento. Luego, con zip, generamos las tuplas de las aparaciones con los elementos sin repetidos (usando nub).
cuentas :: Eq a => [a] -> [(Int, a)]
cuentas = (\xs -> zip (map (\x -> length (filter (==x) xs)) (nub xs)) (nub xs))

-- Ejercicio 4
-- Dado un String, aplicamos Split para obtener la separacion de las "palabras". Luego aplicamos cuentas para obtener la cantidad de apariciones de cada una. Como aparecen en tuplas usamos map para quedarmos con la lista solamente que contiene la cantidad de apariciones y finalmente usamos mean para obtener el promedio.
repeticionesPromedio :: Extractor
repeticionesPromedio = (\t -> mean (map (\x -> fromIntegral (fst x)) (cuentas (split ' ' t))))

-- Ejercicio 5
tokens :: [Char]
tokens = "_,)(*;-=>/.{}\"&:+#[]<|%!\'@?~^$` abcdefghijklmnopqrstuvwxyz0123456789"

-- Por cada elemento que esta en tokens generamos la lista de funciones que dado un String calcula la frecuencia de ese elemento (que es un char de la lista de tokens) para esto usamos la función auxiliar frecuenciaChar
frecuenciaTokens :: [Extractor]
frecuenciaTokens = [ (\t -> frecuenciaChar c t) | c <- tokens ]

-- Calcula la frecuencia de un elemento en una lista, osea, divide las apariciones sobre el tamaño de la lista
frecuenciaChar :: Eq a => a -> [a] -> Float
frecuenciaChar c xs =  fromIntegral (length (filter (==c) xs)) / fromIntegral (length xs)

-- Ejercicio 6
-- Toma el maximo absoluto de aplicar un extractor a toda la lista de textos y luego lo utiliza para normalizar cada aplicacion del extractor a cada texto de la lista. Devuelve la funcion que dado un texto, le aplica el extractor normalizado previamente.
normalizarExtractor :: [Texto] -> Extractor -> Extractor
normalizarExtractor = (\ts ex -> (\t -> (ex t) / maximoAbsoluto (aplicarExtractor ts ex)))

-- Aplica un extractor a cada texto de la lista de textos
aplicarExtractor :: [Texto] -> Extractor -> [Float]
aplicarExtractor = (\ts ex -> map ex ts)

-- Toma el valor absoluto de los elementos de la lista, la ordena y luego devuelve el maximo de la misma
maximoAbsoluto :: (Num a , Ord a) => [a] -> a
maximoAbsoluto = (\xs -> maximum (sort (map abs xs))) 

-- Ejercicio 7
-- Dada una lista de extractores y textos, retorna las aplicaciones de extractores normalizados sobre cada uno de los textos t de ts
extraerFeatures :: [Extractor] -> [Texto] -> Datos
extraerFeatures = \ex ts -> [crearInstancias ex ts t|t <- ts]

-- Devuelve la lista de las aplicaciones de cada uno de los extractores de lista ex, normalizados usando la lista de textos ts, sobre el texto t 
crearInstancias :: [Extractor] -> [Texto] -> Texto -> Instancia
crearInstancias ex ts t = [(normalizarExtractor ts e) t |e <- ex] 

-- Ejercicio 8.1
-- Este ejercicio es muy mecanico. Aplicamos la funcion de restaYCuadrado sobre cada uno de los elementos de xs con los elementos de ys mediante zipWith. Luego con sum obtenemos la sumatoria de la lista obtenida y a este numero aplicamos sqrt que nos da la raiz cuadrada de esta sumatoria.
distEuclideana :: Medida
distEuclideana = \xs ys -> sqrt (sum (zipWith restaYCuadrado xs ys))

-- Dados dos Float, calcula el cuadrado de la resta entre ambos
restaYCuadrado :: Float -> Float -> Float
restaYCuadrado f1 f2 = (f1-f2)*(f1-f2)

-- Ejercicio 8.2
-- Solamente hacemos el productoVectorial entre dos listas y lo dividimos sobre las normas de cada una de las listas, donde normas es usar el productoVectorial pero con la misma lista y aplicar sqrt. SE RENOMBRÓ de esta manera para que sea aun mas declarativos
distCoseno :: Medida
distCoseno = \xs ys -> (productoVectorial xs ys) / ((norma xs) * (norma ys))

-- Calcula el producto vectorial entre dos listas de Float (Instancias)
productoVectorial :: Instancia -> Instancia -> Float
productoVectorial xs ys = sum (zipWith (*) xs ys)

-- Calcula norma de una lista
norma:: [Float] -> Float
norma xs = sqrt (productoVectorial xs xs)

-- Ejercicio 9
-- Mediante Map aplico la funcion de medida pasada sobre cada uno de los elementos de Datos luego aplico zip con la lista de etiquetas y obtengo tuplas. Ordeno mediante sort (al ser tuplas sort ordena teniendo en cuenta el primer elemento, y el segundo si el primero se repito). Usando take me quedo con n elementos y finalmente me genero una lista con el valor de snd de cada tupla, el cual es un Modelo. A esta lista de Modelos le aplico cuentas para calcular las apariciones de cada uno, ordeno nuevamente y me quedo con la que mas se repite.
knn :: Int -> Datos -> [Etiqueta] -> Medida -> Modelo
knn n datos listEti med = \inst ->  snd (last(sort(cuentas [snd(y) | y <- (take n (sort(zip (map (med inst) datos) listEti) ))])))

-- Ejercicio 11 
-- Por medio de zip genero una lista de tuplas de Etiqueta a partir de 2 listas y me quedo solamente con los que se cumple que ambos elementos de cada lista sean igual. Luego obtengo la longitud de esta lista y la divido por la longitud de la lista sin filtrar. 
accuracy :: [Etiqueta] -> [Etiqueta] -> Float
accuracy xs ys = (genericLength([y |y<-(zip xs ys),fst(y)==snd(y) ]))/(genericLength(zip xs ys))   

-- Ejercicio 10
-- Toma Datos y un conjunto de Etiquetas, los particiona en partes iguales, y luego reune las particiones excepto una que la retorna por separado.  
separarDatos :: Datos -> [Etiqueta] -> Int -> Int -> (Datos, Datos, [Etiqueta], [Etiqueta])
separarDatos xs ys n p = ( (part1 xs n p) ++ (part3 xs n p) ,
							(part2 xs n p)					,
						   (part1 ys n p) ++ (part3 ys n p) ,
							(part2 ys n p)
							)

-- cantXPart: Devuelve la cantidad de elementos que habría en cada partición si se divide la lista em "n" partes iguales sin contar los elementos que sobren
cantXPart :: [a] -> Int -> Int
cantXPart xs n = div (length xs) n

-- podarLista: Dada una Lista y una cantidad "n" de particiones a la lista, devuelve la lista ignorando los elementos que sobren de esa partición 
podarLista:: [a] -> Int -> [a]
podarLista xs n = take ((cantXPart xs n)*n) xs

-- Devuelve desde el comienzo de la lista hasta justo antes de comenzar la partición "p"
part1:: [a] -> Int -> Int -> [a]
part1 xs n p = take	 ((p-1)*(cantXPart xs n))	xs 

-- Devuelve la partición "p" de la lista
part2:: [a] -> Int -> Int -> [a]
part2 xs n p = drop ((p-1)*(cantXPart xs n)) (take (p*(cantXPart xs n))	xs)

-- Devuelve la lista desde el elemento después de la partición p hasta la última partición valida (no toma en cuenta los elementos que sobran)
part3:: [a] -> Int -> Int -> [a]
part3 xs n p = drop	(p*(cantXPart xs n)) (podarLista xs n) 

-- Ejercicio 12
-- Hacer un promedio de los accuracy generados tomando separarDatos donde vario el p para quedarme con una particion distinta cada vez. Luego separarDatos nos devuelve una cuadrupla (d1,d2,eti1,eti2) y con esta uso la funcion prueba sobre la particion obtenida d2 para aplicarle 15-vecinos mas cercarnos con distEuclediana usando d1 y eti1 para los datos de entrenamiento. Al resultado le aplico accuracy contra eti2 y armo la lista de estas accuracys para cada vez que vario la particion p. Finalmente, aplico mean sobre la lista para obetener el promedio. 

nFoldCrossValidation :: Int -> Datos -> [Etiqueta] -> Float
nFoldCrossValidation n datos etiq = mean ([accuracy (prueba d2 15 d1 eti1) eti2 | p<-[1,n], let (d1,d2,eti1,eti2) = (separarDatos datos etiq n p)] ) 

prueba::Datos -> Int -> Datos -> [Etiqueta] -> [Etiqueta]
prueba datap n datos etits = map (knn n datos etits distEuclideana) datap 