-- Para correr los tests:
-- ghc Tests-alu.hs && ./Tests-alu

import Tp
import Test.HUnit
import Data.List

-- evaluar main para correr todos los tests
main = runTestTT allTests

allTests = test [
 "split" ~: testsSplit,
 "cuentas" ~: testsCuentas,
 "longitudPromedioPalabras" ~: testLongitudPromedioPalabras,
 "repeticionesPromedio" ~: testRepeticionesPromedio,
 "frecuenciaTokens" ~: testFrecuenciaTokens,
 "normalizarExtractor" ~: testNormalizarExtractor,
 "extraerFeatures" ~: testExtraerFeatures,
 "distEuclideana" ~: testDistEuclideana,
 "distCoseno" ~: testDistCoseno,
 "knn" ~: testKnn,
 "separarDatos" ~: testSepararDatos
 ]

testsSplit = test [
 split ',' "PLP" ~?= ["PLP"],
 split ',' " ,PLP, " ~?= [" ","PLP"," "],
 split ',' "hola,PLP" ~?= ["hola","PLP"]
 ]

testLongitudPromedioPalabras = test [
 longitudPromedioPalabras "Este test tiene palabras $$++$$" ~?= 5.4,
 longitudPromedioPalabras "test test test" ~?= 4,
 longitudPromedioPalabras "a bc def ghij klmno" ~?= 3
 ]

testsCuentas = test [
 cuentas ["x","x","y","x","z"] ~?= [(3,"x"), (1,"y"), (1,"z")],
 cuentas ["x", "y", "z", "a", "b"] ~?= [(1,"x"), (1,"y"), (1,"z"), (1,"a"), (1,"b")],
 cuentas ["x", "x", "x", "x"] ~?= [(4,"x")],
 cuentas ["y"] ~?= [(1,"y")]
 ]

testRepeticionesPromedio = test [
 repeticionesPromedio "lalala $$++$$ lalala lalala $$++$$" ~?= 2.5,
 repeticionesPromedio "lalala lalala lalala" ~?= 3,
 repeticionesPromedio "lalala $$++$$ lalala lalala $$++$$ a b" ~?= 1.75
 ]

testFrecuenciaTokens = test [
 (head frecuenciaTokens) "use_snake_case !" ~?= 0.125,
 (head (tail frecuenciaTokens)) "use_snake_case !" ~?= 0,
 (head (tail (tail frecuenciaTokens))) ")" ~?= 1,
 (head (tail (tail frecuenciaTokens))) "))))" ~?= 1,
 (head frecuenciaTokens) "use___snake_____case____!_" ~?= 0.5 
 ]

testNormalizarExtractor = test [
 (normalizarExtractor ["use_snake_case !", "use___snake_____case____!_"] (head frecuenciaTokens)) "use_snake_case !" ~?= 0.25,
 (normalizarExtractor ["use_snake_case !", "use___snake_____case____!_"] (head frecuenciaTokens)) "use___snake_____case____!_" ~?= 1,
 (normalizarExtractor ["lalala $$++$$ lalala lalala $$++$$", "lalala lalala lalala", "lalala $$++$$ lalala lalala $$++$$ a b"] repeticionesPromedio) "lalala lalala lalala" ~?= 1,
 (normalizarExtractor ["lalala $$++$$ lalala lalala $$++$$", "lalala lalala lalala", "lalala $$++$$ lalala lalala $$++$$ a b"] repeticionesPromedio) "lalala $$++$$ lalala lalala $$++$$" ~?= 0.8333333,
 (normalizarExtractor ["lalala $$++$$ lalala lalala $$++$$", "lalala lalala lalala", "lalala $$++$$ lalala lalala $$++$$ a b"] repeticionesPromedio) "lalala $$++$$ lalala lalala $$++$$ a b" ~?= 0.5833333
 ]

testExtraerFeatures = test [
  extraerFeatures [longitudPromedioPalabras, repeticionesPromedio] ["b=a", "a = 2; a = 4", "C:/DOS C:/DOS/RUN RUN/DOS/RUN"] ~?= [[0.33333334,0.6666667],[0.12962963,1.0],[1.0,0.6666667]],
  extraerFeatures [longitudPromedioPalabras, repeticionesPromedio] ["A B A B A B A B"] ~?= [[1.0,1.0]],
  extraerFeatures [longitudPromedioPalabras, repeticionesPromedio] ["A B A B A B A B", "C"] ~?= [[1.0,1.0], [1.0,0.25]],
  extraerFeatures [longitudPromedioPalabras, repeticionesPromedio] ["ABABABAB", "C"] ~?= [[1.0,1.0], [0.125,1.0]],
  extraerFeatures [longitudPromedioPalabras, (head frecuenciaTokens)] ["use_snake_case !", "lalala $$++$$"] ~?= [[1.0,1.0], [0.8,0.0]],
  extraerFeatures [longitudPromedioPalabras, (head frecuenciaTokens)] ["use_snake_case !", "_____"] ~?= [[1.0,0.125], [0.6666667,1.0]]
 ]

testDistEuclideana = test [
 distEuclideana [1.0,0.75,0.8125] [0.75,1.0,0.5] ~?= 0.47186464,
 distEuclideana [0.75,1.0,0.5] [0.75,1.0,0.5] ~?= 0.0,
 distEuclideana [2.0,2.0,2.0,2.0] [0.0,0.0,0.0,0.0] ~?= 4.0,
 distEuclideana [30.0] [10.0] ~?= 20.0,
 distEuclideana [-30.0] [-10.0] ~?= 20.0
 ]

testDistCoseno = test [
 distCoseno [0,3,4] [0,-3,-4] ~?= -1.0,
 distCoseno [1.0,2.0] [2.0,1.0] ~?= 0.8,
 distCoseno [0.0,2.0] [2.0,0.0] ~?= 0.0,
 distCoseno [30.0] [10.0] ~?= 1.0
 ]

testKnn = test [
 (knn 2 [[0,1],[0,2],[2,1],[1,1],[2,3]] ["i","i","f","f","i"] distEuclideana) [1,1] ~?= "f",
 (knn 2 [[0,1],[0,2],[2,1],[1,1],[2,3]] ["i","i","f","f","i"] distCoseno) [1,1] ~?= "i",
 (knn 5 [[0,1],[0,2],[2,1],[1,1],[2,3]] ["i","i","f","f","i"] distEuclideana) [1,1] ~?= "i",
 (knn 6 [[0,1],[1000,1000],[100,0],[0,100],[100,100],[1,1]] ["i","i","f","f","f","i"] distEuclideana) [1,1] ~?= "i",
 (knn 2 [[0,1],[0,2],[2,1],[1,1],[2,3]] ["i","i","i","i","i"] distEuclideana) [1,1] ~?= "i",
 (knn 2 [[0,1],[1,1]] ["i","f"] distEuclideana) [1,1] ~?= "i",
 (knn 2 [[1,1]] ["f"] distEuclideana) [1,1] ~?= "f"
 ]

xs = [[1,1],[2,2],[3,3],[4,4],[5,5],[6,6],[7,7]] :: Datos
y = ["1","2","3","4","5","6","7"]
(x_train, x_val, y_train, y_val) = separarDatos xs y 3 2
(x_train2, x_val2, y_train2, y_val2) = separarDatos xs y 7 1
(x_train3, x_val3, y_train3, y_val3) = separarDatos xs y 2 1

testSepararDatos = test [
 (x_train, y_train) ~?= ([[1.0,1.0],[2.0,2.0],[5.0,5.0],[6.0,6.0]],["1","2","5","6"]),
 (x_val, y_val) ~?= ([[3.0,3.0],[4.0,4.0]],["3","4"]),
 (x_train2, y_train2) ~?= ([[2.0,2.0],[3.0,3.0],[4.0,4.0],[5.0,5.0],[6.0,6.0],[7.0,7.0]],["2","3","4","5","6","7"]),
 (x_val2, y_val2) ~?= ([[1.0,1.0]],["1"]),
 (x_train3, y_train3) ~?= ([[4.0,4.0],[5.0,5.0],[6.0,6.0]],["4","5","6"]),
 (x_val3, y_val3) ~?= ([[1.0,1.0],[2.0,2.0],[3.0,3.0]],["1","2","3"])
 ]