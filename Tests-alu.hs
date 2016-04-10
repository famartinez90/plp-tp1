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
 "distCoseno" ~: testDistCoseno
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

