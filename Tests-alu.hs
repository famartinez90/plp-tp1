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
 "normalizarExtractor" ~: testNormalizarExtractor
 ]

testsSplit = test [
 split ',' "PLP" ~?= ["PLP"],
 split ',' " ,PLP, " ~?= [" ","PLP"," "],
 split ',' "hola,PLP" ~?= ["hola","PLP"]
 ]

testLongitudPromedioPalabras = test [
 longitudPromedioPalabras "Este test tiene palabras $$++$$" ~?= 5.4,
 longitudPromedioPalabras "test test test" ~?= 4,
 longitudPromedioPalabras "a bc def ghij klmno" ~?= 3,
 longitudPromedioPalabras "" ~?= 0
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
 repeticionesPromedio "lalala $$++$$ lalala lalala $$++$$ a b" ~?= 1.75,
 repeticionesPromedio "" ~?= 1
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