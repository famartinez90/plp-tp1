-- Para correr los tests:
-- ghc Tests-alu.hs && ./Tests-alu

import Tp
import Test.HUnit
import Data.List

-- evaluar main para correr todos los tests
main = runTestTT allTests

allTests = test [
	"split" ~: testsSplit,
	"cuentas" ~: testsCuentas
	]

testsSplit = test [
	split ',' "PLP" ~?= ["PLP"],
	split ',' " ,PLP, " ~?= [" ","PLP"," "],
	split ',' "hola,PLP" ~?= ["hola","PLP"]
	]

testsCuentas = test [
	cuentas ["x","x","y","x","z"] ~?= [(3,"x"), (1,"y"), (1,"z")],
	cuentas ["x", "y", "z", "a", "b"] ~?= [(1,"x"), (1,"y"), (1,"z"), (1,"a"), (1,"b")],
	cuentas ["x", "x", "x", "x"] ~?= [(4,"x")],
	cuentas ["y"] ~?= [(1,"y")]
	]