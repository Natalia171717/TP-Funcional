module Main (main) where

import Documento
import PPON
import Test.HUnit

main :: IO ()
main = runTestTTAndExit allTests

allTests :: Test
allTests =
  test
    [ "Ejercicio 2" ~: testsEj2,
      "Ejercicio 3" ~: testsEj3,
      "Ejercicio 4" ~: testsEj4,
      "Ejercicio 6" ~: testsEj6,
      "Ejercicio 7" ~: testsEj7,
      "Ejercicio 8" ~: testsEj8,
      "Ejercicio 9" ~: testsEj9,
      "Tests Propios" ~: testsNuestros
    ]

testsEj2 :: Test
testsEj2 =
  test
    [ vacio <+> vacio ~?= vacio,
      texto "a" <+> texto "b" ~?= texto "ab",
      (texto "a" <+> linea) <+> texto "b" ~?= texto "a" <+> (linea <+> texto "b")
    ]

testsEj3 :: Test
testsEj3 =
  test
    [ indentar 2 vacio ~?= vacio,
      indentar 2 (texto "a") ~?= texto "a",
      indentar 2 (texto "a" <+> linea <+> texto "b") ~?= texto "a" <+> indentar 2 (linea <+> texto "b"),
      indentar 2 (linea <+> texto "a") ~?= indentar 1 (indentar 1 (linea <+> texto "a")),
      indentar 2 (indentar 4 linea) ~?= indentar 6 linea,
      indentar 3 vacio ~?= vacio,
      indentar 2 (texto "a" <+> (indentar 4 linea <+> (texto "b" <+> (indentar 1 linea <+> texto "c")))) ~?= texto "a" <+> (indentar 6 linea <+> (texto "b" <+> (indentar 3 linea <+> texto "c")))
    ]

testsEj4 :: Test
testsEj4 =
  test
    [ mostrar vacio ~?= "",
      mostrar linea ~?= "\n",
      mostrar (indentar 2 (texto "a" <+> linea <+> texto "b")) ~?= "a\n  b",
      mostrar (texto "abc") ~?=  "abc",
      mostrar (indentar 2 (texto "a" <+> linea <+> texto "b" <+> indentar 3 linea <+> texto "c")) ~?= "a\n  b\n     c"
    ]

pericles, merlina, addams, familias :: PPON
pericles = ObjetoPP [("nombre", TextoPP "Pericles"), ("edad", IntPP 30)]
merlina = ObjetoPP [("nombre", TextoPP "Merlina"), ("edad", IntPP 24)]
addams = ObjetoPP [("0", pericles), ("1", merlina)]
familias = ObjetoPP [("Addams", addams)]

testsEj6 :: Test
testsEj6 =
  test
    [ pponObjetoSimple pericles ~?= True,
      pponObjetoSimple addams ~?= False,
      pponObjetoSimple (TextoPP "hola") ~?= False,
      pponObjetoSimple (IntPP 10) ~?= False
    ]

a, b, c :: Doc
a = texto "a"
b = texto "b"
c = texto "c"

testsEj7 :: Test
testsEj7 =
  test
    [ mostrar (intercalar (texto ", ") []) ~?= "",
      mostrar (intercalar (texto ", ") [a, b, c]) ~?= "a, b, c",
      mostrar (entreLlaves []) ~?= "{ }",
      mostrar (entreLlaves [a, b, c]) ~?= "{\n  a,\n  b,\n  c\n}"
    ]

testsEj8 :: Test
testsEj8 =
  test
    [ mostrar (aplanar (a <+> linea <+> b <+> linea <+> c)) ~?= "a b c",
      mostrar (aplanar a <+> b) ~?="ab",
      mostrar (aplanar (linea <+> indentar 5 linea)) ~?="  ",
      mostrar (aplanar linea <+> indentar 5 linea) ~?=" \n     ",
      mostrar (aplanar (linea <+> texto "hola" <+> indentar 5 linea)) ~?=" hola "
    ]

testsEj9 :: Test
testsEj9 =
  test
    [ mostrar (pponADoc pericles) ~?= "{ \"nombre\": \"Pericles\", \"edad\": 30 }",
      mostrar (pponADoc addams) ~?= "{\n  \"0\": { \"nombre\": \"Pericles\", \"edad\": 30 },\n  \"1\": { \"nombre\": \"Merlina\", \"edad\": 24 }\n}",
      mostrar (pponADoc familias) ~?= "{\n  \"Addams\": {\n    \"0\": { \"nombre\": \"Pericles\", \"edad\": 30 },\n    \"1\": { \"nombre\": \"Merlina\", \"edad\": 24 }\n  }\n}"
    ]

bart, lisa, maggie, hijosDeHomero, homero, herbert, hijosDeAbe, abraham :: PPON
bart = ObjetoPP [("nombre", TextoPP "Bart"), ("color", TextoPP "amarillo"), ("edad", IntPP 10)]
lisa = ObjetoPP [("nombre", TextoPP "Lisa"), ("color", TextoPP "amarillo"), ("edad", IntPP 8)]
maggie = ObjetoPP [("nombre", TextoPP "Maggie"), ("color", TextoPP "amarillo"), ("edad", IntPP 1)]
hijosDeHomero = ObjetoPP [("0", bart), ("1", lisa), ("2", maggie)]


homero = ObjetoPP [("nombre", TextoPP "Homero"), ("color", TextoPP "amarillo"), ("edad", IntPP 38), ("hijos", hijosDeHomero)]
herbert = ObjetoPP [("nombre", TextoPP "Herbert"), ("color", TextoPP "amarillo"), ("edad", IntPP 42)]
hijosDeAbe = ObjetoPP [("0", herbert),("1", homero)]

abraham = ObjetoPP [("nombre", TextoPP "Abraham"), ("color", TextoPP "amarillo"), ("edad", IntPP 83), ("hijos", hijosDeAbe)]

testsNuestros :: Test
testsNuestros =
  test
    [
      texto "a" <+> vacio ~?= texto "a",
      vacio <+> texto "a" ~?= texto "a",
      texto "a" <+> vacio <+> texto "b" ~?= texto "a" <+> texto "b",
      
      mostrar (linea <+> linea) ~?= "\n\n",
      mostrar (indentar 5 linea <+> indentar 2 linea) ~?= "\n     \n  ",
      mostrar (texto "a" <+> indentar 3 linea <+> texto "b") ~?= "a\n   b",
      mostrar (texto "a" <+> vacio <+> texto "b") ~?= "ab",
      mostrar (texto "a" <+> vacio <+> (linea <+> vacio <+> (linea <+> texto "b"))) ~?= "a\n\nb",

      mostrar (intercalar linea [a, b, c]) ~?= "a\nb\nc",
      mostrar (intercalar (indentar 3 linea) [a, b, c]) ~?= "a\n   b\n   c",
      mostrar (intercalar (texto "PLP") [indentar 5 linea, texto ",", linea]) ~?= "\n     PLP,PLP\n",
      mostrar (intercalar vacio [texto "Un", texto " Vacio", linea, texto "FIN"]) ~?= "Un Vacio\nFIN",

      mostrar (aplanar (texto "a" <+> linea)) ~?= "a ",
      mostrar (aplanar (linea <+> texto "espacio")) ~?= " espacio",
      mostrar (aplanar (texto "c" <+> indentar 44 (texto "a" <+> linea) <+> texto "s" <+> texto "a")) ~?= "ca sa",
      mostrar (aplanar (texto "h" <+> linea <+> texto "o" <+> (indentar 3 (texto "l" <+> vacio) <+> texto "a"))) ~?= "h ola",
      mostrar (aplanar (texto "")) ~?= "",
      mostrar (aplanar (linea <+> vacio <+> texto "mia" <+> linea <+> linea <+> texto "seb" <+> linea)) ~?= " mia  seb ",

      mostrar (pponADoc abraham) ~?= "{\n  \"nombre\": \"Abraham\",\n  \"color\": \"amarillo\",\n  \"edad\": 83,\n  \"hijos\": {\n    \"0\": { \"nombre\": \"Herbert\", \"color\": \"amarillo\", \"edad\": 42 },\n    \"1\": {\n      \"nombre\": \"Homero\",\n      \"color\": \"amarillo\",\n      \"edad\": 38,\n      \"hijos\": {\n        \"0\": { \"nombre\": \"Bart\", \"color\": \"amarillo\", \"edad\": 10 },\n        \"1\": { \"nombre\": \"Lisa\", \"color\": \"amarillo\", \"edad\": 8 },\n        \"2\": { \"nombre\": \"Maggie\", \"color\": \"amarillo\", \"edad\": 1 }\n      }\n    }\n  }\n}"
    
    ]
