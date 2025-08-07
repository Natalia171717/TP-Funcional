module Documento
  ( Doc,
    vacio,
    linea,
    texto,
    foldDoc,
    (<+>),
    indentar,
    mostrar,
    imprimir,
  )
where

data Doc
  = Vacio
  | Texto String Doc
  | Linea Int Doc
  deriving (Eq, Show)

vacio :: Doc
vacio = Vacio

linea :: Doc
linea = Linea 0 Vacio

texto :: String -> Doc
texto t | '\n' `elem` t = error "El texto no debe contener saltos de línea"
texto [] = Vacio
texto t = Texto t Vacio

-- El foldDoc hace recursión sobre un documento aplicando su respectiva función dependiendo el constructor 

foldDoc :: b -> (String -> b -> b) -> (Int -> b -> b) -> Doc -> b
foldDoc cVacio cTexto cLinea doc = case doc of
                      Vacio     -> cVacio
                      Texto s d -> cTexto s (rec d)
                      Linea n d -> cLinea n (rec d)
                  where
                      rec = foldDoc cVacio cTexto cLinea

-- NOTA: Se declara `infixr 6 <+>` para que `d1 <+> d2 <+> d3` sea equivalente a `d1 <+> (d2 <+> d3)`
-- También permite que expresiones como `texto "a" <+> linea <+> texto "c"` sean válidas sin la necesidad de usar paréntesis.
infixr 6 <+>

{- 
La función <+> satisface el invariante de Doc.

Dados dos textos d1 y d2, la función <+> crea un nuevo documento que contiene la información de d1 seguida de la de d2.

Utilizando foldDoc, se "recorre" d1 explorando cada posible caso según su constructor, hasta encontrar un Documento Vacio. 
Cuando esto ocurre, el Vacio es reemplazado con d2. La información interna de cada documento no se ve modificada. 
Sabemos que d1 y d2 cumplen el invariante de Doc, ya que el module Documento no exporta los constructores, por lo que ambos documentos
sólo se pueden crear a partir de las funciones que facilita el módulo, las cuales aseguran que se cumpla el invariante.
De esta manera, como no se modifica el contenido de d1 y d2, el documento que se devuelve también cumple el invariante.

Esto funciona y no deja abierta la posibilidad de que se rompa el invariante, salvo en un caso: 
cuando ambos documentos que deseamos concatenar son de tipo Texto, ya que debemos asegurar que dado un Texto s d, el d debe ser Vacio o Linea.

Dicho caso está controlado en la función auxiliar concatTexto, que lo que hace es crear un nuevo Documento de tipo Texto 
con la cadena de texto "s1 ++ s2" (la concatenación no modifica los valores de s1 y s2), donde s1 y s2 son las cadenas de texto
de los documentos a concatenar. Como ya dijimos, ambos Doc cumplen el invariante, por lo que sus strings no son vacios ni contienen
saltos de linea.

-}

(<+>) :: Doc -> Doc -> Doc
d1 <+> d2 = foldDoc d2 concatTexto Linea d1

concatTexto :: String -> Doc -> Doc
concatTexto s rec = case rec of
                Texto s' d  -> Texto (s ++ s') d
                _  -> Texto s rec

concatLinea :: Int -> Doc -> Doc
concatLinea n res = case res of
                Vacio      -> Linea n Vacio
                Texto s' d -> Linea n (Texto s' d)
                Linea n' d -> Linea n (Linea n' d)

{-
La función indentar satisface el invariante de Doc.

Como se mencionó anteriormente, en la explicación del cumplimiento del invariante en la función <+>, el documento que se recibe,
va a cumplir el invariante de Doc (porque no se exportan los constructores).

La función agrega i espacios más después de un salto de linea. Como ese número i que entra como parámetro es mayor a cero por precondición, 
al sumarle n (la cantidad de espacios que teníamos previamente al llamado de la función, que como dijimos, cumple el invariante y por lo tanto,
es mayor o igual a cero), no queda negativo y se sigue cumpliendo el invariante.

Como los textos que forman parte del documento que entra como parámetro cumplen el invariante y no se ven modificados, 
al finalizar el llamado de la función se seguirá cumpliendo el invariante.

-}

indentar :: Int -> Doc -> Doc
indentar i = foldDoc Vacio Texto (\n rec -> Linea (n+i) rec)

mostrar :: Doc -> String
mostrar = foldDoc "" (++) (\n rec ->"\n" ++ replicate n ' ' ++ rec)

-- | Función dada que imprime un documento en pantalla

-- ghci> imprimir (Texto "abc" (Linea 2 (Texto "def" Vacio)))
-- abc
--   def

imprimir :: Doc -> IO ()
imprimir d = putStrLn (mostrar d)
