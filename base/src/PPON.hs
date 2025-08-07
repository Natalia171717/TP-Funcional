module PPON where

import Documento

data PPON
  = TextoPP String
  | IntPP Int
  | ObjetoPP [(String, PPON)]
  deriving (Eq, Show)

pponAtomico :: PPON -> Bool
pponAtomico p = case p of
          ObjetoPP _ -> False 
          _          -> True

pponObjetoSimple :: PPON -> Bool
pponObjetoSimple p = case p of
                    ObjetoPP xs -> all (\(_,t2) -> pponAtomico t2) xs
                    _           -> False

intercalar :: Doc -> [Doc] -> Doc
intercalar sep xs = if null xs then vacio else foldr1 (\x rec -> x <+> sep <+> rec) xs

entreLlaves :: [Doc] -> Doc
entreLlaves [] = texto "{ }"
entreLlaves ds =
  texto "{"
    <+> indentar
      2
      ( linea
          <+> intercalar (texto "," <+> linea) ds
      )
    <+> linea
    <+> texto "}"

aplanar :: Doc -> Doc
aplanar = foldDoc vacio (\s d -> texto s <+> d) (\_ rec -> texto " " <+> rec)

{-

Dado un PPON, la función pponADoc toma dicho PPON y dependiendo si es un PPON atómico o no, lo convierte a Doc de una manera u otra.
Si el PPON representa un texto o un número (casos atómicos), los convierte directamente a Doc con texto (show s) o texto (show n).
Si el PPON es un ObjetoPP, se vuelve a hacer una distinción pero ésta vez entre objetos simples o complejos. Los objetos simples
se mostrarán de forma compacta usando la función aplanar, mientras que los objetos que no lo sean se mostrarán más expandidos.
En ambos casos, se recorre la lista de tuplas (clave, valor) del objeto, aplicando recursivamente pponADoc sobre cada valor.

Tipo de recursión usada en pponADoc:

La función utiliza map, donde se realiza recursión estructural porque solo estaríamos accediendo a los argumentos no recursivos de los 
constructores, y a los resultados de la recursión para las subestructuras (argumentos recursivos). Sin embargo, cuando el PPON es un ObjetoPP,
usamos la función pponObjetoSimple, en la cual accedemos a las subestructuras de PPON para ver si cada subestructura es atómica 
(no accedemos sólo a los resultados de la recursión, si no que accedemos a la subestructura como tal). 
Por lo tanto, podemos concluir que la recursión es primitiva.

-}

pponADoc :: PPON -> Doc
pponADoc p = case p of 
            TextoPP s   -> texto (show s)
            IntPP n     -> texto (show n)
            ObjetoPP xs -> if pponObjetoSimple p then aplanar (entreLlaves (convertirADoc xs))
                                                 else entreLlaves (convertirADoc xs)
        where
          convertirADoc = map (\(s,p') -> texto (show s) <+> texto ": " <+> pponADoc p')
