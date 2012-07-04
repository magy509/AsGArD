--  Module SymSymTable
--  Universidad Simón Bolívar
--  Laboratorio de Traductores e Interpretadores (CI-3725)
--  Proyecto 3 - Análisis de Contexto e Intérprete
--  Desarrollado por: Christian Chomiak     05-38034
--                    Maria Gracia Hidalgo  03-36048

module SymTable(
  SymTable(..),
  Symbol(..),
  Tipo(..),
  Value(..),
  find,
  insert,
  isMember,
  replace)
    where

import qualified Data.Map as Map
import Language.AsGArD.Lexer.Token
import Language.AsGArD.Parser.AST

data SymTable = SymTable (Maybe SymTable) (Map.Map String Symbol)
              deriving (Eq, Show)


data Symbol = SymDec [Token] Tipo --FIXME
            | SymInstr Instruccion
            deriving (Eq, Show)

-- Argumentos que una instruccion puede tener.
data SymArgs = Tipo
             | Token
             | String Tipo
             deriving (Eq, Show)

-- Valores que una variable Symbol puede tomar.
data Value = Numero Integer
              | Booleano Bool
              | Canvas String
              deriving (Eq, Show)
             
{-
 La funcion isMember retorna un booleano si un identificador está contenido en la tabla de símbolos.
-}
isMember :: String -> SymTable -> Bool
isMember a (SymTable Nothing x) = if Map.member a x then True else False
isMember a (SymTable (Just s) x) = if Map.member a x then True else isMember a s

{-
 La funcion find retorna el símbolo asociado a un identificador en la tabla de símbolos. Si no se
 encuentra, nada es retornado.
-}
find :: String -> SymTable -> Maybe Symbol
find a (SymTable Nothing x) = if Map.member a x then Map.lookup a x else Nothing
find a (SymTable (Just s) x) = if Map.member a x then Map.lookup a x else find a s

{-
 La funcion insert, inserta un nuevo símbolo, asociado a un identificador, en la tabla.
-}
insert :: String -> Symbol -> SymTable -> SymTable
insert a b c@(SymTable Nothing x)
  = if Map.member a x
    then
      error $ "La variable o instruccion '"++a++"' fue declarada anteriormente."
    else
      SymTable Nothing (Map.insert a b x)
insert a b c@(SymTable (Just s) x)
  = if Map.member a x
    then
      error $ "La variable o funcion '"++a++"' fue declarada anteriormente."
    else SymTable (Just s) (Map.insert a b x)

{-
 La funcion replace, reemplaza el símbolo asociado con un identificador en la tabla. Si no existe,
 retorna la tabla sin modificaciones. Si existe, retorna la tabla con el símbolo asociado con el
 identificador reemplazado.
-}
replace :: String -> Symbol -> SymTable -> SymTable
replace a b c = if isMember a c then findSpot a b c else c
    where
      findSpot a' b' (SymTable Nothing x) =
          SymTable Nothing (Map.alter f a' x)
              where f _ = Just b'
      findSpot a' b' (SymTable (Just s) x) =
          if Map.member a x
          then SymTable (Just s) (Map.alter f a x)
          else findSpot a' b' s
              where f _ = Just b'