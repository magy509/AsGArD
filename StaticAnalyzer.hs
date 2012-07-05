--  Module StaticAnalyzer
--  Universidad Simón Bolívar
--  Laboratorio de Traductores e Interpretadores (CI-3725)
--  Proyecto 3 - Análisis de Contexto e Intérprete
--  Desarrollado por: Christian Chomiak     05-38034
--                    Maria Gracia Hidalgo  03-36048

module StaticAnalyzer ()
       where

import Control.Monad
import Control.Monad.Instances
import Control.Monad.State
import qualified Data.List as List
import qualified Data.Map as Map
import Language.AsGArD.Parser.AST
import Language.AsGArD.Lexer.Token
import SymTable
import System.IO


{-checkDec::[Declaracion]->SymTable->Bool
checkDec ((Declaracion (x: xs) tipo):ds) st
    =  if isMember nombre st then
        if xs /= [] then
           checkDec [Declaracion xs tipo] st
        else
           if ds /= [] then
              checkDec ds st
        else
              True
      else
           error $ "Variable "++ show x ++" no declarada"
      where
        nombre = show x
  
  -}
  
checkDec::Declaracion->SymTable->Bool
checkDec (Declaracion (x:xs) tipo) st
    = if isMember nombre st then
         if xs == [] then
            True
         else
            checkDec (Declaracion xs tipo) st
      else
         error $ "Variable " ++ show x ++ " no declarada."
      where
         nombre = show x
  
checkEst::Token -> SymTable -> Declaracion -> Bool
checkEst nombre st (Declaracion listanombres tipo)
    = if (elem nombre listanombres && tipo == dtipo) then
        True
      else
         error $ "Error de tipo."
    where
        snombre = show nombre
        Just (SymDec _ dtipo) =  find snombre st
        
checkAsig::Token -> SymTable -> Instruccion -> Bool
checkAsig nombre st (InstrAsignacion identificador _)
    = if nombre == identificador then
        True
      else
        error $ "Variable no inicializada."
      where
        snombre = show nombre
        Just (SymInstr _) = find snombre st
        
getTypeOper::Oper->Tipo
getTypeOper oper = case oper of
                 Suma           -> TInteger
                 Resta          -> TInteger
                 Mult            -> TInteger
                 Division       -> TInteger
                 Modulo         -> TInteger
                 Mayor          -> TInteger
                 Menor          -> TInteger
                 Mayori         -> TInteger
                 Menori         -> TInteger
                 Conj           -> TBoolean
                 Disj           -> TBoolean
                 Negacion       -> TBoolean
                 Igual          -> TBoolean
                 Desigual       -> TBoolean
                 Hconcat        -> TCanvas
                 Vconcat        -> TCanvas
                 Rotacion       -> TCanvas
                 Trasposicion   -> TCanvas
                 
     
getTypeDec :: SymTable -> Symbol -> Tipo
getTypeDec st (SymDec [token] tipo)
    = tipo
     
getTypeExpr :: SymTable -> Exp -> Tipo
getTypeExpr st (ExpBinaria exp1 oper exp2) 
    = if getTypeExpr st exp1 /= getTypeOper oper || getTypeExpr st exp2 /= getTypeOper oper then
        error "Error de tipo"
      else 
        getTypeOper oper