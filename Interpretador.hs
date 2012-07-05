--  Module Interpretador
--  Universidad Simón Bolívar
--  Laboratorio de Traductores e Interpretadores (CI-3725)
--  Proyecto 3 - Análisis de Contexto e Intérprete
--  Desarrollado por: Christian Chomiak     05-38034
--                    Maria Gracia Hidalgo  03-36048

module Interpretador ()
    where

import Data.Maybe
import Language.AsGArD.Parser.AST
import SymTable
import System.IO

{-runAsgard::SymTable->Programa->IO()
runAsgard st (Programa declaracion instruccion)
    = do
        st <- runInstruccion st declaracion instruccion
        return ()

runInstruccion::SymTable->[Declaracion]->[Instruccion]->IO (SymTable, Maybe SymValue)
runInstruccion st declaracion ((InstrAsignacion token exp):xs)
    = do
        val <- asignacion token exp
        return (st, Just val)


asignacion::SymTable->[Instruccion]->IO(SymTable, Maybe SymValue)
asignacion st ((InstrAsignacion token exp):xs)
    = do
        if isMember var st then
            replace var 
        
evalBool :: SymTable -> [Instruccion] -> Exp -> IO (Bool)
evalBool _ _ _ = return True        
  
  -}
  
{-

getValue::SymTable->Instruccion->Exp->IO (SymValue)
getValue st _ (ExpIdent t)
    = return symval
    where
        exp = show t
        symval
            = if msym == Nothing then
                error "Variable " ++ show t ++ " no inicializada."
              else
                value msym
        value (Just (VarSymbol _ _ (Just v))) = v
        msym = find exp st
getValue st instruccion exp = evaluarExp st instruccion exp

evaluarExp::SymTable->Instruccion->Exp->IO (SymValue)
evaluarExp st instruccion (ExpBinaria exp1 oper exp2)
    = do
        s1 <- getValue st instruccion exp1
        s2 <- getValue st instruccion exp2
        return (expresionSuma s1 s2)

        
   -}
{-        
evaluarExpPrefija::SymTable->Instruccion->Exp->IO (SymValue)
evaluarExpPrefija st instruccion (ExpPrefja oper exp)
    = do
        s <- getValue st instruccion
        if oper == Resta
           return expresionMenos (s)-}
            
-- Expresiones Aritméticas        
expresionSuma::SymValue -> SymValue -> SymValue
expresionSuma (Numero n1) (Numero n2) = Numero (n1 + n2)

expresionResta::SymValue -> SymValue -> SymValue
expresionResta (Numero n1) (Numero n2) = Numero (n1 - n2)

expresionMult::SymValue -> SymValue -> SymValue
expresionMult (Numero n1) (Numero n2) = Numero (n1 * n2)

expresionDivision::SymValue -> SymValue -> SymValue
expresionDivision (Numero n1) (Numero n2) = Numero (div n1 n2)

expresionModulo::SymValue -> SymValue -> SymValue
expresionModulo (Numero n1) (Numero n2) = Numero (mod n1 n2)

expresionMenor::SymValue -> SymValue -> SymValue
expresionMenor (Numero n1) (Numero n2) = Booleano (n1 < n2)

expresionMenori::SymValue -> SymValue -> SymValue
expresionMenori (Numero n1) (Numero n2) = Booleano (n1 <= n2)

expresionMayor::SymValue -> SymValue -> SymValue
expresionMayor (Numero n1) (Numero n2) = Booleano (n1 > n2)

expresionMayori::SymValue -> SymValue -> SymValue
expresionMayori (Numero n1) (Numero n2) = Booleano (n1 >= n2)

expresionMenos::SymValue -> SymValue
expresionMenos (Numero n) = Numero (-n)

   
--Expresiones Booleanas
expresionConj::SymValue -> SymValue -> SymValue
expresionConj (Booleano b1) (Booleano b2) = Booleano (b1 && b2)

expresionDisj::SymValue -> SymValue -> SymValue
expresionDisj (Booleano b1) (Booleano b2) = Booleano (b1 || b2)

expresionIgual ::SymValue -> SymValue -> SymValue
expresionIgual (Numero t1) (Numero t2) = Booleano (t1 == t2)
expresionIgual (Booleano t1) (Booleano t2) = Booleano (t1 == t2)
expresionIgual (Canvas t1) (Canvas t2) = Booleano (t1 == t2)
-- 
-- expresionDesigual ::SymValue -> SymValue -> SymValue
-- expresionDesigual (Tipo t1) (Tipo t2) = Tipo (t1 /= t2)

expresionNegacion::SymValue -> SymValue
expresionNegacion (Booleano b) = Booleano (not b)


{-   
--Expresiones Canvas
expresionHConcat ::SymValue -> SymValue -> SymValue
ExpBinaria (Canvas c1) (Canvas c2) = Canvas (c1 ++ c2)

expresionVConcat ::SymValue -> SymValue -> SymValue
ExpBinaria (Canvas c1) (Canvas c2) = Canvas (c1 ++ c2)

expresionRotacion::SymValue -> SymValue
ExpPostfija (Booleano b) = not b

expresionTrasposicion::SymValue -> SymValue
ExpPostfija (Booleano b) = not b
-}