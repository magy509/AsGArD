--  Module Main
--  Universidad Simón Bolívar
--  Laboratorio de Traductores e Interpretadores (CI-3725)
--  Proyecto 3 - Análisis de Contexto e Intérprete
--  Desarrollado por: Christian Chomiak     05-38034
--                    Maria Gracia Hidalgo  03-36048

module Main (main) where

import qualified Data.Map as Map
import Interpretador
import Language.AsGArD.Lexer (alexScanTokens)
import Language.AsGArD.Parser (parse)
import Language.AsGArD.Lexer.Token
import StaticAnalyzer
import SymTable
import System.IO
import System.Environment



--main = print . parse . alexScanTokens =<< getContents

printElements :: [Token] -> IO()
printElements [] = return ()
printElements (x:xs) = do print x
                          printElements xs


main = do

   s <- getContents
   let x = (alexScanTokens s)
   let errores = filter (\ x -> case x of
         TkError _ _ _ -> True
         otherwise -> False) x
   if ((length errores) == 0) then 
      --(printElements x)
      print (parse x)
   else 
      (printElements errores)

{-      
      
printPrograma::SymTable->Programa->String->String
printPrograma st [] ac = ac
printPrograma st ((Declaracion (t:ts) tipo):ds) ac
    = " " ++ nombre ++ " del tipo " ++ show tipo ++ "\n" ++ printArgs args " Declaracion: \n"
      ++ "printDec dec 2 \n" ++ printPrograma st [Declaracion ts tipo] ac
      if ds /= [] then
        printPrograma st ds ac
      where nombre = show t
             
printArgs::[(Token, SymType)]->String->String
printArgs[] ac = "  Argumentos:\n   \nNinguno" ++ ac
printArgs args ac
    = " Argumentos:\n"++printArgs args ac
    where
      printArgs [] ac = ac
      printArgs ((nombre,tipo):args) ac = " " ++ show nombre ++ ": " ++ show tipo ++ "\n" ++ printArgs ac
      
ident :: Int->String
ident il = replicate (il*2) ' '

printInstr::Instruccion->Int->String->String
printInstr (InstrAsignacion token exp) il ac
  = (ident il)++"Asignment of\n"
    ++printVarOper nombre (il+1) (ident il)++" :=\n"++(printExp exp (il+1) ac)
    where nombre = show token
          
printVarOper::Token->Int->String->String
printVarOper token il ac
    = (ident)++"Variable '" ++ show token ++ "'\n"++ac
    
    -}