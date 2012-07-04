--  Module Main
--  Universidad Simón Bolívar
--  Laboratorio de Traductores e Interpretadores (CI-3725)
--  Proyecto 3 - Análisis de Contexto e Intérprete
--  Desarrollado por: Christian Chomiak     05-38034
--                    Maria Gracia Hidalgo  03-36048

module Main (main) where

import Language.AsGArD.Lexer (alexScanTokens)
import Language.AsGArD.Parser (parse)

import Language.AsGArD.Lexer.Token

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