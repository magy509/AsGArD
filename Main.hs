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