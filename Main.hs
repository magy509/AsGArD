module Main (main) where

import Language.AsGArD.Lexer (alexScanTokens)
import Language.AsGArD.Parser (parse)

main = print . parse . alexScanTokens =<< getContents

-- Algo sobre el main que no quiero hacer T_T
-- main = do
-- 	inStr <- getContents
-- 	let parseTree = newl (alexScanTokens inStr) 
-- 	putStrLn ("parseTree:" ++ show(parseTree))
-- 	print "done"
