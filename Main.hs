module Main (main) where

import Language.AsGArD.Lexer (alexScanTokens)
import Language.AsGArD.Parser (parse)
import Language.AsGArD.Lexer.Token

-- Some action helpers:
tok f p s = f p s

printElements :: [Token] -> IO ()
printElements = mapM_ print


main = do ts <- alexScanTokens <$> getContents
	  if any isError ts then putStr . unlines . map show . filter isError $ tokens s else putStr . unlines . map show $ ts



-- main = do s <- getContents
--           mapM_ print $ if null $ errores s then tokens s
--                                             else errores s
--           where
--                 isError (TkError _ _ _) = True
--                 isError _               = False
-- 
--                 tokens s = alexScanTokens s
--                 errores s = filter isError $ tokens s
--                 
--                 
--                 
-- main = print . parse . alexScanTokens =<< getContents