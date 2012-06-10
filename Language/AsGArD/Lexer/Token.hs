-- 	Module Main
-- 	Universidad Simón Bolívar
-- 	Laboratorio de Traductores e Interpretadores (CI-3725)
-- 	Proyecto 2 - Parser usando Haskell y Happy
-- 	Desarrollado por:		Christian Chomiak 	05-38034
-- 					Maria Gracia Hidalgo    03-36048


module Language.AsGArD.Lexer.Token where

data Token = TkNum { fila, columna :: Int, número :: Integer }
           | TkIdent { fila, columna :: Int, identificador :: String }
           | TkUsing { fila, columna :: Int }
           | TkOfType { fila, columna :: Int }
           | TkCanvas { fila, columna :: Int }
           | TkWhile { fila, columna :: Int }
           | TkRead { fila, columna :: Int }
           | TkBegin { fila, columna :: Int }
           | TkFrom { fila, columna :: Int }
           | TkTo { fila, columna :: Int }
           | TkRepeat { fila, columna :: Int }
           | TkWith { fila, columna :: Int }
           | TkIf { fila, columna :: Int }
           | TkThen { fila, columna :: Int }
           | TkElse { fila, columna :: Int }
           | TkDone { fila, columna :: Int }
           | TkEnd { fila, columna :: Int }
           | TkPrint { fila, columna :: Int }
           | TkInteger { fila, columna :: Int }
           | TkBoolean { fila, columna :: Int }
           | TkTrue { fila, columna :: Int }
           | TkFalse { fila, columna :: Int }
           | TkLienzo { fila, columna :: Int, lienzo :: String }
           | TkComa { fila, columna :: Int }
           | TkPuntoYComa { fila, columna :: Int }
           | TkParAbre { fila, columna :: Int }
           | TkParCierra { fila, columna :: Int }
           | TkSuma { fila, columna :: Int }
           | TkResta { fila, columna :: Int }
           | TkMult { fila, columna :: Int }
           | TkDiv { fila, columna :: Int }
           | TkMod { fila, columna :: Int }
           | TkConjuncion { fila, columna :: Int }
           | TkDisyuncion { fila, columna :: Int }
           | TkNegacion { fila, columna :: Int }
           | TkMenor { fila, columna :: Int }
           | TkMenorIgual { fila, columna :: Int }
           | TkMayor { fila, columna :: Int }
           | TkMayorIgual { fila, columna :: Int }
           | TkIgual { fila, columna :: Int }
           | TkDesigual { fila, columna :: Int }
           | TkHorConcat { fila, columna :: Int }
           | TkVerConcat { fila, columna :: Int }
           | TkRot { fila, columna :: Int }
           | TkTras { fila, columna :: Int }
           | TkAsignacion { fila, columna :: Int }
           | TkError { fila, columna :: Int, inesperado :: String }
           deriving (Eq)

instance Show Token where
  show t = case t of
    TkNum _ _ i      -> "TkNum("      ++ show i ++ ")"
    TkLienzo _ _ i   -> "TkLienzo(\"" ++ show i ++ "\")"
    TkIdent _ _ i    -> "TkIdent("    ++ show i ++ ")"
    TkUsing _ _      -> "TkUsing"
    TkOfType _ _     -> "TkOfType"
    TkCanvas _ _     -> "TkCanvas"
    TkWhile _ _      -> "TkWhile"
    TkRead _ _       -> "TkRead"
    TkBegin _ _      -> "TkBegin"
    TkFrom _ _       -> "TkFrom"
    TkTo _ _         -> "TkTo"
    TkRepeat_ _      -> "TkRepeat"
    TkWith _ _       -> "TkWith"
    TkIf _ _         -> "TkIf"
    TkThen _ _       -> "TkThen"
    TkElse _ _       -> "TkElse"
    TkDone _ _       -> "TkDone"
    TkEnd _ _        -> "TkEnd"
    TkPrint _ _      -> "TkPrint"
    TkInteger _ _    -> "TkInteger"
    TkBoolean _ _    -> "TkBoolean"
    TkTrue _ _       -> "TkTrue"
    TkFalse _ _      -> "TkFalse"
    TkComa _ _       -> "TkComa"
    TkPuntoYComa _ _ -> "TkPuntoYComa"
    TkParAbre _ _    -> "TkParAbre"
    TkParCierra _ _  -> "TkParCierra"
    TkSuma _ _       -> "TkSuma"
    TkResta _ _      -> "TkResta"
    TkMult _ _       -> "TkMult"
    TkDiv _ _        -> "TkDiv"
    TkMod _ _        -> "TkMod"
    TkConjuncion _ _ -> "TkConjuncion"
    TkDisyuncion _ _ -> "TkDisyuncion"
    TkNegacion _ _   -> "TkNegacion"
    TkMenor _ _      -> "TkMenor"
    TkMenorIgual _ _ -> "TkMenorIgual"
    TkMayor _ _      -> "TkMayor"
    TkMayorIgual _ _ -> "TkMayorIgual"
    TkIgual _ _      -> "TkIgual"
    TkDesigual _ _   -> "TkDesigual"
    TkHorConcat _ _  -> "TkHorConcat"
    TkVerConcat _ _  -> "TkVerConcat"
    TkRot _ _        -> "TkRot"
    TkTras _ _       -> "TkTras"
    TkAsignacion _ _ -> "TkAsignacion"
    TkError i j k    -> "Error: En la fila " ++ show j ++ ", columna " ++ show k " está el caracter inesperado " ++ show i ++


data LiteralCanvas = Empty
                   | Slash
                   | BackSlash
                   | Pipe
                   | Underscore
                   | Dash
                   | Blank
                   deriving (Eq)

readCanvas::String->LiteralCanvas
readCanvas s = case s of
  "<empty>" -> Empty
  "</>"     -> Slash
  "<\\>"    -> BackSlash
  "<|>"     -> Pipe
  "<_>"     -> Underscore
  "<->"     -> Dash
  "< >"     -> Blank
  otherwise -> error "Literal Canvas invalido"

instance Show LiteralCanvas where
  show s = case s of
    Empty      -> "empty"
    Slash      -> "/"
    BackSlash  -> "\\"
    Pipe       -> "|"
    Underscore -> "_"
    Dash       -> "-"
    Blank      -> " "