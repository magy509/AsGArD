{-  Module LiteralCanvas
*   Universidad Simón Bolívar
**  Laboratorio de Traductores e Interpretadores (CI-3725)
**  Proyecto 1 - Lexer usando Haskell y Alex
**  Desarrollado por: Christian Chomiak    05-38034
**                    Maria Gracia Hidalgo 03-36048
-}

module Language.AsGArD.Lexer.Token where

data Token = TkNum Int
           | TkIdent String
           | TkUsing
           | TkOfType
           | TkCanvas
           | TkWhile
           | TkRead
           | TkBegin
           | TkFrom
           | TkTo
           | TkRepeat
           | TkWith
           | TkIf
           | TkThen
           | TkElse
           | TkDone
           | TkEnd
           | TkPrint
           | TkInteger
           | TkBoolean
           | TkTrue
           | TkFalse
           | TkLienzo LiteralCanvas
           | TkComa
           | TkPuntoYComa
           | TkParAbre
           | TkParCierra
           | TkSuma
           | TkResta
           | TkMult
           | TkDiv
           | TkMod
           | TkConjuncion
           | TkDisyuncion
           | TkNegacion
           | TkMenor
           | TkMenorIgual
           | TkMayor
           | TkMayorIgual
           | TkIgual
           | TkDesigual
           | TkHorConcat
           | TkVerConcat
           | TkRot
           | TkTras
           | TkAsignacion
           | TkError String Int Int

instance Show Token where
  show t = case t of
    TkNum i       -> "TkNum("      ++ show i ++ ")"
    TkLienzo i    -> "TkLienzo(\"" ++ show i ++ "\")"
    TkIdent i     -> "TkIdent("    ++ show i ++ ")"
    TkUsing       -> "TkUsing"
    TkOfType      -> "TkOfType"
    TkCanvas      -> "TkCanvas"
    TkWhile       -> "TkWhile"
    TkRead        -> "TkRead"
    TkBegin       -> "TkBegin"
    TkFrom        -> "TkFrom"
    TkTo          -> "TkTo"
    TkRepeat      -> "TkRepeat"
    TkWith        -> "TkWith"
    TkIf          -> "TkIf"
    TkThen        -> "TkThen"
    TkElse        -> "TkElse"
    TkDone        -> "TkDone"
    TkEnd         -> "TkEnd"
    TkPrint       -> "TkPrint"
    TkInteger     -> "TkInteger"
    TkBoolean     -> "TkBoolean"
    TkTrue        -> "TkTrue"
    TkFalse       -> "TkFalse"
    TkComa        -> "TkComa"
    TkPuntoYComa  -> "TkPuntoYComa"
    TkParAbre     -> "TkParAbre"
    TkParCierra   -> "TkParCierra"
    TkSuma        -> "TkSuma"
    TkResta       -> "TkResta"
    TkMult        -> "TkMult"
    TkDiv         -> "TkDiv"
    TkMod         -> "TkMod"
    TkConjuncion  -> "TkConjuncion"
    TkDisyuncion  -> "TkDisyuncion"
    TkNegacion    -> "TkNegacion"
    TkMenor       -> "TkMenor"
    TkMenorIgual  -> "TkMenorIgual"
    TkMayor       -> "TkMayor"
    TkMayorIgual  -> "TkMayorIgual"
    TkIgual       -> "TkIgual"
    TkDesigual    -> "TkDesigual"
    TkHorConcat   -> "TkHorConcat"
    TkVerConcat   -> "TkVerConcat"
    TkRot         -> "TkRot"
    TkTras        -> "TkTras"
    TkAsignacion  -> "TkAsignacion"
    TkError i j k -> "Error: Caracter inesperado " ++ show i ++ " en la fila " ++ show j ++ ", columna " ++ show k

data LiteralCanvas = Empty
                   | Slash
                   | BackSlash
                   | Pipe
                   | Underscore
                   | Dash
                   | Blank

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
