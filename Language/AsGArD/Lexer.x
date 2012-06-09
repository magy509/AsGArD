--  Module Main
--  Universidad Simón Bolívar
--  Laboratorio de Traductores e Interpretadores (CI-3725)
--  Proyecto 1 - Lexer usando Haskell y Alex
--  Desarrollado por: Christian Chomiak    05-38034
--                    Maria Gracia Hidalgo 03-36048


{
module Language.AsGArD.Lexer where

import Language.AsGArD.Lexer.Token
}

%wrapper "posn" -- FIXME


$digit = [0-9]
$alpha = [a-zA-Z]
@literalcanvas = "<empty>" | "</>" | "<\>" | "<|>" | "<_>" | "<->" | "< >"
@comentarios = ("{-" ([^\-] | \-[^\}] | ($white))* \-* "-}")
@blancos = (($white) | (@comentarios))+

tokens :-
        @literalcanvas         { tok (\p s -> TkLienzo $ readCanvas s) }
        "using"                { tok (\p s -> TkUsing                ) }
        "of" @blancos "type"   { tok (\p s -> TkOfType               ) }
        "canvas"	       { tok (\p s -> TkCanvas               ) }
	"while"		       { tok (\p s -> TkWhile                ) }
	"read"  	       { tok (\p s -> TkRead                 ) }
	"begin"                { tok (\p s -> TkBegin                ) }
        "from"                 { tok (\p s -> TkFrom                 ) }
        "to"                   { tok (\p s -> TkTo                   ) }
        "repeat"               { tok (\p s -> TkRepeat               ) }
        "with"                 { tok (\p s -> TkWith                 ) }
        "if"                   { tok (\p s -> TkIf                   ) }
        "then"                 { tok (\p s -> TkThen                 ) }
        "else"                 { tok (\p s -> TkElse                 ) }
        "done"                 { tok (\p s -> TkDone                 ) }
        "end"                  { tok (\p s -> TkEnd                  ) }
        "print"                { tok (\p s -> TkPrint                ) }
        "integer"              { tok (\p s -> TkInteger              ) }
        "boolean"              { tok (\p s -> TkBoolean              ) }
        "true"                 { tok (\p s -> TkTrue                 ) }
        "false"                { tok (\p s -> TkFalse                ) }
        ","                    { tok (\p s -> TkComa                 ) }
        ";"                    { tok (\p s -> TkPuntoYComa           ) }
        "("                    { tok (\p s -> TkParAbre              ) }
        ")"                    { tok (\p s -> TkParCierra            ) }
        "+"                    { tok (\p s -> TkSuma                 ) }
        "-"                    { tok (\p s -> TkResta                ) }
        "*"                    { tok (\p s -> TkMult                 ) }
        "/"                    { tok (\p s -> TkDiv                  ) }
        "%"                    { tok (\p s -> TkMod                  ) }
        "/" \\                 { tok (\p s -> TkConjuncion           ) }
        \\ "/"                 { tok (\p s -> TkDisyuncion           ) }
        "^"                    { tok (\p s -> TkNegacion             ) }
        "<"                    { tok (\p s -> TkMenor                ) }
        "<="                   { tok (\p s -> TkMenorIgual           ) }
        ">"                    { tok (\p s -> TkMayor                ) }
        ">="                   { tok (\p s -> TkMayorIgual           ) }
        "="                    { tok (\p s -> TkIgual                ) }
        "/="                   { tok (\p s -> TkDesigual             ) }
        ":"                    { tok (\p s -> TkHorConcat            ) }
        "|"                    { tok (\p s -> TkVerConcat            ) }
        "$"                    { tok (\p s -> TkRot                  ) }
        "'"                    { tok (\p s -> TkTras                 ) }
        ":="                   { tok (\p s -> TkAsignacion           ) }
        $digit+                { tok (\p s -> TkNum $ read s         ) }
        $alpha[$alpha $digit]* { tok (\p s -> TkIdent s              ) }
        $white+;
        @blancos+;

        . { tok (\ (AlexPn _ line col) s -> TkError s line col) }
