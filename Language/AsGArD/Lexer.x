-- 	Module Main
-- 	Universidad Simón Bolívar
-- 	Laboratorio de Traductores e Interpretadores (CI-3725)
-- 	Proyecto 2 - Parser usando Haskell y Happy
-- 	Desarrollado por:		Christian Chomiak 	05-38034
-- 					Maria Gracia Hidalgo    03-36048


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
        @literalcanvas         { tok (\ (AlexPn line col _) s -> TkLienzo line col $ readCanvas s) }
        "using"                { tok (\ (AlexPn line col _) s -> TkUsing line col                ) }
        "of" @blancos "type"   { tok (\ (AlexPn line col _) s -> TkOfType line col               ) }
        "canvas"	       { tok (\ (AlexPn line col _) s -> TkCanvas line col               ) }
	"while"		       { tok (\ (AlexPn line col _) s -> TkWhile line col                ) }
	"read"  	       { tok (\ (AlexPn line col _) s -> TkRead line col                 ) }
	"begin"                { tok (\ (AlexPn line col _) s -> TkBegin line col                ) }
        "from"                 { tok (\ (AlexPn line col _) s -> TkFrom line col                 ) }
        "to"                   { tok (\ (AlexPn line col _) s -> TkTo line col                   ) }
        "repeat"               { tok (\ (AlexPn line col _) s -> TkRepeat line col               ) }
        "with"                 { tok (\ (AlexPn line col _) s -> TkWith line col                 ) }
        "if"                   { tok (\ (AlexPn line col _) s -> TkIf line col                   ) }
        "then"                 { tok (\ (AlexPn line col _) s -> TkThen line col                 ) }
        "else"                 { tok (\ (AlexPn line col _) s -> TkElse line col                 ) }
        "done"                 { tok (\ (AlexPn line col _) s -> TkDone line col                 ) }
        "end"                  { tok (\ (AlexPn line col _) s -> TkEnd line col                  ) }
        "print"                { tok (\ (AlexPn line col _) s -> TkPrint line col                ) }
        "integer"              { tok (\ (AlexPn line col _) s -> TkInteger line col              ) }
        "boolean"              { tok (\ (AlexPn line col _) s -> TkBoolean line col              ) }
        "true"                 { tok (\ (AlexPn line col _) s -> TkTrue line col                 ) }
        "false"                { tok (\ (AlexPn line col _) s -> TkFalse line col                ) }
        ","                    { tok (\ (AlexPn line col _) s -> TkComa line col                 ) }
        ";"                    { tok (\ (AlexPn line col _) s -> TkPuntoYComa line col           ) }
        "("                    { tok (\ (AlexPn line col _) s -> TkParAbre line col              ) }
        ")"                    { tok (\ (AlexPn line col _) s -> TkParCierra line col            ) }
        "+"                    { tok (\ (AlexPn line col _) s -> TkSuma line col                 ) }
        "-"                    { tok (\ (AlexPn line col _) s -> TkResta line col                ) }
        "*"                    { tok (\ (AlexPn line col _) s -> TkMult line col                 ) }
        "/"                    { tok (\ (AlexPn line col _) s -> TkDiv line col                  ) }
        "%"                    { tok (\ (AlexPn line col _) s -> TkMod line col                  ) }
        "/" \\                 { tok (\ (AlexPn line col _) s -> TkConjuncion line col           ) }
        \\ "/"                 { tok (\ (AlexPn line col _) s -> TkDisyuncion line col           ) }
        "^"                    { tok (\ (AlexPn line col _) s -> TkNegacion line col             ) }
        "<"                    { tok (\ (AlexPn line col _) s -> TkMenor line col                ) }
        "<="                   { tok (\ (AlexPn line col _) s -> TkMenorIgual line col           ) }
        ">"                    { tok (\ (AlexPn line col _) s -> TkMayor line col                ) }
        ">="                   { tok (\ (AlexPn line col _) s -> TkMayorIgual line col           ) }
        "="                    { tok (\ (AlexPn line col _) s -> TkIgual line col                ) }
        "/="                   { tok (\ (AlexPn line col _) s -> TkDesigual line col             ) }
        ":"                    { tok (\ (AlexPn line col _) s -> TkHorConcat line col            ) }
        "|"                    { tok (\ (AlexPn line col _) s -> TkVerConcat line col            ) }
        "$"                    { tok (\ (AlexPn line col _) s -> TkRot line col                  ) }
        "'"                    { tok (\ (AlexPn line col _) s -> TkTras line col                 ) }
        ":="                   { tok (\ (AlexPn line col _) s -> TkAsignacion line col           ) }
        $digit+                { tok (\ (AlexPn line col _) s -> TkNum line col $ read s         ) }
        $alpha[$alpha $digit]* { tok (\ (AlexPn line col _) s -> TkIdent line col s              ) }
        @blancos;

        . { tok (\ (AlexPn line col _) s -> TkError line col s ) }

{
tok = id
}