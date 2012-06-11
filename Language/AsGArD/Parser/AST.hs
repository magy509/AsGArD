-- 	Module Main
-- 	Universidad Simón Bolívar
-- 	Laboratorio de Traductores e Interpretadores (CI-3725)
-- 	Proyecto 2 - Parser usando Haskell y Happy
-- 	Desarrollado por:		Christian Chomiak 	05-38034
-- 					Maria Gracia Hidalgo    03-36048


module Language.AsGArD.Parser.AST where

import Language.AsGArD.Lexer.Token

-- Definicion de los tipos de datos
data Programa = Programa [Declaracion] [Instruccion]
              deriving (Eq, Show)

data Declaracion = Declaracion [Token] Tipo                 
                 deriving (Eq, Show)

data Tipo = TInteger                    
          | TBoolean
          | TCanvas
          deriving (Eq, Show)

data Exp = ExpBinaria Exp Oper Exp
         | ExpPrefija Oper Exp
         | ExpPostfija Exp Oper
         | ExpParentesis Exp
         | ExpCanvas LiteralCanvas
	 | ExpIdent Token
         | ExpNumero Numero
         | ExpTrue
         | ExpFalse
         deriving (Eq, Show)

data Oper = Suma
          | Resta
          | Mult
          | Division
          | Modulo
          | Menor
          | Menori
          | Mayor
          | Mayori
          | Igual
          | Conj
          | Disj
          | Desigual
          | Hconcat
          | Vconcat
	  | Negacion
          | Rotacion
          | Trasposicion
        deriving (Eq, Show)

data Instruccion = InstrAsignacion Token Exp
                 | InstrCondicional Exp [Instruccion]
                 | InstrCondicionalElse Exp [Instruccion] [Instruccion]
                 | InstrRepeticionInd Exp [Instruccion]
		 | InstrRepeticionDetBase Exp Exp [Instruccion]
                 | InstrRepeticionDet Token Exp Exp [Instruccion]
		 | InstrAlcance [Declaracion] [Instruccion]
                 | InstrRead [Instruccion]
                 | InstrPrint Exp
                 deriving (Eq, Show)

type Numero = Integer

