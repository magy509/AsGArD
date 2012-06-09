module Language.AsGArD.Parser.AST where

-- Definicion de los tipos de datos
data Programa = Programa ListaDeclaracion Instruccion
              deriving (Eq, Show)

data ListaDeclaracion = ListaDeclaracion ListaDeclaracion ListaIdentificadores Tipo
                      deriving (Eq, Show)

data ListaIdentificadores = ListaIdentificadores ListaIdentificadores Ident Tipo
                          deriving (Eq, Show)

data Identificador = Identificador String
                   deriving (Eq, Show)

data Tipo = TInteger
          | TBoolean
          | TCanvas
          deriving (Eq, Show)

data Exp = ExpBinaria Exp Oper Exp
         | ExpPrefija OperUnario Exp
         | ExpPostfija Exp OperUnario
         | ExpParentesis Exp
         | ExpCanvas Canvas
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
          deriving (Eq, Show)

data OperUnario =
          Negacion
        | Rotacion
        | Trasposicion
        deriving (Eq, Show)

data Instruccion = InstrAsignacion Ident Exp
                 | InstrSecuenciacion Instruccion Instruccion
                 | InstrCondicional Exp Instruccion
                 | InstrCondicionalElse Exp Instruccion Instruccion
                 | InstrRepeticionInd Exp Instruccion
                 | InstrRepeticionDet Identificador Exp Exp
                 | InstrAlcance ListaDeclaracion Instruccion
                 | InstrRead Instruccion
                 | InstrPrint Exp
                 deriving (Eq, Show)

type Ident = String

type Canvas = String

type Numero = Integer
