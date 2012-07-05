-- Module Main
-- Universidad Simón Bolívar
-- Laboratorio de Traductores e Interpretadores (CI-3725)
-- Proyecto 3 - Análisis de Contexto e Intérprete
-- Desarrollado por: Christian Chomiak      05-38034
--                   Maria Gracia Hidalgo   03-36048


module Language.AsGArD.Parser.AST where

import Language.AsGArD.Lexer.Token

-- Definicion de los tipos de datos
data Programa = Programa [Declaracion] [Instruccion]
              deriving (Eq)

instance Show Programa where
  show t = case t of
   Programa _ i -> "Programa\n" ++ (unlines (map show i))

data Declaracion = Declaracion [Token] Tipo                 
                 deriving (Eq, Show)

{-instance Show Declaracion where
  show t = case t of
   Declaracion _ _ -> ""--}

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
         deriving (Eq)

instance Show Exp where
  show t = case t of
   ExpBinaria e o f -> "ExpBinaria" ++ "\n      Operacion: " ++ show o ++ "\n      Operando izquierdo: " ++ show e ++ "\n      Operando derecho: " ++ show f
   ExpPrefija o e -> "ExpPrefija " ++ show o ++ " " ++ show e
   ExpPostfija e o -> "ExpPostfija " ++ show e ++ " " ++ show o
   ExpParentesis e -> "ExpParentesis " ++ show e
   ExpCanvas l -> "ExpCanvas " ++ show l
   ExpIdent t -> "ExpIdent " ++ show t
   ExpNumero n -> "ExpNumero " ++ show n
   ExpTrue -> "ExpTrue"
   ExpFalse -> "ExpFalse"


data Oper = 
      Suma
    | Resta
    | Mult
    | Division
    | Modulo
    | Menor
    | Menori
    | Mayor
    | Mayori
    | Conj
    | Disj
    | Negacion
    | Igual
    | Desigual
    | Hconcat
    | Vconcat
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
                 | InstrRead Token
                 | InstrPrint Exp
                 deriving (Eq)


instance Show Instruccion where
  show t = case t of
   InstrAsignacion t e -> "InstrAsignacion\n" ++ "      " ++  show t ++ "\n      " ++ show e
   InstrCondicional e i -> "InstrCondicional\n  " ++ "Guardia: " ++ show e ++ "\n   " ++ "Exito: " ++ (unlines (map show i))
   InstrCondicionalElse e i j -> "InstrCondicionalElse " ++ show e ++ (unlines (map show i)) ++ (unlines (map show j))
   InstrRepeticionInd e i -> "InstrRepeticionInd " ++ show e ++ " " ++ (unlines (map show i))
   InstrRepeticionDetBase e f i -> "InstrRepeticionDetBase " ++ show e ++ " " ++ show f ++ " " ++ (unlines (map show i))
   InstrRepeticionDet t e f i -> "InstrRepeticionDet " ++ show t ++ " " ++ show e ++ " " ++ show f ++ " " ++ (unlines (map show i))
   InstrAlcance d i -> "InstrAlcance " ++ " " ++ (unlines (map show i))
   InstrRead t -> "InstrRead " ++ show t
   InstrPrint e -> "InstrPrint " ++ show e
   --_ -> "BU"


{--
instance Show Instruccion where
  show t = case t of
   InstrAsignacion t e -> show t
   _ -> "BU"
--}


type Numero = Integer

