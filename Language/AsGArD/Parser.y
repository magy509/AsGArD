{
module Language.AsGArD.Parser where

import Language.AsGArD.Parser.AST
import Language.AsGArD.Lexer.Token
}

%name parse
%tokentype { Token }
%error { parseError }

%token
        "using"         { TkUsing       }
        "of type"       { TkOfType      }
        "canvas"        { TkCanvas      }
        "while"         { TkWhile       }
        "read"          { TkRead        }
        "begin"         { TkBegin       }
        "from"          { TkFrom        }
        "to"            { TkTo          }
        "repeat"        { TkRepeat      }
        "with"          { TkWith        }
        "if"            { TkIf          }
        "then"          { TkThen        }
        "else"          { TkElse        }
        "done"          { TkDone        }
        "end"           { TkEnd         }
        "print"         { TkPrint       }
        "integer"       { TkInteger     }
        "boolean"       { TkBoolean     }
        "true"          { TkTrue        }
        "false"         { TkFalse       }
        "coma"          { TkComa        }
        "pcoma"         { TkPuntoYComa  }
        "("             { TkParAbre     }
        ")"             { TkParCierra   }
        "suma"          { TkSuma        }
        "resta"         { TkResta       }
        "mult"          { TkMult        }
        "division"      { TkDiv         }
        "modulo"        { TkMod         }
        "conj"          { TkConjuncion  }
        "disj"          { TkDisyuncion  }
        "negacion"      { TkNegacion    }
        "menor"         { TkMenor       }
        "menori"        { TkMenorIgual  }
        "mayor"         { TkMayor       }
        "mayori"        { TkMayorIgual  }
        "igual"         { TkIgual       }
        "desigual"      { TkDesigual    }
        "hconcat"       { TkHorConcat   }
        "vconcat"       { TkVerConcat   }
        "rotacion"      { TkRot         }
        "trasposicion"  { TkTras        }
        "asignacion"    { TkAsignacion  }
        Canvas   	{ TkLienzo $$   }
        Numero          { TkNum $$      }
        Ident           { TkIdent $$    }
--      Errores         { TkError $$    }


--Reglas de Precedencia
%left           "coma"
%left           "pcoma"
%left           "disj"
%left           "conj"
%left           "igual" "desigual"
%nonassoc       "menor" "menori" "mayor" "mayori"
%left           "suma" "resta"
%left           "mult" "division" "modulo"
%right          "negacion"
%right          NUnaria
%left           MUnario
%left           "hconcat" "vconcat"
%left           "rotacion"
%left           Ranita
%left           "trasposicion"

%%


-- Definicion de las reglas de la gramatica
Programa:
          "begin" ListaInstrucciones "end"                               { Programa [] $2 }
        | "using" ListaDeclaraciones "begin" ListaInstrucciones "end"    { Programa $2 $4 }

ListaDeclaraciones:
	Declaracion				 { [$1]     }
	| ListaDeclaraciones "pcoma" Declaracion { $1++[$3] }

Declaracion:
          Identificadores "of type" Tipo  { Declaracion $1 $3 }

Identificadores:
	  Ident				{ [$1]     }
	| Identificadores "coma" Ident 	{ $1++[$3] }

Exp:
          Exp "suma" Exp                        { ExpBinaria    $1 Suma $3      }
        | Exp "resta" Exp                       { ExpBinaria    $1 Resta $3     }
        | "resta" Exp   %prec MUnario           { ExpPrefija     Resta $2       }
        | Exp "mult" Exp                        { ExpBinaria    $1 Mult $3      }
        | Exp "division" Exp                    { ExpBinaria    $1 Division $3  }
        | Exp "modulo" Exp                      { ExpBinaria    $1 Modulo $3    }
        | Exp "menor" Exp                       { ExpBinaria    $1 Menor $3     }
        | Exp "menori" Exp                      { ExpBinaria    $1 Menori $3    }
        | Exp "mayor" Exp                       { ExpBinaria    $1 Mayor $3     }
        | Exp "mayori" Exp                      { ExpBinaria    $1 Mayori $3    }
        | Exp "igual" Exp                       { ExpBinaria    $1 Igual $3     }
        | Exp "negacion" %prec NUnaria          { ExpPostfija   $1 Negacion     }
        | Exp "conj" Exp                        { ExpBinaria    $1 Conj $3      }
        | Exp "disj" Exp                        { ExpBinaria    $1 Disj $3      }
        | Exp "desigual" Exp                    { ExpBinaria    $1 Desigual $3  }
        | Exp "hconcat" Exp                     { ExpBinaria    $1 Hconcat $3   }
        | Exp "vconcat" Exp                     { ExpBinaria    $1 Vconcat $3   }
        | "rotacion" Exp                        { ExpPrefija    Rotacion $2     }
        | Exp "trasposicion" %prec Ranita       { ExpPostfija   $1 Trasposicion }
        | "(" Exp ")"                           { ExpParentesis $2              }
        | Numero                                { ExpNumero     $1              }
        | "true"                                { ExpTrue                       }
        | "false"                               { ExpFalse                      }
        | Canvas	                	{ ExpCanvas     $1              }

ListaInstrucciones:
	  Instruccion                            { [$1]     }
	| ListaInstrucciones "pcoma" Instruccion { $1++[$3] }

Instruccion:
          Ident "asignacion" Exp                                               { InstrAsignacion        $1 $3       }
        | "if" Exp "then" ListaInstrucciones "done"                            { InstrCondicional       $2 $4       }
        | "if" Exp "then" ListaInstrucciones "else" ListaInstrucciones "done"  { InstrCondicionalElse   $2 $4 $6    }
        | "while" Exp "repeat" ListaInstrucciones "done"                       { InstrRepeticionInd     $2 $4       }
        | "from" Exp "to" Exp "repeat" ListaInstrucciones "done"               { InstrRepeticionDetBase $2 $4 $6    }
        | "with" Ident "from" Exp "to" Exp "repeat" ListaInstrucciones "done"  { InstrRepeticionDet     $2 $4 $6 $8 }
        | "using" ListaDeclaraciones "begin" ListaInstrucciones "end"          { InstrAlcance           $2 $4       }
        | "read" "(" ListaInstrucciones ")"                                    { InstrRead              $3          }
        | "print" "(" Exp ")"                                                  { InstrPrint             $3          }

Tipo:
          "integer"     { TInteger }
        | "boolean"     { TBoolean }
        | "canvas"      { TCanvas  }

{
parseError = error "¡Carambolas, se encogieron mis polainas!"
}
