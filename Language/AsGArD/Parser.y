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
        Literalcanvas   { TkLienzo $$   }
        Numero          { TkNum $$      }
        Ident           { TkIdent $$    }
--      Errores         { TkError $$    }



%left           "coma"
%left           "pcoma"
-- %right               "asignacion"
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
          "begin" Instruccion "end"                             { Programa $2           }
        | "using" ListaDeclaracion "begin" Instruccion "end"    { Programa $1 $3        }

ListaDeclaracion:
          ListaIdentificadores "of type" Tipo                           { ListaDeclaracion $1 $3        }
        | ListaDeclaracion "pcoma" ListaIdentificadores "of type" Tipo  { ListaDeclaracion $1 $3  $5    }

ListaIdentificadores:
          Identificador "of type" Tipo                                  { ListaIdentificadores $1 $3    }
        | ListaIdentificadores "coma" Identificador "of type" Tipo      { ListaIdentificadores $1 $3 $5 }

Exp: -- Reglas para las expresiones
          Exp "suma" Exp                        { ExpBinaria    $1 Suma $3      }
        | Exp "resta" Exp                       { ExpBinaria    $1 Resta $3     }
        | "resta" Exp   %prec MUnario           { ExpPrefija     Resta $2        }
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
        | "true"                                { ExpTrue       True            }
        | "false"                               { ExpFalse      False           }
        | Literalcanvas                         { ExpCanvas     $1              }

-- Reglas para las instrucciones
Instruccion:
          Identificador "asignacion" Exp                                        { InstrAsignacion       $1 $3           }
        | Instruccion "pcoma" Instruccion                                       { InstrSecuenciacion    $1 $3           }
        | "if" Exp "then" Instruccion "done"                                    { InstrCondicional      $2 $4           }
        | "if" Exp "then" Instruccion "else" Instruccion "done"                 { InstrCondicional      $2 $4 $6        }
        | "while" Exp "repeat" Instruccion "done"                               { InstrRepeticionInd    $2 $4           }
        | "from" Exp "to" Exp "repeat" Instruccion "done"                       { InstrRepeticionDet    $2 $4 $6        }
        | "with" Identificador "from" Exp "to" Exp "repeat" Instruccion "done"  { InstrRepeticionDet    $2 $4 $6 $8     }
        | "begin" Instruccion "end"                                             { InstrAlcance          $2              }
        | "using" ListaDeclaracion "begin" Instruccion "end"                    { InstrAlcance          $2 $4           }
        | "read" "(" Instruccion ")"                                            { InstrRead             $3              }
        | "print" "(" Exp ")"                                                   { InstrPrint            $3              }

Tipo:
          "integer"     { TInteger }
        | "boolean"     { TBoolean }
        | "canvas"      { TCanvas  }

Identificador:
        Ident { Identificador $1 }

{
parseError = error "¡Carambolas, se encogieron mis polainas!"
}
