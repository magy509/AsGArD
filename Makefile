.PHONY: all clean

all: SintAsgard

SintAsgard: Main.hs Language/AsGArD/Lexer.hs Language/AsGArD/Lexer/Token.hs Language/AsGArD/Parser.hs Language/AsGArD/Parser/AST.hs
	ghc --make Main -o SintAsgard

Language/AsGArD/Parser.hs: Language/AsGArD/Parser.y
	happy -gai $<

Language/AsGArD/Lexer.hs: Language/AsGArD/Lexer.x
	alex --outfile=$@ $<

clean:
	rm -f ./*.o ./*.hi SintAsgard Language/AsGArD/Parser.info Language/AsGArD/Parser.hs Language/AsGArD/Lexer.hs
