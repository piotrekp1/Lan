default:
	cp happy_grammar.hs happy_grammar.y
	happy -o Parser.hs happy_grammar.y
	ghc -o Lang.out Main.hs
clean:
	rm *.hi
	rm *.o
	rm *.y
	rm *.out
