default: compile clean_src
compile:
	cd src; \
	cp happy_grammar.hs happy_grammar.y; \
	happy -o Parser.hs happy_grammar.y; \
	ghc -o ../Lang.out Main.hs
clean_src:
	cd src; \
	rm -f *.hi; \
	rm -f *.o; \
	rm -f *.y; \
	rm -f *.out	
clean: clean_src
	rm -f *.out
