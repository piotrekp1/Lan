default: Lang clean_src

Gramma:
	cd src;\
	cp Gramma_tmp.hs Gramma.y;\
	happy Gramma.y

Tokens: 
	cd src;\
	cp Tokens_tmp.hs Tokens.x;\
	alex Tokens.x

Lang: Gramma Tokens
	cd src;\
	ghc -o ../Lan Main.hs
clean_src:
	cd src; \
	rm -f *.hi; \
	rm -f *.o; \
	rm -f *.y; \
	rm -f Gramma.hs Tokens.hs \
	rm -f *.out	
clean: clean_src
	rm -f *.out
	rm Lan
