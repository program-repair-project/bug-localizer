MAKE=@make
DUNE=@dune
LN=@ln -sf
RM=@rm
EXE=localizer

all:
	$(DUNE) build src/main.exe
	$(DUNE) build src/visualizer.exe
	$(LN) _build/default/src/main.exe $(EXE)
	$(LN) _build/default/src/visualizer.exe visualizer

test: all
	$(MAKE) -C test
	$(DUNE) test

clean:
	$(MAKE) -C test clean
	$(DUNE) clean
	$(RM) -rf $(EXE)
