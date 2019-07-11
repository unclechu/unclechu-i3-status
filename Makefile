all:
	stack build --install-ghc -j$(shell nproc --all)
	stack install

clean:
	stack clean

distclean:
	stack clean --full
