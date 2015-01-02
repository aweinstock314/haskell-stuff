TARGETS=conway_life
.PHONY: all clean

all: ${TARGETS}

%: %.hs
	ghc -O2 $^
