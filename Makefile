TARGETS=conway_life cheatingquine
.PHONY: all clean

PROFFLAGS=-prof -rtsopts -auto-all -caf-all

all: ${TARGETS}

%: %.hs
	ghc -O2 $^

%.simpl: %.hs
	ghc -O2 -fforce-recomp -ddump-simpl $^ > $@

clean:
	bash -c 'for target in ${TARGETS}; do rm $$target{,.o,.hi}; done'
