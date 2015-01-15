TARGETS=conway_life cheatingquine
.PHONY: all clean

PROFFLAGS=-prof -rtsopts -auto-all -caf-all

all: ${TARGETS}

%: %.hs
	ghc -O2 $^

%.simpl: %.hs
	ghc -O2 -fforce-recomp -ddump-simpl $^ > $@

%.profbuild: %.hs
	ghc -O2 $^
	ghc -O2 ${PROFFLAGS} -osuf p_o $^
	echo 'Run with "+RTS -sstderr -p" to get a .prof report.'

clean:
	bash -c 'for target in ${TARGETS}; do rm $$target{,.o,.hi}; done'
