TARGETS=conway_life cheatingquine
.PHONY: all clean

PROFFLAGS=-prof -rtsopts -auto-all -caf-all
OPTFLAGS=-O2 -Odph -fllvm

all: ${TARGETS}

%: %.hs
	ghc ${OPTFLAGS} $^

%.simpl: %.hs
	ghc ${OPTFLAGS} -fforce-recomp -ddump-simpl -dsuppress-all $^ > $@

%.profbuild: %.hs
	ghc ${OPTFLAGS} $^
	ghc ${OPTFLAGS} ${PROFFLAGS} -osuf p_o $^
	echo 'Run with "+RTS -sstderr -p" to get a .prof report.'

clean:
	bash -c 'for target in ${TARGETS}; do rm $$target{,.o,.hi}; done'
