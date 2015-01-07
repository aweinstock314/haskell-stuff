TARGETS=conway_life
.PHONY: all clean

all: ${TARGETS}

%: %.hs
	ghc -O2 $^

clean:
	bash -c 'for target in ${TARGETS}; do rm $$target{,.o,.hi}; done'
