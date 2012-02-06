ERL_FILES=$(wildcard src/*.erl)
BEAM_FILES=$(patsubst src/%.erl,ebin/%.beam,$(ERL_FILES))

all: $(BEAM_FILES)

clean:
	rm -fv $(BEAM_FILES)
	
$(BEAM_FILES): ebin/bio_ers.app Emakefile $(ERL_FILES)
	erl -make

.PHONY: all clean

