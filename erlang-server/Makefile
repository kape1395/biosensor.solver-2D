ERL_FILES=$(wildcard src/*.erl)
BEAM_FILES=$(patsubst src/%.erl,ebin/%.beam,$(ERL_FILES))

all: $(BEAM_FILES)

clean:
	rm -fv $(BEAM_FILES)
	
$(BEAM_FILES): $(ERL_FILES)
	erl -make

run: $(BEAM_FILES)
	erl -pa ebin -s bio_ers_app startup

.PHONY: all clean run

