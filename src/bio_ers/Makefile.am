#
# Copyright 2011 Karolis Petrauskas
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.
#


ERL_APP_FILES=ebin/bio_ers.app
ERL_BEAM_FILES=\
    ebin/bio_ers.beam \
    ebin/bio_ers_app.beam \
    ebin/bio_ers_solver.beam \
    ebin/bio_ers_sup.beam


all: bio_ers


#
#   Erlang specific build.
#

bio_ers: $(ERL_BEAM_FILES) $(ERL_APP_FILES)

$(ERL_APP_FILES): ebin/% : %
	awk -v "beams=$(ERL_BEAM_FILES)" '/MAKEFILE_REPLACE_WITH_MODULES/{ \
	        modules=beams; \
	        gsub("ebin/", "", modules); \
	        gsub(".beam ", ", ", modules); \
	        gsub(".beam", "", modules); \
	        gsub("MAKEFILE_REPLACE_WITH_MODULES", modules); \
	    }; {print;}' $< > $@

$(ERL_BEAM_FILES): ebin/%.beam: src/%.erl
	$(ERLC) $(ERLCFLAGS) -o ebin $< 

#
#   Some tests.
#
test_run: $(ERL_BEAM_FILES)
	erl -pa ebin -s bio_ers_app startup


#
#   Formalities...
#
clean-local:
	rm -fv $(ERL_BEAM_FILES) $(ERL_APP_FILES)
	
.PHONY: all bio_ers clean-local test_run

