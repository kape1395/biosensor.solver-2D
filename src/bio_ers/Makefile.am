#
# Copyright 2012 Karolis Petrauskas
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
    ebin/bio_ers_model.beam \
    ebin/bio_ers_solver.beam \
    ebin/bio_ers_sup.beam

ERL_TEST_BEAM_FILES=\
    ebin/bio_ers_testsuite.beam \
    ebin/bio_ers_tests.beam \
    ebin/bio_ers_model_tests.beam

ERL_TEST_MODULES=\
    bio_ers \
    bio_ers_model


all: bio_ers


################################################################################
#   Erlang specific build.
#

bio_ers: ebin priv/bio_ers_solver_port $(ERL_BEAM_FILES) $(ERL_APP_FILES)

ebin:
	mkdir ebin

priv/bio_ers_solver_port: ../bio_ers_solver_port/bio_ers_solver_port
	make -C ../bio_ers_solver_port
	cp ../bio_ers_solver_port/bio_ers_solver_port priv/

$(ERL_APP_FILES): ebin/% : %
	@echo "awk -v \"beams=$(ERL_BEAM_FILES)\" '...' $< > $@"
	@awk -v "beams=$(ERL_BEAM_FILES)" '/MAKEFILE_REPLACE_WITH_MODULES/{ \
	        modules=beams; \
	        gsub("ebin/", "", modules); \
	        gsub(".beam ", ", ", modules); \
	        gsub(".beam", "", modules); \
	        gsub("MAKEFILE_REPLACE_WITH_MODULES", modules); \
	    }; {print;}' $< > $@

$(ERL_BEAM_FILES): ebin/%.beam: src/%.erl
	$(ERLC) $(ERLCFLAGS) -o ebin $< 

################################################################################
#
#   Some tests.
#

$(ERL_TEST_BEAM_FILES): ebin/%.beam: test/%.erl
	$(ERLC) $(ERLCFLAGS) -o ebin $< 

test_run: $(ERL_BEAM_FILES) $(ERL_TEST_BEAM_FILES)
	erl -pa ebin -s bio_ers_app startup

test: bio_ers $(ERL_TEST_BEAM_FILES)
	erl -pa ebin -noinput -s bio_ers_testsuite run $(ERL_TEST_MODULES)


################################################################################
#   Formalities...
#
clean-local:
	rm -rfv ebin priv/bio_ers_solver_port

check-local: test
	
.PHONY: all bio_ers clean-local test_run

