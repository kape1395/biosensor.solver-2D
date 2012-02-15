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


ERL_FILES=$(wildcard src/*.erl)
BEAM_FILES=$(patsubst src/%.erl,ebin/%.beam,$(ERL_FILES))

all: $(BEAM_FILES)

clean-local:
	rm -fv $(BEAM_FILES)
	
$(BEAM_FILES): $(ERL_FILES)
	erl -make

run: $(BEAM_FILES)
	erl -pa ebin -s bio_ers_app startup

.PHONY: all clean-local run

