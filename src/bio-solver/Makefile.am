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
bin_PROGRAMS = bio-solver

bio_solver_CPPFLAGS = \
    -I../libbiosensor \
    -I../libbiosensor-xml \
    -I../libbiosensor-slv-fd

bio_solver_LDADD = \
    ../libbiosensor/libbiosensor.la \
    ../libbiosensor-xml/libbiosensor-xml.la \
    ../libbiosensor-slv-fd/libbiosensor-slv-fd.la

bio_solver_SOURCES = \
    main.cxx

dist_man1_MANS = bio-solver.man

astyle:
	astyle @ASTYLE_OPTS@ `find  . -name  "*.cxx"` `find  . -name  "*.hxx"`

