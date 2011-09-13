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
SUBDIRS = src
ACLOCAL_AMFLAGS = -I acinclude

#AM_LDLFAGS=$(AM_LDLFAGS) $(BOOST_FILESYSTEM_LIB) $(BOOST_LDFLAGS) -lboost_filesystem

EXTRA_DIST = \
    Doxyfile \
    valgrind.supp \
    doc/Requirements.odt \
    doc/UML.xmi

#
#dist_bin_SCRIPTS = bin/bio-output
#

doxygen:
	doxygen

test-solver: src/bio-solver/bio-solver
	src/bio-solver/bio-solver src/libbiosensor-xml/Model-example-2D.xml 

test-solver-mem:
	valgrind --suppressions=valgrind.supp local_install/bin/bio-solver  src/libbiosensor-xml/Model-example-2D.xml

astyle:
	make -C src/libbiosensor astyle
	make -C src/libbiosensor-slv-fd astyle
	make -C src/bio-solver astyle
	make -C src/bio-editor astyle
