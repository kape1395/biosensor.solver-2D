CXX      = gcc
CXXFLAGS = -x c++ -ansi -Wall -pedantic -ggdb
CPPFLAGS =

target  = test1
obj_sa  = sa/AbstractSA.o sa/BasicExplicitSA.o sa/BasicImplicitSA.o
obj_dm  = dm/AbstractDM.o dm/ArrayDM.o dm/LinkedDM.o
obj_etc = cfg/Config.o SolveListener.o
obj_xsd = xsd/Model.o
objects = $(target).o $(obj_etc) $(obj_sa) $(obj_dm) $(obj_xsd)
libs    = -lstdc++ -lxerces-c


PLATFORM = LINUX


.PHONY: clean execs docs format
	
	
execs: $(target)
	
	
clean:
	rm -vf $(objects) $(target) xsd/Model.cc xsd/Model.hh
	
clean-all: clean
	rm -rf build/*
	find . -regextype posix-basic -regex ".*\.orig$$" -printf "removed \`%p'\n" -delete
	
docs:
	mkdir -p build/doc
	doxygen
	
$(target): $(objects) 
	$(CXX) $(libs) $(objects) -o $@ 
	
	
$(objects): %.o: %.cc %.hh
	$(CXX) -c $(CXXFLAGS) $< $(CPPFLAGS) -o $@
	
	
xsd/Model.cc: xsd/Model.xsd
	xsd cxx-tree --generate-serialization --output-dir xsd \
        --hxx-suffix .hh --cxx-suffix .cc \
        --namespace-map http://lt.5grupe.karolis/biosensor/model=xsd::model \
        xsd/Model.xsd
	sed -i -e 's/ long long / long /' xsd/Model.hh
	
format:
	astyle --style=ansi *.cc *.hh dm/*.cc dm/*.hh sa/*.cc sa/*.hh cfg/*.cc cfg/*.hh
