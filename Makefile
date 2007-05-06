CXX      = gcc
CXXFLAGS = -x c++ -ansi -Wall -pedantic -ggdb
CPPFLAGS =

target  = test1
obj_sa  = sa/AbstractSA.o sa/BasicExplicitSA.o sa/BasicImplicitSA.o
obj_dm  = dm/AbstractDM.o dm/ArrayDM.o dm/LinkedDM.o
objects = $(target).o Model.o Config.o $(obj_sa) $(obj_dm)


.PHONY: clean execs docs format
	
	
execs: $(target)
	
	
clean:
	rm -f $(objects) $(target)
	
docs:
	mkdir -p build/doc
	doxygen
	
$(target): $(objects) 
	$(CXX) -lstdc++ $(objects) -o $@
	
	
$(objects): %.o: %.cc %.hh
	$(CXX) -c $(CXXFLAGS) $< $(CPPFLAGS) -o $@
	
	
format:
	astyle --style=ansi *.cc *.hh dm/*.cc dm/*.hh sa/*.cc sa/*.hh

