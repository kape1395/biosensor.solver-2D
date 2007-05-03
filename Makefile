CXX      = gcc
CXXFLAGS = -x c++ -ansi -Wall -pedantic -ggdb
CPPFLAGS =

target  = test1
obj_sa  = sa/Abstract.o sa/BasicExplicit.o sa/BasicImplicit.o
obj_dm  = dm/Abstract.o dm/Array.o dm/Linked.o
objects = $(target).o Model.o Config.o $(obj_sa) $(obj_dm)


.PHONY: clean execs docs
	
	
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
	
	
