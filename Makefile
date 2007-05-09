CXX      = gcc
CXXFLAGS = -x c++ -ansi -Wall -pedantic -ggdb
CPPFLAGS =

target  = test1
obj_sa  = sa/AbstractSA.o sa/BasicExplicitSA.o sa/BasicImplicitSA.o
obj_dm  = dm/AbstractDM.o dm/ArrayDM.o dm/LinkedDM.o
obj_etc = Config.o
objects = $(target).o $(obj_etc) $(obj_sa) $(obj_dm)


.PHONY: clean execs docs format
	
	
execs: $(target)
	
	
clean:
	rm -vf $(objects) $(target)
	
clean-all: clean
	rm -rf build/*
	find . -regextype posix-basic -regex ".*\.orig$$" -printf "removed \`%p'\n" -delete
	
docs:
	mkdir -p build/doc
	doxygen
	
$(target): $(objects) 
	$(CXX) -lstdc++ $(objects) -o $@
	
	
$(objects): %.o: %.cc %.hh
	$(CXX) -c $(CXXFLAGS) $< $(CPPFLAGS) -o $@
	
	
format:
	astyle --style=ansi *.cc *.hh dm/*.cc dm/*.hh sa/*.cc sa/*.hh

