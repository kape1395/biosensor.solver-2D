datarootdir = @datarootdir@

SUBDIRS = cfg dm sa xsd

bin_PROGRAMS = test1
test1_SOURCES = \
	SolveListener.cc \
	test1.cc \
	cfg/Config.cc \
	dm/AbstractDM.cc \
	dm/ArrayDM.cc \
	dm/LinkedDM.cc \
	sa/AbstractSA.cc \
	sa/BasicExplicitSA.cc \
	sa/BasicImplicitSA.cc \
	xsd/Model.cc \
	xsd/ModelBoundCondition.cc \
	xsd/ModelMediumReaction.cc 

test1_LDADD = xsd/libxsd.a

