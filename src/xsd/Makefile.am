datarootdir = @datarootdir@

BUILT_SOURCES = \
    Model.cc \
    ModelBoundCondition.cc \
    ModelMediumReaction.cc

CLEANFILES = *.cc *.hh

#   noinst_LIBRARIES = libxsd.a
#   libxsd_a_SOURCES = $(BUILT_SOURCES)



Model.cc: Model.xsd ModelBoundCondition.cc ModelMediumReaction.cc
	xsd cxx-tree --generate-serialization \
	    --hxx-suffix .hh --cxx-suffix .cc \
	    --namespace-map http://lt.5grupe.karolis/biosensor/model=xsd::model \
	    --namespace-map http://lt.5grupe.karolis/biosensor/model/bc=xsd::model::bc \
	    --namespace-map http://lt.5grupe.karolis/biosensor/model/mr=xsd::model::mr \
	    --generate-doxygen --generate-polymorphic \
	    Model.xsd

ModelBoundCondition.cc: ModelBoundCondition.xsd
	xsd cxx-tree --generate-serialization \
	    --hxx-suffix .hh --cxx-suffix .cc \
	    --namespace-map http://lt.5grupe.karolis/biosensor/model=xsd::model \
	    --namespace-map http://lt.5grupe.karolis/biosensor/model/bc=xsd::model::bc \
	    --namespace-map http://lt.5grupe.karolis/biosensor/model/mr=xsd::model::mr \
	    --generate-doxygen --generate-polymorphic \
	    ModelBoundCondition.xsd

ModelMediumReaction.cc: ModelMediumReaction.xsd
	xsd cxx-tree --generate-serialization \
	    --hxx-suffix .hh --cxx-suffix .cc \
	    --namespace-map http://lt.5grupe.karolis/biosensor/model=xsd::model \
	    --namespace-map http://lt.5grupe.karolis/biosensor/model/bc=xsd::model::bc \
	    --namespace-map http://lt.5grupe.karolis/biosensor/model/mr=xsd::model::mr \
	    --generate-doxygen --generate-polymorphic \
	    ModelMediumReaction.xsd


