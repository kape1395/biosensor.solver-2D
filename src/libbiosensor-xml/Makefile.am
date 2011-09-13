pkglib_LTLIBRARIES = libbiosensor-xml.la
libbiosensor_xml_la_LDFLAGS = -version-info 0:0:0

GENERATED_CXX = Model.cxx ModelBound.cxx ModelReaction.cxx ModelSolver.cxx ModelTransducer.cxx
GENERATED_HXX = Model.hxx ModelBound.hxx ModelReaction.hxx ModelSolver.hxx ModelTransducer.hxx


libbiosensor_xml_la_SOURCES = $(GENERATED_CXX)
nobase_pkginclude_HEADERS = $(GENERATED_HXX) biosensor-xml.hxx 
nobase_dist_pkgdata_DATA = Model.xsd ModelBound.xsd ModelReaction.xsd ModelSolver.xsd ModelTransducer.xsd

CLEANFILES = $(GENERATED_CXX) $(GENERATED_HXX)

XSD_DEF_NSMAP = \
    --namespace-map http://karolis.5grupe.lt/biosensor/model=@BIO_NS@::xml::model \
    --namespace-map http://karolis.5grupe.lt/biosensor/model/bound=@BIO_NS@::xml::model::bound \
    --namespace-map http://karolis.5grupe.lt/biosensor/model/reaction=@BIO_NS@::xml::model::reaction \
    --namespace-map http://karolis.5grupe.lt/biosensor/model/solver=@BIO_NS@::xml::model::solver \
    --namespace-map http://karolis.5grupe.lt/biosensor/model/transducer=@BIO_NS@::xml::model::transducer

XSD_DEF_OPTS = cxx-tree \
    --generate-serialization \
    --hxx-suffix .hxx \
    --cxx-suffix .cxx \
    $(XSD_DEF_NSMAP) \
    --generate-doxygen \
    --generate-polymorphic \
    --polymorphic-type-all


Model.cxx: Model.xsd ModelBound.cxx ModelReaction.cxx
	$(XSD) $(XSD_DEF_OPTS) Model.xsd

ModelBound.cxx: ModelBound.xsd
	$(XSD) $(XSD_DEF_OPTS) ModelBound.xsd

ModelReaction.cxx: ModelReaction.xsd
	$(XSD) $(XSD_DEF_OPTS) ModelReaction.xsd

ModelSolver.cxx: ModelSolver.xsd
	$(XSD) $(XSD_DEF_OPTS) ModelSolver.xsd

ModelTransducer.cxx: ModelTransducer.xsd
	$(XSD) $(XSD_DEF_OPTS) ModelTransducer.xsd

