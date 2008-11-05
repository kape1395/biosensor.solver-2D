pkglib_LTLIBRARIES = libbiosensor-xml.la
libbiosensor_xml_la_LDFLAGS = -version-info 0:0:0

GENERATED_CXX = Model.cxx ModelBoundCondition.cxx ModelMediumReaction.cxx ModelSolver.cxx
GENERATED_HXX = Model.hxx ModelBoundCondition.hxx ModelMediumReaction.hxx ModelSolver.hxx


libbiosensor_xml_la_SOURCES = $(GENERATED_CXX)
nobase_pkginclude_HEADERS = $(GENERATED_HXX) biosensor-xml.hxx 
nobase_pkgdata_DATA = Model.xsd ModelBoundCondition.xsd ModelMediumReaction.xsd ModelSolver.xsd

CLEANFILES = $(GENERATED_CXX) $(GENERATED_HXX)

XSD_DEF_NSMAP = \
    --namespace-map http://lt.5grupe.karolis/biosensor/model=@BIO_NS@::xml::model \
    --namespace-map http://lt.5grupe.karolis/biosensor/model/bc=@BIO_NS@::xml::model::bc \
    --namespace-map http://lt.5grupe.karolis/biosensor/model/mr=@BIO_NS@::xml::model::mr \
    --namespace-map http://lt.5grupe.karolis/biosensor/model/s=@BIO_NS@::xml::model::s

XSD_DEF_OPTS = cxx-tree \
    --generate-serialization \
    --hxx-suffix .hxx \
    --cxx-suffix .cxx \
    $(XSD_DEF_NSMAP) \
    --generate-doxygen \
    --generate-polymorphic


Model.cxx: Model.xsd ModelBoundCondition.cxx ModelMediumReaction.cxx
	$(XSD) $(XSD_DEF_OPTS) Model.xsd

ModelBoundCondition.cxx: ModelBoundCondition.xsd
	$(XSD) $(XSD_DEF_OPTS) ModelBoundCondition.xsd

ModelMediumReaction.cxx: ModelMediumReaction.xsd
	$(XSD) $(XSD_DEF_OPTS) ModelMediumReaction.xsd

ModelSolver.cxx: ModelSolver.xsd
	$(XSD) $(XSD_DEF_OPTS) ModelSolver.xsd
