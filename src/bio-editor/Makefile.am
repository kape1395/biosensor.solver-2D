bin_PROGRAMS = bio-editor
bio_editor_LDADD = \
    ../libbiosensor-xml/libbiosensor-xml.la

bio_editor_SOURCES = \
    main.cxx

dist_man1_MANS = bio-editor.man

astyle:
	astyle @ASTYLE_OPTS@ `find  . -name  "*.cxx"` `find  . -name  "*.hxx"`

