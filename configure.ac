AC_PREREQ(2.61)
AC_INIT([biosensor], [0.3.0])
AC_CONFIG_AUX_DIR(admin)
AC_CONFIG_SRCDIR([src/README])
AC_CONFIG_HEADERS([ src/libbiosensor-xml/biosensor-xml.hxx src/libbiosensor/biosensor.hxx src/libbiosensor-slv-fd/biosensor-slv-fd.hxx ])
AM_INIT_AUTOMAKE([subdir-objects -Wall])

ASTYLE_OPTS="--style=ansi --options=none"


#### Checks for programs.
AC_PROG_CXX
AC_PROG_LIBTOOL
KP_PROG_XSD

#### Checks for libraries.
#                          AM_PATH_LOG4CPP(1.0)
KP_LIB_LOG4CXX
KP_LIB_XERCESC

#### Checks for header files.
# ...

#### Checks for typedefs, structures, and compiler characteristics.
AC_HEADER_STDBOOL
AC_C_CONST
AC_C_INLINE

#### Checks for library functions.
# ...


#### Check for parameter ROOTNS
AC_ARG_VAR([ROOTNS], [Root namespace for this package (default is "bio").])
test -z "$ROOTNS" && ROOTNS="bio"
AC_MSG_NOTICE([Using root namespace: $ROOTNS])
AC_DEFINE_UNQUOTED([BIO_NS], [$ROOTNS], [Library namespace.])


AC_SUBST(BIO_NS, [$ROOTNS])
AC_SUBST(ASTYLE_OPTS)
AC_CONFIG_FILES([
    Makefile
    src/Makefile
    src/bio-editor/Makefile
    src/bio-solver/Makefile
    src/libbiosensor/Makefile
    src/libbiosensor-slv-fd/Makefile
    src/libbiosensor-xml/Makefile
])

AC_OUTPUT

