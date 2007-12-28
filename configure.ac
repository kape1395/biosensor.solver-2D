AC_PREREQ(2.61)
AC_INIT([biosensor], [0.3.0])
AC_CONFIG_AUX_DIR(admin)
AC_CONFIG_SRCDIR([src/README])
AC_CONFIG_HEADERS([config.h])
AM_INIT_AUTOMAKE([subdir-objects -Wall])


#### Checks for programs.
AC_PROG_CXX
AC_PROG_LIBTOOL
KP_PROG_XSD

#### Checks for libraries.
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

