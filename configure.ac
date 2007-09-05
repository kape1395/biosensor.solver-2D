AC_PREREQ(2.61)
AC_INIT([kape1395.biosensor.solver-2D], [0.1])
AC_CONFIG_AUX_DIR(admin)
AC_CONFIG_SRCDIR([src/SolveListener.cc])
AM_INIT_AUTOMAKE([-Wall])
AM_CONFIG_HEADER(config.h)

# Checks for programs.
AC_PROG_CXX
AC_PROG_RANLIB
#AC_PROG_LIBTOOL

# Checks for libraries.

# Checks for header files.

# Checks for typedefs, structures, and compiler characteristics.
AC_HEADER_STDBOOL
AC_C_CONST
AC_C_INLINE

# Checks for library functions.



saved_LIBS="$LIBS"
LIBS="-lxerces-c $LIBS"
AC_LANG_PUSH([C++])
AC_LINK_IFELSE(
        [#include <xercesc/util/PlatformUtils.hpp>
         int main() {xercesc::XMLPlatformUtils::Initialize(); return 0; }],
        [AC_DEFINE(HAVE_LIBXERCESC,1,[Define if Xerces-C library was found])],
        [AC_MSG_ERROR([unable to link with Xerces])
         LIBS="$saved_LIBS"
        ])
AC_LANG_POP()



AC_CONFIG_FILES([Makefile
                 src/Makefile
                 src/cfg/Makefile
                 src/dm/Makefile
                 src/sa/Makefile
                 src/xsd/Makefile])
AC_OUTPUT
