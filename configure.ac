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


##### Check for XERCES-C - begin
AC_MSG_CHECKING([for xerces-c])
saved_LIBS="$LIBS"
LIBS="-lxerces-c $LIBS"
AC_LANG_PUSH([C++])
AC_LINK_IFELSE(
        [#include <xercesc/util/PlatformUtils.hpp>
         int main() { xercesc::XMLPlatformUtils::Initialize(); return 0; }
        ],
        [AC_DEFINE(HAVE_LIBXERCESC,1,[Define if Xerces-C library was found])
         AC_MSG_RESULT([yes])
        ],
        [LIBS="$saved_LIBS"
         AC_MSG_RESULT([no])
         AC_MSG_ERROR([unable to link with Xerces])
        ])
AC_LANG_POP()
##### Check for XERCES-C - end 

##### Check for LOG4CXX - begin
AC_MSG_CHECKING([for log4cxx])
saved_LIBS="$LIBS"
LIBS="-llog4cxx $LIBS"
AC_LANG_PUSH([C++])
AC_LINK_IFELSE(
        [#include <log4cxx/logger.h>
         int main() { log4cxx::Logger::getRootLogger(); return 0; }
        ],
        [AC_DEFINE(HAVE_LIBLOG4CXX,1,[Define if log4cxx library was found])
         AC_MSG_RESULT([yes])
        ],
        [LIBS="$saved_LIBS"
         AC_MSG_RESULT([no])
         AC_MSG_ERROR([unable to link with log4cxx])
        ])
AC_LANG_POP()
##### Check for LOG4CXX 0 - end



AC_CONFIG_FILES([Makefile
                 src/Makefile
                 src/xsd/Makefile])
AC_OUTPUT
