
# LIB_XERCESC
# ------------------------------------------------------------------------------
AC_DEFUN([KP_LIB_XERCESC],[
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
])


# LIB_LOG4CXX
# ------------------------------------------------------------------------------
AC_DEFUN([KP_LIB_LOG4CXX],[
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
])


# PROG_XSD      -- blogas sis testas, kazkuo...
# ------------------------------------------------------------------------------
AC_DEFUN([KP_PROG_XSD], [
    AC_ARG_VAR([XSD], [XML Data Binding for C++ (www.codesynthesis.com)])
    AC_CHECK_PROG([XSD], [xsd], [xsd])
    if test "$XSD" != "xsd"; then
        AC_MSG_ERROR([unable to find xsd (http://www.codesynthesis.com/)])
    fi
])

