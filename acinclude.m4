
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


# LIB_LOG4CPP
# ------------------------------------------------------------------------------
AC_DEFUN([KP_LIB_LOG4CPP],[
    AC_MSG_CHECKING([for log4cpp])
    saved_LIBS="$LIBS"
    LIBS="-llog4cpp -lnsl $LIBS"
    AC_LANG_PUSH([C++])
    AC_LINK_IFELSE(
        [#include <log4cpp/Category.hh>
         int main() {
           log4cpp::Category& cat = log4cpp::Category::getInstance("test");
           cat.error("test test test");
           return 0;
         }
        ],
        [AC_DEFINE(HAVE_LIBLOG4CPP,1,[Define if log4cpp library was found])
         AC_MSG_RESULT([yes])
        ],
        [LIBS="$saved_LIBS"
         AC_MSG_RESULT([no])
         AC_MSG_ERROR([unable to link with log4cpp])
        ])
    AC_LANG_POP()
])


# PROG_XSD      -- blogas sis testas, kazkuo...
# ------------------------------------------------------------------------------
AC_DEFUN([KP_PROG_XSD], [
    AC_ARG_VAR([XSD], [XML Data Binding for C++ (www.codesynthesis.com)])
    AC_CHECK_PROG([XSD], [xsdcxx], [xsdcxx])
    if test "$XSD" != "xsdcxx"; then
        AC_MSG_ERROR([unable to find xsdcxx (http://www.codesynthesis.com/)])
    fi
])

