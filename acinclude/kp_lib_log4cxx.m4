#
# LIB_LOG4CXX (unused)
#
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

