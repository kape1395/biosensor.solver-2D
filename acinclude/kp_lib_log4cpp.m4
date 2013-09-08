#
# LIB_LOG4CPP (unused)
#
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

