#
# LIB_XERCESC
#
AC_DEFUN([KP_LIB_XERCESC],[
    AC_MSG_CHECKING([for xerces-c])
    saved_LIBS="$LIBS"
    LIBS="-lxerces-c $LIBS"
    AC_LANG_PUSH([C++])
    AC_LINK_IFELSE(
        [AC_LANG_SOURCE([
            #include <xercesc/util/PlatformUtils.hpp>
            int main() {
                xercesc::XMLPlatformUtils::Initialize();
                return 0;
            }
        ])],
        [AC_DEFINE(HAVE_LIBXERCESC,1,[Define if Xerces-C library was found])
         AC_MSG_RESULT([yes])
        ],
        [LIBS="$saved_LIBS"
         AC_MSG_RESULT([no])
         AC_MSG_ERROR([unable to link with Xerces])
        ])
    AC_LANG_POP()
])


