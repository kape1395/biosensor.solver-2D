#
# PROG_XSD (unused and broken)
#
AC_DEFUN([KP_PROG_XSD], [
    AC_ARG_VAR([XSD], [XML Data Binding for C++ (www.codesynthesis.com)])
    AC_CHECK_PROG([XSD], [xsdcxx], [xsdcxx])
    if test "$XSD" != "xsdcxx"; then
        AC_MSG_ERROR([unable to find xsdcxx (http://www.codesynthesis.com/)])
    fi
])

