#                                               -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.

AC_PREREQ(2.59)
AC_INIT(erlyate, 0.1, [Mikael Magnusson <mikma@users.sourceforge.net>])
AC_CONFIG_SRCDIR([configure.ac])

dnl AC_CANONICAL_HOST
dnl AC_CANONICAL_TARGET

AM_INIT_AUTOMAKE([foreign])

dnl AM_CONFIG_HEADER([config.h])

AC_ARG_VAR([ERL_FLAGS], [Erlang/OTP emulator flags [none]])dnl
AC_ARG_VAR([YATE], [Yate - Yet Another Telephony Engine])dnl
AC_ARG_VAR([YATE_CONFIG], [Yate config script])dnl

# Checks for programs.
AC_PROG_CC
AC_PROG_AWK
dnl AC_PROG_INSTALL
AC_PROG_LN_S
AC_PATH_PROGS([YATE], [yate],,[$PATH])
AC_PATH_PROGS([YATE_CONFIG],[yate-config],,[$PATH])

AC_MSG_CHECKING([for yateconfdir])
defaultconfdir=""
if test "${YATE_CONFIG}" != ""; then
  defaultconfdir=$( ${YATE_CONFIG} --config )
fi
yateconfdir=""
AC_ARG_ENABLE([yateconfdir],
         [AS_HELP_STRING([--enable-yateconfdir],
                [Set Yate config directory manually])],
[
  if test "${enableval}" != "no" -a "${enableval}" != "yes"; then
    yateconfdir=${enableval}
  else
    yateconfdir=${defaultconfdir}
  fi
],[
  if test "${yateconfdir}" == ""; then
    yateconfdir=${defaultconfdir}
  fi
])
AC_MSG_RESULT([${yateconfdir}])

AC_ERLANG_NEED_ERLC
AC_ERLANG_PATH_ERL
AC_ERLANG_SUBST_INSTALL_LIB_DIR
AC_ERLANG_SUBST_ERTS_VER
AC_ERLANG_CHECK_LIB(kernel)
AC_ERLANG_CHECK_LIB(stdlib)
AC_ERLANG_CHECK_LIB(sasl)
AC_ERLANG_CHECK_LIB(inets)
AC_ERLANG_CHECK_LIB(yxa)
AC_ERLANG_CHECK_LIB(sipua)

dnl AC_ERLANG_CHECK_LIB(yxa)

ERLANG_SUBST_LIB_VER(kernel)
ERLANG_SUBST_LIB_VER(stdlib)
ERLANG_SUBST_LIB_VER(sasl)
ERLANG_SUBST_LIB_VER(inets)
ERLANG_SUBST_LIB_VER(yxa)
ERLANG_SUBST_LIB_VER(sipua)

dnl AC_PATH_PROGS([ERLC],[erlc],,[$path])
dnl AC_PATH_PROGS([ERL],[erl],,[$path])
dnl AC_LANG_PUSH(C++)

AC_SUBST(ERL_FLAGS)
AC_SUBST(yateconfdir)

AC_CONFIG_FILES([
		 Makefile
		 yate/Makefile
		 yate/c_src/Makefile
		 yate/src/Makefile
		 yate_demo/Makefile
		 yate_demo/src/Makefile
		 sipclient/Makefile
		 sipclient/src/Makefile
		 releases/Makefile
		])

AC_OUTPUT
