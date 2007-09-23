#                                               -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.

AC_PREREQ(2.59)
AC_INIT(eryate, 0.1, [Mikael Magnusson <mikma@users.sourceforge.net>])
AC_CONFIG_SRCDIR([configure.ac])

dnl AC_CANONICAL_HOST
dnl AC_CANONICAL_TARGET

AM_INIT_AUTOMAKE

dnl AM_CONFIG_HEADER([config.h])

AC_ARG_VAR([ERL_FLAGS], [Erlang/OTP emulator flags [none]])dnl
AC_ARG_VAR([YATE], [Yate - Yet Another Telephony Engine])dnl

# Checks for programs.
AC_PROG_CC
AC_PROG_AWK
dnl AC_PROG_INSTALL
AC_PROG_LN_S
AC_ERLANG_NEED_ERLC
AC_ERLANG_PATH_ERL
AC_ERLANG_SUBST_INSTALL_LIB_DIR
AC_ERLANG_CHECK_LIB(kernel)
AC_ERLANG_CHECK_LIB(stdlib)
AC_ERLANG_CHECK_LIB(sasl)
AC_ERLANG_CHECK_LIB(inets)
AC_ERLANG_CHECK_LIB(sipua)
AC_PATH_PROGS([YATE], [yate],,[$PATH])

dnl AC_ERLANG_CHECK_LIB(yxa)

AC_ERLANG_LIB_VER(kernel)
AC_ERLANG_LIB_VER(stdlib)
AC_ERLANG_LIB_VER(sasl)
AC_ERLANG_LIB_VER(inets)
AC_ERLANG_LIB_VER(sipua)

dnl AC_PATH_PROGS([ERLC],[erlc],,[$path])
dnl AC_PATH_PROGS([ERL],[erl],,[$path])
dnl AC_LANG_PUSH(C++)

AC_SUBST(ERL_FLAGS)

AC_CONFIG_FILES([
		 Makefile
		 include/Makefile
		 yate/Makefile
		 yate/include/Makefile
		 yate/c_src/Makefile
		 yate/src/Makefile
		 yate_demo/Makefile
		 yate_demo/src/Makefile
		 sipclient/Makefile
		 sipclient/src/Makefile
		 releases/Makefile
		])

AC_OUTPUT
