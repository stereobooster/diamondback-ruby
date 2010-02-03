# Copyright (c) 2007 Christopher R. Waterson
AC_INIT(ocaml-syck, 0.1, waterson@maubi.net)
AC_MSG_RESULT(configuring $PACKAGE_STRING)

# Check for Ocaml compilers
AC_CHECK_PROG(OCAMLFIND,ocamlfind,ocamlfind,no)
if test "$OCAMLFIND" = no ; then
   AC_MSG_ERROR(Cannot find ocamlfind.)
fi

AC_CHECK_PROG(OCAMLDEP,ocamldep,ocamldep)
if test "x$OCAMLDEP" = x ; then
   AC_MSG_ERROR(Cannot find ocamldep.)
fi

AC_CHECK_PROG(OCAMLC,ocamlc,ocamlc)
if test "x$OCAMLC" = x ; then
   AC_MSG_ERROR(Cannot find ocamlc.)
fi

AC_CHECK_PROG(ocamlopt,ocamlopt,ocamlopt,ocamlc)
if test "$ocamlopt" != "ocamlopt" ; then
   AC_MSG_WARN(Cannot find ocamlopt; bytecode compilation only.)
fi

AC_CHECK_PROG(OCAMLMKLIB,ocamlmklib,ocamlmklib)
if test "x$OCAMLMKLIB" = x ; then
   AC_MSG_ERROR(Cannot find ocamlmklib.)
fi

# Turn on debugging?
OCAMLC=ocamlopt
OCAMLDEBUGFLAGS=
OCAMLCFLAGS=
OCAMLLDFLAGS=
LIB=cmxa
OBJ=cmx
AC_ARG_ENABLE([debug],
              AS_HELP_STRING([--enable-debug],
                             [build bytecode with debugging information]))
if test "${enable_debug}" = "yes" || \
   test "x$ocamlopt" = x
then
   if test "$ocamlopt" = "ocamlc" ; then
      AC_MSG_WARN([Building bytecode with debugging information since there is no native compiler])
   fi

   OCAMLC=ocamlc
   OCAMLDEBUGFLAGS="-g -dtypes"
   OCAMLCFLAGS="-ccopt -g"
   OCAMLLDFLAGS="-cclib -g"
   LIB=cma
   OBJ=cmo
fi

# Allow the location for syck.h and libsyck.a to be specified using a
# single "--with-syck" option, which just twiddles the CPPFLAGS and
# LDFLAGS variables.
AC_ARG_WITH([syck],
  [AS_HELP_STRING([--with-syck=DIR], [location for syck])],
  [], [])

if test "x$with_syck" != x ; then
  CPPFLAGS="$CPPFLAGS -I$with_syck/include"
  LDFLAGS="$LDFLAGS -L$with_syck/lib"
fi
                    

AC_PROG_CC
AC_PROG_RANLIB
AC_CHECK_HEADER(syck.h,,AC_MSG_ERROR(Cannot find syck.h.))
AC_CHECK_LIB(syck,syck_new_parser,,AC_MSG_ERROR(Cannot find libsyck))

AC_CHECK_FUNCS(snprintf)

AC_SUBST(CPPFLAGS)
AC_SUBST(CFLAGS)
AC_SUBST(LDFLAGS)
AC_SUBST(OCAMLC)
AC_SUBST(OCAMLDEBUGFLAGS)
AC_SUBST(OCAMLCFLAGS)
AC_SUBST(OCAMLLDFLAGS)
AC_SUBST(LIB)
AC_SUBST(OBJ)
AC_SUBST(HAVE_SNPRINTF)

# Turn on profiling?
AC_ARG_ENABLE([profile],
              AS_HELP_STRING([--enable-gprof],
                             [build executable that can be profiled with gprof]))

OCAMLPROFFLAGS=
if test "${enable_gprof}" = "yes" ; then
   if test "$OCAMLC" = "ocamlc" ; then
      AC_MSG_ERROR(Cannot profile bytecode builds with gprof)
   fi
   OCAMLPROFFLAGS=-p
fi

AC_SUBST(OCAMLPROFFLAGS)
              

# Spew!
AC_CONFIG_FILES([Makefile],[chmod a-w Makefile])
AC_CONFIG_FILES([config.mk],[chmod a-w config.mk])
AC_CONFIG_FILES([yaml/Makefile],[chmod a-w yaml/Makefile])
AC_CONFIG_FILES([yaml/META],[chmod a-w yaml/META])
AC_CONFIG_HEADERS([yaml/config.h],[chmod a-w yaml/config.h])
AC_CONFIG_FILES([t/Makefile],[chmod a-w t/Makefile])
AC_OUTPUT
