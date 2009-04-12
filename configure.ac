AC_INIT([minara], [0.0.3], [rob@robmyers.org])
AC_CONFIG_SRCDIR(src/minara_main.c)
AM_INIT_AUTOMAKE

dnl Get the host, important for MacOS X
AC_CANONICAL_HOST

case $host in
	*darwin*)
		dnl set MacOS X Cocoa GL/GLUT flags
 		: ${with_apple_opengl_framework=yes}
		dnl make sure we play nice with Fink
		;;
	*)
		dnl no extra flags
		;;
esac

dnl Checks for programs.
AC_PROG_CC
     
dnl Checks for libraries.
GUILE_FLAGS
AX_CHECK_GL
AX_CHECK_GLU
AX_CHECK_GLUT

dnl Checks for header files.
AC_HEADER_STDC

dnl Go!
AC_CONFIG_FILES([Makefile src/Makefile lisp/Makefile])
AC_OUTPUT
