AC_INIT([minara], [0.0.3], [rob@robmyers.org])
AC_CONFIG_SRCDIR(src/minara_main.c)
#AC_CONFIG_HEADERS([config.h])

AM_INIT_AUTOMAKE

# Check for programs.
AC_PROG_CXX
AC_PROG_CC
     
# Check for standard libraries, headers and functions.
AC_CHECK_HEADERS([stdlib.h string.h])
AC_HEADER_STDBOOL
AC_FUNC_MALLOC

# Checks for Guile.
GUILE_FLAGS

# Checks for OpenGL.
ACX_PTHREAD
AX_CHECK_GL
AX_CHECK_GLU
# This fails on Fedora 13 if you don't install libXmu-devel and libXi-devel
AX_CHECK_GLUT
#AC_CHECK_HEADERS(GL/gl.h GL/glu.h GL/glut.h)
#AC_CHECK_HEADERS(GL/glext.h,,,
#                 [[#if defined(HAVE_GL_GL_H)
#                 #include <GL/gl.h>
#                 #endif /* HAVE_GL_H */]])

dnl Go!
AC_CONFIG_FILES([Makefile src/Makefile lisp/Makefile])
AC_OUTPUT
