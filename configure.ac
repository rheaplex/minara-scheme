AC_INIT([minara], [0.4.0], [rob@robmyers.org])
AC_CONFIG_SRCDIR(lisp/minara-bootstrap.scm)

AM_INIT_AUTOMAKE

# Checks for Guile.
GUILE_FLAGS

dnl Go!
AC_CONFIG_FILES([Makefile
                 bin/Makefile
                 lisp/Makefile
                 lisp/minara/Makefile
                 lisp/minara-internal/Makefile
			     lisp/scripts/Makefile
                 lisp/tools/Makefile
                 bin/minara
                 lisp/minara/config.scm],
                 [chmod +x bin/minara])
AC_OUTPUT
