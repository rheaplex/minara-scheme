## Process this file with automake to produce Makefile.in
bin_PROGRAMS = minara
minara_SOURCES = minara_cache.c minara_events.c minara_guile.c minara_main.c minara_rendering.c minara_window.c
AM_CPPFLAGS = -DDATADIR='"$(datadir)"' -DVERSION='"$(PACKAGE_VERSION)"'
AM_CFLAGS =  $(GUILE_CFLAGS)
AM_LDFLAGS =  $(GUILE_LDFLAGS) $(GLUT_CFLAGS) $(GLUT_LIBS) $(pkg-config --libs guile-2.0)
