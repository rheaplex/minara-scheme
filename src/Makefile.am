## Process this file with automake to produce Makefile.in
bin_PROGRAMS = minara
minara_SOURCES = minara_cache.c minara_events.c minara_guile.c minara_main.c minara_rendering.c minara_window.c minara_menu.c
##minara_CPPFLAGS =
minara_CFLAGS =  $(GUILE_CFLAGS)
minara_LDFLAGS =  $(GUILE_LDFLAGS) $(GLUT_CFLAGS) $(GLUT_LIBS) -lguile
