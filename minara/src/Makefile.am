## Process this file with automake to produce Makefile.in
bin_PROGRAMS = minara
minara_SOURCES = minara_buffer.c minara_events.c minara_guile.c minara_main.c minara_rendering.c minara_window.c text.c
##minara_CPPFLAGS =
minara_CFLAGS =  $(GUILE_CFLAGS)
minara_LDFLAGS =  $(GUILE_LDFLAGS) $(GLUT_CFLAGS) $(GLUT_LIBS)
