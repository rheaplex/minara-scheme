## Process this file with automake to produce Makefile.in

SUBDIRS = bin lisp

dotminaradir = $(datadir)/minara/$(VERSION)/dotminara
dotminara_DATA = dotminara/minara.minara
EXTRA_DIST = $(dotminara_DATA)

## Naive but useful
run:
	MINARA_DATADIR=. guile -L lisp lisp/minara-bootstrap.scm
