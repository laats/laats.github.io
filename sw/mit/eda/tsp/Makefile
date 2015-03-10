################################################################################
#
# File:         Makefile
# RCS:          $Header: $
# Description:  Makefile for tsp
# Author:       Staal Vinterbo
# Created:      Mon Nov 16 10:35:44 1998
# Modified:     Wed May 11 15:52:32 2005 (Staal Vinterbo) staal@MGV
# Language:     Makefile
# Package:      N/A
# Status:       Experimental (Do Not Distribute)
#
# (c) Copyright 2005, Staal Vinterbo, all rights reserved.
#
# This file is part of TSP.
#
# TSP is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2 of the License, or
# (at your option) any later version.
#
# TSP is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with TSP; if not, write to the Free Software
# Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
#
################################################################################

### Might have to be changed

UNAME = $(shell uname)
LNFLAGS = -L.. -leda -lgmpxx -lgmp
INCLS = -I. -I..


### Can hopefully be left alone

SHELL = /bin/bash
CC=g++
AR = ar cq
RANLIB = ranlib
CFLAGS = -c -O3 -Wall 
DEFS = 

TOPDIR = .
PROG = tsp

PROGSOURCES = tspmain.cc tsp.cc 
PROGHEADERS = tsp.h version.h
PROGOBJS = $(PROGSOURCES:.cc=.o)
PROGDOC = tsp.1 tsp.txt tsp.html
EXTRAFILES = example.input COPYING.txt
DISTFILES = $(PROGSOURCES) $(PROGHEADERS) Makefile $(PROGDOC)\
	    build.h $(EXTRAFILES)

all: $(PROG)

$(PROG): $(PROGOBJS)
	$(CC) -o $@ $(PROGOBJS) $(LNFLAGS)
	cat build.h | perl -pe 's/([0-9]+)(?{$$i=$$^N+1})/$$i/g' > build.h.tmp
	mv build.h.tmp build.h
	touch $(PROGOBJS) $(PROG)

%.o: %.cc
	$(CC) -o $(<:.cc=.o) $(CFLAGS) $(INCLS) $(DEFS) $<

clean:
	rm -f $(PROGOBJS) Makefile~

dist: $(DISTFILES)
	echo "Distribution of tsp, (C) 2005 SAV" > distdate.txt
	echo "Packaged: " `date` >> distdate.txt
	rm -f tsp.tgz
	tar cvzf tsp.tgz $(DISTFILES) distdate.txt 
	rm -f distdate.txt
love:
	@echo your place or mine?

tsp.txt: tsp.1
	nroff -c -man -u0 $< | perl -pe 's/.\x8//g' > $@

tsp.html: tsp.1
	groff -man -Thtml $< > $@

test: $(PROG) example.input
	./$(PROG) -v -l --logint 1 -n 10 -s 2 < ./example.input

deps:
	cp Makefile Makefile~
	rm Makefile
	cat Makefile~ | gawk "{print} /^#--Dep/ {exit}" > Makefile
	g++ -MM $(DEFS) $(PROGSOURCES)  >> Makefile


#--Dependencies do not edit this line, or beyond this line ----
tspmain.o: tspmain.cc tsp.h version.h build.h
tsp.o: tsp.cc tsp.h
