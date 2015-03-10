################################################################################
#
# File:         Makefile
# RCS:          $Header: $
# Description:  
# Author:       Staal Vinterbo
# Created:      Thu Apr 28 18:03:12 2005
# Modified:     Wed May 11 15:47:39 2005 (Staal Vinterbo) staal@MGV
# Language:     Makefile
# Package:      EDA
# Status:       Experimental 
#
# (c) Copyright 2005, Staal Vinterbo, all rights reserved.
#
# This file is part of EDA.
#
# EDA is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2 of the License, or
# (at your option) any later version.
#
# EDA is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with EDA; if not, write to the Free Software
# Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
#
################################################################################

### Might have to be changed

UNAME = $(shell uname)
LNFLAGS =  -lgmpxx -lgmp
INCLS = -I.


### Can hopefully be left alone

SHELL = /bin/bash
CC=g++
AR = ar cq
RANLIB = ranlib
CFLAGS = -c -O3 -Wall 
DEFS = 

TOPDIR = .
LIB = libeda.a

LIBSOURCES = eda.cc parameter.cc farith.cc
LIBHEADERS = eda.h objective.h parameter.h compose2.h farith.h util.h
LIBOBJS = $(LIBSOURCES:.cc=.o)
LIBDOC = eda.3 eda.txt eda.html
EXTRAFILES = example.cc COPYING.txt
DISTFILES = $(LIBSOURCES) $(LIBHEADERS) Makefile $(LIBDOC) $(EXTRAFILES)

all: $(LIB)

$(LIB): $(LIBOBJS)
	rm -f $(LIB)
	$(AR) $@ $(LIBOBJS)
	$(RANLIB) $@

%.o: %.cc
	$(CC) -o $(<:.cc=.o) $(CFLAGS) $(INCLS) $(DEFS) $<

clean:
	rm -f $(LIBOBJS) Makefile~

dist: $(DISTFILES)
	echo "Distribution of eda, (C) 2005 SAV" > distdate.txt
	echo "Packaged: " `date` >> distdate.txt
	rm -f eda.tgz
	tar cvzf eda.tgz $(DISTFILES) distdate.txt 
	rm -f distdate.txt

example: $(LIB) example.cc
	g++ -o $@ example.cc -I. -Wall -O3 -L. -leda -lgmpxx -lgmp

test: example
	@echo This should compute a increasing ordering of the input
	@echo 3 2 4 1 5, which should be 3 1 0 2 4.
	@echo
	echo 3 2 4 1 5 | ./example

love:
	@echo your place or mine?

eda.txt: eda.3
	nroff -c -man -u0 $< | col -b > $@

eda.html: eda.3
	groff -man -Thtml $< > $@

deps:
	cp Makefile Makefile~
	rm Makefile
	cat Makefile~ | gawk "{print} /^#--Dep/ {exit}" > Makefile
	g++ -MM $(INCLS) $(DEFS) $(LIBSOURCES)  >> Makefile


#--Dependencies do not edit this line, or beyond this line ----
eda.o: eda.cc util.h eda.h objective.h farith.h
parameter.o: parameter.cc parameter.h compose2.h
farith.o: farith.cc farith.h
