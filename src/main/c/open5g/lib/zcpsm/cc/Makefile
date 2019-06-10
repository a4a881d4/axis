# Generated automatically from Makefile.in by configure.
# Makefile.in generated automatically by automake 1.4 from Makefile.am

# Copyright (C) 1994, 1995-8, 1999 Free Software Foundation, Inc.
# This Makefile.in is free software; the Free Software Foundation
# gives unlimited permission to copy and/or distribute it,
# with or without modifications, as long as this notice is preserved.

# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY, to the extent permitted by law; without
# even the implied warranty of MERCHANTABILITY or FITNESS FOR A
# PARTICULAR PURPOSE.


SHELL = /bin/sh

srcdir = .

DESTDIR =

CC = gcc

LINK = gcc

ALL:zcc

cc86_cc1_SOURCES = zccop.c assign.c declare.c gencode.c label.c preserve.c type.c bcc-cc1.c express.c genloads.c loadexp.c scan.c exptree.c glogcode.c longop.c softop.c codefrag.c floatop.c hardop.c output.c state.c debug.c function.c input.c preproc.c table.c align.h byteord.h condcode.h const.h gencode.h input.h label.h os.h output.h parse.h proto.h reg.h sc.h scan.h sizes.h table.h type.h types.h


DEFS = -DHAVE_CONFIG_H -I. -I$(srcdir) -I..

cc86_cc1_OBJECTS =  zccop.o assign.o declare.o gencode.o label.o preserve.o \
type.o bcc-cc1.o express.o genloads.o loadexp.o scan.o exptree.o \
glogcode.o longop.o softop.o codefrag.o floatop.o hardop.o output.o \
state.o debug.o function.o input.o preproc.o table.o


INCLUDES = 

CFLAGS = -g 
COMPILE = $(CC) $(DEFS) $(INCLUDES) $(AM_CPPFLAGS) $(CPPFLAGS) $(AM_CFLAGS) $(CFLAGS)


SOURCES = $(cc86_cc1_SOURCES)
OBJECTS = $(cc86_cc1_OBJECTS)

.SUFFIXES: .c .o 

.c.o:
	$(COMPILE) -c $<



zcc: $(cc86_cc1_OBJECTS) 
	@rm -f zcc
	$(LINK) $(cc86_cc1_LDFLAGS) $(cc86_cc1_OBJECTS) -o zcc

clean :
	rm -f *.o; rm -f *~; rm -f zcc; rm -f *.bak;

backup :

install: zcc
	cp zcc ../../../../../../../../bin/zcc
	



