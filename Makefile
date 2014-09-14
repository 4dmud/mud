# tbaMUD Makefile.in - Makefile template used by 'configure'
# Clean-up provided by seqwith.

#ECL?
ECL = 1

#if ECL is set, check if it is actually there
#ifeq ($(ECL),1)
#	ECL = $(shell ./detect_ecl.sh)
#endif

# C compiler to use
CC = g++

# Any special flags you want to pass to the compiler
MYFLAGS = -Wall -Wno-sign-compare -Wunused -Wformat -O0 -ggdb3 -fno-inline
LIBS = -lz -lpthread -lnsl -lm -lc -lcrypt
SRCFILES := $(wildcard *.cpp)
OBJFILES := $(patsubst %.cpp,%.o,$(SRCFILES))
ifeq ($(ECL),1)
	MYFLAGS += -DECL=1 $(shell ecl-config --cflags)
	LIBS += $(shell ecl-config --ldflags)
	OBJFILES += lisp/lib4d-lisp.a
endif

#flags for profiling (see hacker.doc for more information)
PROFILE = 

##############################################################################
# Do Not Modify Anything Below This Line (unless you know what you're doing) #
##############################################################################

BINDIR = ../bin

CPPFLAGS = $(MYFLAGS) $(PROFILE)

default: all

all:
	$(MAKE) $(BINDIR)/circle

circle:
	$(MAKE) $(BINDIR)/circle

$(BINDIR)/circle : $(OBJFILES)
	$(CC) -o $(BINDIR)/circle $(PROFILE) $(OBJFILES) $(LIBS)

$%.o: %.cpp
	$(CC) $< $(CFLAGS) -c -o $@ 

%.obj: %.lisp
	echo compiling $< >>lisp-output.txt
	ecl -q -s -o $@ -compile $< >>lisp-output.txt

lisp/lib4d-lisp.a: force-look
	cd lisp;ecl -shell build >>lisp-output.txt

clean:
	rm -f *.o depend
	rm -f *.obj *.fas *.eclh *.data lisp/lib4d-lisp.a lisp/*.fas lisp/*.c lisp/*.o lisp/lisp-output.txt
	find ./ -name \*.o -exec rm {} \;
	find ./ -name \*.fas -exec rm {} \;

# Dependencies for the object files (automagically generated with
# gcc -MM)

#depend:
#	$(CC) -MM *.cpp > depend

force-look:
	true

#-include depend
