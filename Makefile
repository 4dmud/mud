# tbaMUD Makefile.in - Makefile template used by 'configure'
# Clean-up provided by seqwith.

#ECL?
ECL = 0

# C compiler to use
CC = g++

# Any special flags you want to pass to the compiler
MYFLAGS = -Wall -Wno-sign-compare -Wunused -Wformat -O0 -ggdb -fno-inline
LIBS = -lz -lpthread -lnsl -lm -lc -lcrypt
SRCFILES := $(wildcard *.cpp)
OBJFILES := $(patsubst %.cpp,%.o,$(SRCFILES))
ifeq ($(ECL),1)
	LISPFILES := init.lisp helpers.lisp
	LISPOBJS := $(patsubst %.lisp,%.obj,$(LISPFILES))
	MYFLAGS += -DECL=1 $(shell ecl-config --cflags)
	LIBS += $(shell ecl-config --ldflags)
	OBJFILES += lib4d-lisp.a
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
	ecl -q -s -o $@ -compile $<

lib4d-lisp.a: $(LISPOBJS)
	(for f in $(LISPOBJS);do echo $$f;done)|ecl -shell make-4d-lisp

lisp/lib4d-lisp.a: $(BINDIR)/circle
	cd lisp;rm lisp/lib4d-lisp.a;$(BINDIR)/circle --shell build.lisp

clean:
	rm -f *.o depend
	rm -f lisp/lib4d-lisp.a

# Dependencies for the object files (automagically generated with
# gcc -MM)

depend:
	$(CC) -MM *.cpp > depend

force-look:
	true

-include depend
