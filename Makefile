# tbaMUD Makefile.in - Makefile template used by 'configure'
# Clean-up provided by seqwith.

# C compiler to use
CC = g++

# Any special flags you want to pass to the compiler
MYFLAGS = -Wall -Wno-sign-compare -Wunused -Wformat -O0 -ggdb -fno-inline

#flags for profiling (see hacker.doc for more information)
PROFILE = 

##############################################################################
# Do Not Modify Anything Below This Line (unless you know what you're doing) #
##############################################################################

BINDIR = ../bin

CPPFLAGS = $(MYFLAGS) $(PROFILE)

LIBS = -lz -lpthread -lnsl -lm -lc -lcrypt

SRCFILES := $(wildcard *.cpp)
OBJFILES := $(patsubst %.cpp,%.o,$(SRCFILES))  

default: all

all:
	$(MAKE) $(BINDIR)/circle

circle:
	$(MAKE) $(BINDIR)/circle

$(BINDIR)/circle : $(OBJFILES)
	$(CC) -o $(BINDIR)/circle $(PROFILE) $(OBJFILES) $(LIBS)

$%.o: %.cpp
	$(CC) $< $(CFLAGS) -c -o $@ 

clean:
	rm -f *.o depend

# Dependencies for the object files (automagically generated with
# gcc -MM)

depend:
	$(CC) -MM *.cpp > depend

-include depend
