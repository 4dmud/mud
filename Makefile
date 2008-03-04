# Generated automatically from Makefile.in by configure.
# CircleMUD 3.0 Makefile.in - Makefile template used by 'configure'
#

# C compiler to use
# CC = insure
CC = g++


# Path to cxref utility
CXREF = cxref

# Any special flags you want to pass to the compiler
MYFLAGS = -Wall -Wno-sign-compare -Wunused -Wformat -Wno-conversion 
#-D NO_MAKEFILE
# -Wnested-externs -Wshadow -m32 -Wstrict-prototypes 
# -I/usr/src/lib/libcrypt/ 

#flags for profiling (see hacker.doc for more information)
PROFILE = 
#-fprofile-arcs
#gprof for profiling -pg

##############################################################################
# Do Not Modify Anything Below This Line (unless you know what you're doing) #
##############################################################################
BINDIR = ../bin

CFLAGS = 
#-O2
CPPFLAGS = -O1 -ggdb $(MYFLAGS) $(PROFILE)

LIBS =  -lcrypt -lz -lm -lc -lnsl
#-lpthread


OBJFILES = act.comm.o act.create.o act.informative.o act.item.o \
	act.movement.o act.offensive.o act.other.o act.social.o \
	act.wizard.o action.o aedit.o alias.o arena.o asciimap.o assemblies.o \
	assedit.o auction.o bsd-snprintf.o ban.o boards.o calender.c cali.o cedit.o clan.o \
	class.o color.o comm.o config.o constants.o context_help.o corpse.o character.o \
	col_string.o chcreate.o compressor.o \
	debug.o descriptor.o \
	damage.o db.o dg_comm.o dg_db_scripts.o dg_event.o dg_handler.o dg_misc.o \
	dg_mobcmd.o dg_objcmd.o dg_scripts.o dg_triggers.o dg_variables.o dg_wldcmd.o \
	dg_olc.o event.o familiar.o fight.o \
	gamble.o graph.o genmob.o genobj.o genolc.o genshp.o genwld.o \
	genzon.o handler.o house.o htree.o improved-edit.o \
	ignore.o interpreter.o ident.o kalten.o limits.o lockers.o \
	mapper.o mapmaker.o mine.o magic.o mail.o mxp.o \
	math.o mobact.o modify.o molly.o mordecai.o medit.o note.o objsave.o oedit.o \
	oasis_copy.o oasis_delete.o oasis_list.o oasis.o pk.o playerindex.o proffessions.o \
        qic.o race.o random.o regen.o remort.o redit.o romance.o room.o \
	shop.o sedit.o skills.o spec_assign.o \
	spec_procs.o spell_parser.o subskills.o spells.o string.o cppstring.o strutil.o task.o trees.o \
	utils.o vehicle.o vehicle_edit.o weather.o 4d_hedit.o tedit.o zedit.o 
# dlib/all_console.o

CXREF_FILES = act.comm.c act.create.c act.informative.c act.item.c \
	act.movement.c act.offensive.c act.other.c act.social.c \
	act.wizard.c action.c aedit.c alias.c arena.c asciimap.c  assemblies.c \
	assedit.c auction.c bsd-snprintf.c ban.c boards.c calender.c cali.c cedit.c clan.c \
	class.c color.c comm.c config.c constants.c context_help.c corpse.c character.c \
	col_string.cpp chcreate.c compressor.cpp \
	descriptor.cpp \
	damage.c db.cpp dg_comm.c dg_db_scripts.c dg_event.c dg_handler.c dg_misc.c \
	dg_mobcmd.c dg_objcmd.c dg_scripts.c dg_triggers.c dg_variables.c dg_wldcmd.c \
	dg_olc.c event.c familiar.c fight.c \
	gamble.c graph.c genmob.c genobj.c genolc.c genshp.c genwld.c \
	genzon.c handler.c house.c htree.c improved-edit.c\
	ignore.c interpreter.cpp ident.c kalten.c limits.c lockers.o \
	mapper.c mapmaker.c mine.c magic.c mail.c mxp.cpp \
	math.c mobact.c modify.c molly.c mordecai.c medit.c note.c objsave.c oedit.c \
	oasis_copy.c oasis_delete.c oasis_list.c oasis.c pk.c playerindex.cpp proffessions.c qic.c \
	race.c random.c regen.c remort.c redit.c romance.c room.cpp \
	shop.c sedit.c skills.c spec_assign.c \
	spec_procs.c spell_parser.c subskills.c spells.c string.c \
	cppstring.cpp strutil.cpp  task.c trees.c \
	utils.c vehicle.c vehicle_edit.c weather.c 4d_hedit.c tedit.c zedit.c

SRC = *.cpp *.c 
#dlib/all_console.cpp

INC = *.h


default: all

all: 
	$(MAKE) $(BINDIR)/circle 



utils: 
	(cd util; $(MAKE) all)
circle:
	$(MAKE) $(BINDIR)/circle

proper:
	rm -f *.o *.orig *.rej *.c~ *.h~ depend

# Check in all files
checkin:
	ci -l $(SRC) $(INC)

# Make a unified diff for all files
diff:
	rcsdiff -u $(SRC) $(INC) > diff.new
	gzip diff.new

$(BINDIR)/circle : $(OBJFILES)
	$(CC) -o $(BINDIR)/circle $(PROFILE) $(OBJFILES) $(LIBS)

clean:
	rm -f *.o

# Dependencies for the object files (automagically generated with
# gcc -MM)
depend:
	$(CC) -MM $(SRC) > depend

-include depend
