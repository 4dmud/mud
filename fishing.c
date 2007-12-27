/*
*  C Implementation: %{$MODULE}
*
* Description:
*
*
* Author: %{AUTHOR} <%{EMAIL}>, (C) %{YEAR}
*
* Copyright: See COPYING file that comes with this distribution
*
*/
#include "conf.h"
#include "sysdep.h"

#include "structs.h"
#include "utils.h"
#include "comm.h"
#include "interpreter.h"
#include "handler.h"
#include "db.h"
#include "spells.h"
#include "dg_scripts.h"
#include "constants.h"
#include "dg_event.h"

extern struct time_info_data time_info;
EVENTFUNC(message_event);

int in_fish_room(Character *ch) {
char *nm = NULL;
if (!IN_ROOM(ch))
return 0;
if (!(nm = IN_ROOM(ch)->name))
return 0;

if (
isname("sea", nm) ||
isname("water", nm) ||
isname("harbour", nm) ||
isname("beach", nm) ||
isname("lake", nm) || 
isname("pond", nm) ||
isname("river", nm) ||
isname("stream", nm) ||
isname("moat", nm) ||
isname("swamp", nm) ||
isname("marsh", nm) ||
isname("creek", nm) ||
isname("ocean", nm)
)
{
return 1;
}
return 0;

}
int has_bait(Character *ch) {
struct obj_data *bait;
char *nm;
if ((bait = GET_EQ(ch, POS_HOLD)) == NULL)
return NULL;
if (GET_OBJ_TYPE(bait) != ITEM_FOOD) {
return NULL;
}
if (GET_OBJ_VAL(bait, 1) > 3)
return NULL;
nm = bait->name;
if (
isname("bait", nm) ||
isname("fish", nm) ||
isname("food", nm) ||
isname("filet", nm) ||
isname("crab", nm) || 
isname("meat", nm) ||
isname("worm", nm) ||
isname("shrimp", nm)
)
{
return bait;
}
return NULL;

}

int has_rod(Character *ch) {
struct obj_data *rod;
if (!ch)
return 0;
if ((rod = GET_EQ(ch, POS_WIELD)) == NULL)
return NULL;
/** change this to be of ITEM_ROD mord**/
if (GET_OBJ_VNUM(rod) == 1799)
return rod;
}

ACMD(do_fish) {
char arg[MAX_INPUT_LENGTH];
argument = one_argument(argument, arg);
struct obj_data *bait, *rod;
if (IS_NPC(ch)) {
ch->Send( "mobs can't fish\r\n");
return;
}

if (FIGHTING(ch)) {
ch->Send( "You can't concentrate enough!\r\n");
return;
}

if (*arg) {
if (!str_cmp(arg, "tally") || !str_cmp(arg, "talley")) {
ch->Send( 
"\r\n"
"[   FishObjects: %-5d  ]\r\n"
"[   LiveOnes   : %-5d  ]\r\n"
"[   Debris     : %-5d  ]\r\n", 
TALLY_FOBJ(ch), TALLY_FISH(ch), TALLY_DEBRIS(ch));
}
else
ch->Send( "That option isnt available yet, just type fish, or fish tally\r\n");
return;
}

if (!(rod = has_rod(ch))) {
ch->Send( "You can't fish without a rod or line!\r\n");
return;
}

if (!in_fish_room(ch)) {
ch->Send( "You can't fish here.\r\n");
return;
}

if (!(bait = has_bait(ch))) {
ch->Send( "You are not holding suitable bait!\r\n");
return;
}

/** start the fishing! **/
GET_OBJ_VAL(bait, 1)++;
if (GET_OBJ_VAL(bait, 1) == 4) {
act("You put the last of $p on the hook and drop it in the water.", FALSE, ch, bait, 0, TO_CHAR);
act("$n puts the last bit of $p on the hook and drops it in the water.", FALSE, ch, bait, 0, TO_ROOM);
} else {
act("You put some $p on the hook and drop it in the water.", FALSE, ch, bait, 0, TO_CHAR);
act("$n puts some $p on the hook and drops it in the water.", FALSE, ch, bait, 0, TO_ROOM);
}
}

int fish(Character *ch, int chance, int round) {
ch->Send( "[FISH] ");
if (chance < 0) {
act("You get a few tugs on your line", FALSE, ch, 0, 0, TO_CHAR);
act("$n's line twitches.", FALSE, ch, 0, 0, TO_ROOM);
} else if (chance < 15) {
act("Try as you might, you don't catch a single fish.", FALSE, ch, 0, 0, TO_CHAR);
act("$n doesn't catch a single fish.", FALSE, ch, 0, 0, TO_ROOM);
} else if (chance < 30) {
act("You mumble and sigh as your bait just falls into the water.", FALSE, ch, 0, 0, TO_CHAR);
act("$n's bait seems to have just fallen off.", FALSE, ch, 0, 0, TO_ROOM);
} else if (chance < 100) {
TALLY_FOBJ(ch)++;
if (round == 0) {
act("You get a stronger tug on your line!", FALSE, ch, 0, 0, TO_CHAR);
act("$n gets a strong tug on $s line!", FALSE, ch, 0, 0, TO_ROOM);
} else {
act("You pull up great catch!", FALSE, ch, 0, 0, TO_CHAR);
act("$n pulls up a great catch!", FALSE, ch, 0, 0, TO_ROOM);
/**Fish Objects - hopfully edible **/
           /* if %a% < 40
              %load% ob 2003
            elseif %a% < 50
              %load% ob 2127
            elseif %a% < 60
              %load% ob 6928
            elseif %a% < 70
              %load% ob 7215
            elseif %a% < 80
              %load% ob 12212
            elseif %a% < 90
              %load% ob 12388
            elseif %a% < 100
              %load% ob 13901
            elseif %a% < 110
              %load% ob 13928
            end*/
}

}
/*

set lg [FISH]


          elseif (%a%>30&&%a%<100)
            eval a (30+%random.80%)
            eval fo (1+%actor.fo%)
            remote fo %actor.id%
            %ss%  %lg% You get a bite!
            wait 3s
            %ss%  %lg% You pull up great catch! [%fo%]
            %se%  %lg% %actor.name% pulls up a great catch!
            *Fish Objects - hopfully edible
            if %a% < 40
              %load% ob 2003
            elseif %a% < 50
              %load% ob 2127
            elseif %a% < 60
              %load% ob 6928
            elseif %a% < 70
              %load% ob 7215
            elseif %a% < 80
              %load% ob 12212
            elseif %a% < 90
              %load% ob 12388
            elseif %a% < 100
              %load% ob 13901
            elseif %a% < 110
              %load% ob 13928
            end
            %force% %actor% get fish
          elseif (%a%>100&&%a%<200)
            *Mobs that attack
            eval a (110+%random.59%)
            eval fm (1+%actor.fm%)
            remote fm %actor.id%
            %ss%  %lg% You get a WHOPPER tug!! [%fm%]
            %se%  %lg% %actor.name% gets a WHOPPER tug!!
            wait 3s
            %ss%  %lg% You pull up great catch!
            %se%  %lg% %actor.name% pulls up a great catch!
            if %actor.room%!=%room%
              *%teleport% %actor% %room.vnum%
            end
            if %a% < 120
              %load% mob 2004
              %force% %actor%  give rod shark
              %force% %actor%  kill shark
            elseif %a% < 130
              %load% mob 13424
              %force% %actor%  give rod fish
              %force% %actor%  kill fish
            elseif %a% < 140
              %load% mob 6926
              %force% %actor%  give rod fish
              %force% %actor%  kill fish
            elseif %a% < 150
              %load% mob 18902
              %force% %actor%  give rod fish
              %force% %actor%  kill fish
            elseif %a% < 160
              %load% mob 7218
              %force% %actor%  give rod fish
              %force% %actor%  kill fish
            elseif %a% < 170
              %load% mob 14308
              %force% %actor%  give rod fish
              %force% %actor%  kill fish
              *              elseif %a% < 180
              *                %load% mob 5966
              *                %force% %actor%  kill croc
              *              elseif %a% < 190
              *                %load% mob 5966
              *                %force% %actor%  kill croc
              *              elseif %a% < 200
              *                %load% mob 5975
              *                %force% %actor%  kill croc
              *              elseif %a% < 210
              *                %load% mob 5976
              *                %force% %actor%  kill croc
              *              elseif %a% < 220
              *                %load% mob 2218
              *                *line 128
              *                %force% %actor%  kill squid
              *              elseif %a% < 230
              *                %load% mob 14319
              *                %force% %actor%  kill squid
              *              elseif %a% < 240
              *                %load% mob 16749
              *                %force% %actor%  kill squid
              *              elseif %a% < 250
              *                %load% mob 2006
              *                %force% %actor%  kill dolphin
              *              elseif %a% < 260
              *                %load% mob 2219
              *                %force% %actor%  kill crab
              *                %force% %actor%  give rod crab
            elseif %a% < 270
              %load% mob 2347
              %force% %actor%  give rod crab
              %force% %actor%  kill crab
            elseif %a% < 280
              %load% mob 6902
              %force% %actor%  give rod crab
              %force% %actor%  kill crab
            elseif %a% < 290
              %load% mob 13422
              %force% %actor%  give rod crab
              %force% %actor%  kill crab
            elseif %a% < 300
              %load% mob 14618
              %force% %actor%  give rod crab
              %force% %actor%  kill crab
            elseif %a% < 310
              %load% mob 16741
              %force% %actor%  give rod crab
              %force% %actor%  kill crab
            elseif %a% < 320
              %load% mob 23002
              %force% %actor%  give rod crab
              %force% %actor%  kill crab
            end
          else
            eval fd (1+%actor.fd%)
            remote fd %actor.id%
            eval baited 1
            %ss%  %lg% You get a pathetic tug as something gets tangled in your
line. [%fd%]
            %se%  %lg% %actor.name% gets a pathetic tug as something gets tangle
d in the line.
            wait 2s
            eval psy %random.200%
            if %psy%<10
              eval deb (4800+%random.40%)
            elseif %psy%<20
              eval deb (5004+%random.24%)
            elseif %psy%<30
              eval deb (5600+%random.20%)
            elseif %psy%<35
              eval deb (5622+%random.22%)
            elseif %psy%<40
              eval deb (6016+%random.19%)
            elseif %psy%<50
              eval deb (6410+%random.24%)
            elseif %psy%<60
              eval deb (6711+%random.25%)
            elseif %psy%<67
              eval deb (6922+%random.10%)
            elseif %psy%<80
              eval deb (7007+%random.43%)
            elseif %psy%<88
              eval deb (7209+%random.8%)
            elseif %psy%<95
              eval deb (7700+%random.14%)
            elseif %psy%<104
              eval deb (7900+%random.7%)
            elseif %psy%<111
              eval deb (8700+%random.42%)
            elseif %psy%<120
              eval deb (9200+%random.36%)
            elseif %psy%<135
              eval deb (1100+%random.65%)
            elseif %psy%<145
              eval deb (17400+%random.48%)
            elseif %psy% < 155
              eval deb (17500+%random.19%)
            elseif %psy%<165
              eval deb (19800+%random.30%)
            elseif %psy%<175
              eval deb (21300+%random.32%)
            elseif %psy%<185
              eval deb (21800+%random.39%)
            elseif %psy%<195
              eval deb (22300+%random.58%)
            else
              eval deb (7311+%random.24%)
            end
            %echo% %lg% a piece of debris surfaces.
            %load% obj %deb%
            %force% %actor%  look
            %ss%  %lg% It could be a treasure!
          end
          eval expbon %actor.level%*1250
          *nop %actor.exp(%expbon%)%
        else
          %ss%  %lg% You are not holding any bait!
        end
      else
        %ss%  %lg% You are not holding any bait!
      end
    else
      %ss%  The room that you are in does not have
      %ss%  strong enough water to attract fish.
    end
  end
end
set prompt [FishObjects:%actor.fo%|LiveOnes:%actor.fm%|Debris:%actor.fd%]
if %one%
  %ss%
  %ss%  Tally: %prompt%
  %ss%
end
if %actor.room.vnum%!=%room.vnum%
  *%teleport% %actor% %room.vnum%
end
if %hasbait%
  if %b%
    if (%baited%!=1)
      if !%actor.varexists(countup)%
        eval countup 0
        remote countup %actor.id%
      end
      if %actor.countup%>1
        eval countup 0
        remote countup %actor.id%
        attach 7180 %b.id%
        %force% %actor%  bait %actor.id%
        %ss%  %lg% On the last of your '%b.shortdesc%'.[3/3]
      else
        eval countup 1+%actor.countup%
        remote countup %actor.id%
        %ss%  %lg% You use up some of '%b.shortdesc%'.[%actor.countup%/3]
      end
    else
    end
  end
end
unset a
unset b
unset c
unset baited
unset deb
unset psy
*/
}