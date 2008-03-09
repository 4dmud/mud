/*************************************************************************
*   File: act.other.c                                   Part of CircleMUD *
*  Usage: Miscellaneous player-level commands                             *
*                                                                         *
*  All rights reserved.  See license.doc for complete information.        *
*                                                                         *
*  Copyright (C) 1993, 94 by the Trustees of the Johns Hopkins University *
*  CircleMUD is based on DikuMUD, Copyright (C) 1990, 1991.               *
************************************************************************ */
/*
 * $Log: act.other.c,v $
 * Revision 1.42  2007/11/18 06:50:38  w4dimenscor
 * Fixed the bug where you could dig up any buried object in the mud. Removed all threadding from the code to stop the freezes.
 *
 * Revision 1.41  2007/08/23 20:41:29  w4dimenscor
 * - Created a new MudException class, so that we can try and throw and catch errors.
 * - Fixed room description editing in OLC so that it works with the new system.
 * - Removed called to ident.c from the code
 * - changed the hostname values on descriptors and characters from char arrays to strings.
 *
 * Revision 1.40  2007/08/19 01:06:10  w4dimenscor
 * - Changed the playerindex to be a c++ object with search functions.
 * - changed the room descriptions to be searched from a MAP index, and
 * added Get and Set methods for room descriptions.
 * - changed the zone reset so that it doesnt search the entire object list
 * to find the object to PUT things into.
 * - rewrote other parts of the zone reset function, to make it give correct errors.
 * - rewrote the parts of the code to do with loading and searching for directorys and files.
 * - added a new dlib library.
 *
 * Revision 1.39  2007/06/26 10:48:05  w4dimenscor
 * Fixed context in scripts so that it works again, changed mounted combat so that it is about 2/3rds player one third mount damage, updated the way skills get read using total_chance, stopped things with a PERC of 0 assisting, made it so that the ungroup command disbanded charmies
 *
 * Revision 1.38  2006/09/20 09:53:31  w4dimenscor
 * Fixed issue where players starting combat with a spell would not attack, and visa versa
 *
 * Revision 1.37  2006/09/15 08:01:11  w4dimenscor
 * Changed a large amount of send_to_char's to ch->Send and d->Output. fixed namechange command
 *
 * Revision 1.36  2006/08/31 10:39:16  w4dimenscor
 * Fixe dthe crash bug in medit. and also changed the mob proto list. there is still a memory leak in medit, which is being fixed now
 *
 * Revision 1.35  2006/08/25 06:39:43  w4dimenscor
 * fixed the way skills would be deleted when you quit
 *
 * Revision 1.34  2006/08/23 09:01:26  w4dimenscor
 * Changed some of the std::vectors to std::map, killlist, and the lookup tables for id nums
 *
 * Revision 1.33  2006/08/18 11:09:58  w4dimenscor
 * updated some clan functions to use vectors instead of malloccing memory, and also sorted clan lists and updated their layout
 *
 * Revision 1.32  2006/08/13 06:26:50  w4dimenscor
 * New branch created, most arrays in game converted to vectors, and the way new zones are created, many conversions of structs to classes
 *
 * Revision 1.31  2006/06/21 09:28:57  w4dimenscor
 * Added the ability for Mortals of imms to listen to the wizchat. it is a
 * flag with the name wizmort, so set player wizmort on should do the
 * trick.
 *
 * Revision 1.30  2006/06/19 06:25:39  w4dimenscor
 * Changed the player saved mount feature so that all players can load mounts from houses
 *
 * Revision 1.29  2006/06/16 10:54:51  w4dimenscor
 * Moved several functions in fight.c into the Character object. Also removed all occurances of send_to_char from skills.c
 *
 * Revision 1.28  2006/06/11 10:10:11  w4dimenscor
 * Created the ability to use characters as a stream, so that you can do things like: *ch << "You have " << GET_HIT(ch) << "hp.\r\n";
 *
 * Revision 1.27  2006/05/30 09:14:19  w4dimenscor
 * rewrote the color code, process_output, and vwrite_to_output so that they use strings and have better buffer checks
 *
 * Revision 1.26  2006/05/22 10:50:48  w4dimenscor
 * Created 3 new files, mxp.cpp, mxp.h and descriptor.cpp
 * struct descriptor_data has been converted to class Descriptor
 *
 * Revision 1.25  2006/05/21 11:02:25  w4dimenscor
 * converted game from being C code to C++
 * to use new_send_to_char(ch, 'blah') now, you use ch->Send('Blah')
 *
 * Revision 1.24  2006/05/11 06:39:12  w4dimenscor
 * Added comments to some files, changed the width of the name field in the typo and bug reports
 *
 * Revision 1.23  2006/05/11 06:20:02  w4dimenscor
 * Altered the list of typos to fix in act.comm.c, added room vnum to the typo and bug mud log
 *
 * Revision 1.22  2006/05/01 11:29:26  w4dimenscor
 * I wrote a typo checker that automaticly corrects typos in the comm channels. I have also been fixing shadowed variables. There may be residual issues with it.
 *
 * Revision 1.21  2006/04/30 08:14:59  w4dimenscor
 * added a replace string function, using it to remove html formatting from lines in the 'file' command
 *
 * Revision 1.20  2006/04/29 07:19:48  w4dimenscor
 * changed bugs and typos to be saved in tables
 *
 * Revision 1.18  2006/04/27 08:57:26  w4dimenscor
 * updated the typo and bug files to write using DIV and SPAN, so that the data can be formatted in a browser.
 *
 * Revision 1.17  2006/04/03 23:31:35  w4dimenscor
 * Added new commands called pclean, it removes the files of anyone who is not in the player index from the lib directory.
 *
 * Revision 1.16  2006/03/13 19:07:40  w4dimenscor
 * Added a toggle for autogroup so you dont type Y to accept people in your group, and a commandthat lets you split involvement evenly, involve even
 *
 * Revision 1.15  2006/02/17 22:19:54  w4dimenscor
 * Fixed error for ubuntu that doesnt like empty array declarations, moved ice shield to a better place and fixed its messages, added auto auction fixes, allowed mounts to gain exp properly
 *
 * Revision 1.14  2006/01/23 05:23:19  w4dimenscor
 * sorry self. another. _cant remember the changes_ entry
 *
 * Revision 1.13  2005/10/30 08:37:05  w4dimenscor
 * Updated compare command and fixed mining
 *
 * Revision 1.12  2005/10/23 13:53:30  w4dimenscor
 * Added thotters login/logout message concept
 *
 * Revision 1.11  2005/08/07 04:12:39  w4dimenscor
 * Manu changes and command have been made, sorry for the lack of description. Main changes include command landscape, fixes to helpfile stuff, subskill fixes
 *
 * Revision 1.10  2005/04/23 12:18:12  w4dimenscor
 * Fixed some buffer read errors in the fread_string function, also fixed (temp) an index search issue for real_trigger()
 *
 * Revision 1.9  2005/03/23 15:23:13  w4dimenscor
 * Added toggle rp. roleplaying toggle is shown on:
 * - who list
 * - ooc
 * - room description of char
 * - tell
 * - prompt
 *
 * Revision 1.8  2005/03/17 10:41:59  w4dimenscor
 * fixed bug: nogat toggles nosum instead of nogat
 *
 * Revision 1.7  2005/02/28 15:07:55  w4dimenscor
 * fixed messages for noct and afktell
 *
 * Revision 1.6  2005/02/25 05:02:45  w4dimenscor
 * added new commands and a few little changes - i forget what eek
 *
 * Revision 1.5  2005/02/20 00:20:48  w4dimenscor
 * now extracting chars are removed from chairs, and throttling checks for victim
 *
 * Revision 1.4  2005/02/04 20:46:11  w4dimenscor
 * Many changes - i couldn't connect to this for a while
 *
 * Revision 1.3  2004/12/05 09:46:52  w4dimenscor
 * fixed mtransform, fixed format in clan tell, and added limit on magic items carried, and lowered weight of magic items, and increased cost
 *
 * Revision 1.2  2004/11/17 14:19:46  w4dimenscor
 * added Aggro mode to attack everything in sight and 'kill all' command
 *
 * Revision 1.1.1.1  2004/11/12 02:16:45  w4dimenscor
 * Initial clean submission of 4Dimensions src code
 *
 * Revision 1.39  2004/09/18 04:42:45  molly
 * cleared up some memory leaks again, possibly fixed the QIC miscounts
 *
 * Revision 1.37  2004/09/04 03:46:50  molly
 * made it so only one cost for recovering corpses, and skillist is sorted
 *
 * Revision 1.36  2004/08/15 01:12:23  molly
 * aqdded logging to several files, fixed error in the setting of immtitles. fixed typo in busy
 *
 */
#define __ACT_OTHER_C__

#include "conf.h"
#include "sysdep.h"

#include "structs.h"
#include "utils.h"
#include "comm.h"
#include "interpreter.h"
#include "handler.h"
#include "db.h"
#include "spells.h"
#include "screen.h"
#include "house.h"
#include "constants.h"
#include "dg_scripts.h"
#include "descriptor.h"

/* extern variables */
extern Descriptor *descriptor_list;
extern Character *ch_selling;
extern char *class_abbrevs[];
extern const int xap_objs;

/* extern procedures */
void list_skills(Character *ch, int skillspell);
void appear(Character *ch);
void perform_immort_vis(Character *ch);
SPECIAL(shop_keeper);
ACMD(do_gen_comm);
void die(Character *ch, Character *killer);
void Crash_rentsave(Character *ch, int cost);
void write_poofs(Character *ch);
void improve_skill(Character *ch, int skill);
void stop_auction(int type, Character *ch);
void add_follower(Character *ch, Character *leader);
void raw_kill(Character *ch, Character *killer);
void ReplaceString ( char * str, const char *search, const char *replace , size_t len);


/* local functions */
int perform_group(Character *ch, Character *vict);
void print_group(Character *ch);
ACMD(do_quit);
ACMD(do_save);
ACMD(do_not_here);
ACMD(do_practice);
ACMD(do_visible);
ACMD(do_title);
ACMD(do_group);
ACMD(do_ungroup);
ACMD(do_report);
ACMD(do_split);
ACMD(do_use);
ACMD(do_wimpy);
ACMD(do_display);
ACMD(do_gen_write);
ACMD(do_gen_tog);
ACMD(do_file);
ACMD(do_follow);
ACMD(do_die);
ACMD(do_loginmsg);    /*THOTTER EDIT!!! */
ACMD(do_logoutmsg);   /*THOTTER EDIT!!! */
C_FUNC(allow_follow);


ACMD(do_quit)
{
  if (IS_NPC(ch) || !ch->desc)
    return;

  if (subcmd != SCMD_QUIT && GET_LEVEL(ch) < LVL_HERO)
    ch->Send("You have to type quit--no less, to quit!\r\n");
  else if (GET_POS(ch) == POS_FIGHTING)
    ch->Send("No way!  You're fighting for your life!\r\n");
  else if (GET_POS(ch) < POS_STUNNED)
  {
    ch->Send("You die before your time...\r\n");
    die(ch, NULL);
  }
  else
  {
    if (IN_ROOM(ch) != NULL && GET_LEVEL(ch) < LVL_GOD)
    {
      SET_BIT_AR(PLR_FLAGS(ch), PLR_LOADROOM);
      GET_LOADROOM(ch) = GET_ROOM_VNUM(IN_ROOM(ch));
    }

    if (!GET_INVIS_LEV(ch))
    {
      if (GET_LOGOUTMSG(ch)==NULL)                                             /*Thotter edit */
        act("$n has left the game.", TRUE, ch, 0, 0, TO_ROOM);              /*Thotter edit*/
      else
        send_to_room(IN_ROOM(ch), "%s\r\n",  GET_LOGOUTMSG(ch));

    }
    new_mudlog( NRM, MAX(LVL_GOD, GET_INVIS_LEV(ch)), TRUE, "%s has quit the game [%s].", GET_NAME(ch), ch->desc->host.c_str());
    ch->Send("Goodbye, %s.. Come back soon!\r\n", GET_NAME(ch));

    /*  We used to check here for duping attempts, but we may as well
         *  do it right in extract_char(), since there is no check if a
         *  player rents out and it can leave them in an equally screwy
         *  situation.
         */

    Crash_rentsave(ch, 0);

    /* If someone is quitting in their house, let them load back here. */
    if (!PLR_FLAGGED(ch, PLR_LOADROOM) && ROOM_FLAGGED(IN_ROOM(ch), ROOM_HOUSE))
      GET_LOADROOM(ch) = GET_ROOM_VNUM(IN_ROOM(ch));

    extract_char(ch);         /* Char is saved before extracting. */
    make_wholist();
  }
}

ACMD(do_ground)
{
  int landed = FALSE, i;
  struct obj_data *obj;
  if (affected_by_spell(ch, SPELL_FLIGHT))
  {
    affect_from_char(ch, SPELL_FLIGHT);
    landed = TRUE;
  }

  for (i = 0; i < NUM_WEARS; i++)
  {
    if (HAS_BODY(ch, i) && (GET_EQ(ch, i)) &&
        (GET_OBJ_TYPE(GET_EQ(ch, i)) == ITEM_WINGS))
    {
      obj = unequip_char(ch, i);
      obj_to_char(obj, ch);
      act("$n detaches $p from $s body.", FALSE, ch, obj, NULL, TO_ROOM);
      act("You detach $p from your body.", FALSE, ch, obj, NULL, TO_CHAR);
      landed = TRUE;
    }
  }

  if (landed == TRUE)
  {
    act("You find yourself grounded firmly again.", FALSE, ch, NULL, NULL, TO_CHAR);
    act("$n kisses the ground seeming happy to be back on it.", FALSE, ch, NULL, NULL, TO_ROOM);
  }
  else
  {
    act("You seem to be on the ground already!", FALSE, ch, NULL, NULL, TO_CHAR);
  }
}

ACMD(do_die)
{
  if (IS_NPC(ch) || !ch->desc)
    return;
  if (PLR_FLAGGED(ch, PLR_DYING))
  {
    ch->Send( "See you next lifetime...\r\n");
    raw_kill(ch, NULL);
  }
  else
  {
    ch->Send( "You are far to healthy! Get deader!\r\n");
  }
}

void check_for_dead(void)
{
  Descriptor *d, *next_d;
  time_t curr = time(0);
  for (d = descriptor_list; d; d = next_d)
  {
    next_d = d->next;
    if (!IS_PLAYING(d))
      continue;
    if (!PLR_FLAGGED(d->character, PLR_DYING))
      continue;
    if (DIE_TIME(d->character) < (curr - 60))
      die(d->character, NULL);
  }
}

int allowed_loginmsg(Character *ch)
{
  if (GET_LEVEL(ch) > LVL_HERO)
    return TRUE;

  if (!PLR_FLAGGED(ch, PLR_ROLEPLAYER))
    return FALSE;

  if (PLR_FLAGGED(ch, PLR_HERO))
    return TRUE;
  if (PLR_FLAGGED(ch, PLR_RP_LEADER))
    return TRUE;

  if (GET_AWARD(ch) >= 250)
    return TRUE;

  return FALSE;

}
#define MAX_LOGINMSG_LENGTH 80
#define MAX_LOGOUTMSG_LENGTH 80
ACMD(do_loginmsg)
{
  skip_spaces(&argument);
  delete_doubledollar(argument);
  if (!allowed_loginmsg(ch))
  {
    ch->Send( "Sorry, but you don't deserve a logout message yet.\r\n");
    return;
  }
  if (strlen(argument) > MAX_LOGINMSG_LENGTH)
    ch->Send( "Sorry, but your login message can't be longer then %d characters.\r\n",
                     MAX_LOGINMSG_LENGTH);
  else
  {
    set_loginmsg(ch, argument);
    if(GET_LOGINMSG(ch)==NULL)
      ch->Send( "Ok, you don't have a login message anymore.\r\n");
    else
      ch->Send( "Your new loginmsg is: %s\r\n", GET_LOGINMSG(ch));
  }
  return;
}

ACMD(do_logoutmsg)
{
  skip_spaces(&argument);
  delete_doubledollar(argument);
  if (!allowed_loginmsg(ch))
  {
    ch->Send( "Sorry, but you don't deserve a logout message yet.\r\n");
    return;
  }
  if (strlen(argument) > MAX_LOGOUTMSG_LENGTH)
    ch->Send( "Sorry, but your logout message can't be longer then %d characters.\r\n",
                     MAX_LOGOUTMSG_LENGTH);
  else
  {
    set_logoutmsg(ch, argument);
    if(GET_LOGOUTMSG(ch)==NULL)
      ch->Send( "Ok, you don't have a logout message anymore.\r\n");
    else
      ch->Send( "Your new logoutmsg is: %s\r\n", GET_LOGOUTMSG(ch));
  }
  return;
}


ACMD(do_save)
{
  if (IS_NPC(ch) || !ch->desc)
    return;

  /* Only tell the char we're saving if they actually typed "save" */
  if (cmd)
  {
    /*
     * This prevents item duplication by two PC's using coordinated saves
     * (or one PC with a house) and system crashes. Note that houses are
     * still automatically saved without this enabled. This code assumes
     * that guest immortals aren't trustworthy. If you've disabled guest
     * immortal advances from mortality, you may want < instead of <=.
     */
    if ((CONFIG_AUTO_SAVE) && GET_LEVEL(ch) < LVL_IMMORT)
    {
      ch->Send("Saving aliases.\r\n");
      return;
    }
    ch->Send( "Saving %s and aliases.\r\n", GET_NAME(ch));
  }

  write_aliases(ch);
  ch->save();
  Crash_crashsave(ch);
  if (ROOM_FLAGGED(IN_ROOM(ch), ROOM_HOUSE_CRASH))
    House_crashsave(GET_ROOM_VNUM(IN_ROOM(ch)));
}

void list_rprooms_to_char(Character *ch)
{}
void list_kills_to_char(Character *ch)
{
  char line[MAX_INPUT_LENGTH];
  char *first, *last;
  Character *pmob = NULL;
  int found = 0;
  if (SPECIALS(ch)->KillsCount() == 0)
  {
    ch->Send( "You have no recorded kills.\r\n");
    return;
  }
  
  DYN_DEFINE;
  DYN_CREATE;
  for (kill_map::iterator it = SPECIALS(ch)->KillsBegin();it != SPECIALS(ch)->KillsEnd();it++)
  {
    if ((it->second)->vnum == NOBODY)
      continue;

    if (MobProtoExists((it->second)->vnum))
    {
      pmob = GetMobProto((it->second)->vnum);
      first = asctime(localtime(&(it->second)->first));
      first += 4;
      //*(first + strlen(first) - 2) = '\0';
      last = asctime(localtime(&(it->second)->last));
      last += 4;
      //*(last + 11 ) = '\0';
      snprintf(line, sizeof(line), "(%5dx) %-30.30s - Lev: %d Last:%-20.20s\r\n", (it->second)->count, pmob->player.short_descr, GET_LEVEL(pmob),  last);
      DYN_RESIZE(line);
      found++;
    }
  }

  page_string(ch->desc, dynbuf, DYN_BUFFER);
}
ACMD(do_killlist)
{
  list_kills_to_char(ch);
}

/* generic function for commands which are normally overridden by
   special procedures - i.e., shop commands, mail commands, etc. */
ACMD(do_not_here)
{
  ch->Send("Sorry, but you cannot do that here!\r\n");
}

ACMD(do_prac_skills)
{
  list_skills(ch, 1);
}

ACMD(do_prac_spells)
{
  list_skills(ch, 0);
}


ACMD(do_practice)
{
  if (IS_NPC(ch))
    return;
  char arg[MAX_INPUT_LENGTH];

  one_argument(argument, arg);

  if (!*arg)
  {
    ch->Send( "You have {cW%d{c0 practice sessions remaining.\r\n\r\n", GET_PRACTICES(ch));
    ch->Send("To View Skills Or Spells Either Type:\r\n");
    ch->Send("{cCpractice skills{c0 or type {cCpractice spells{c0\r\n\r\n");
    ch->Send("To view any other abilities you may have (subskills, clan skills, etc) type:\r\n");
    ch->Send("{cCpractice subs{c0\r\n\r\n");
    ch->Send("Otherwise to get better in that skill or spell try:\r\n");
    ch->Send("{ccpractice <skill/spell name>{c0\r\n");
    ch->Send("{cG[NOTE: You can practice with your skills and spells at a guildmaster]{c0\r\n");
    return;
  }

  if (isname("skills", arg))
  {
    list_skills(ch, 0);
    return;
  }
  if (isname("spells", arg))
  {
    list_skills(ch, 1);
    return;
  }

  if (is_abbrev(arg, "subskills"))
  {
    list_skills(ch, 2);
    return;
  }
  ch->Send("{cCpractice skills{c0 or type {cCpractice spells{c0 or {cCpractice subs{c0\r\n\r\n");
}



ACMD(do_visible)
{
  if (GET_LEVEL(ch) >= LVL_GOD)
    perform_immort_vis(ch);

  if (AFF_FLAGGED(ch, AFF_INVISIBLE))
  {
    ch->appear();
    *ch << "You break the spell of invisibility.\r\n";
  }
  else
    *ch << "You are already visible.\r\n";
}



ACMD(do_title)
{
  skip_spaces(&argument);
  delete_doubledollar(argument);

  if (IS_NPC(ch))
    *ch << "Your title is fine... go away.\r\n";
  else if (PLR_FLAGGED(ch, PLR_NOTITLE))
    *ch << "You can't title yourself -- you shouldn't have abused it!\r\n";
  else if (GET_RACE(ch) == RACE_GLADIATOR)
    *ch << "Your current title is just fine.\r\n"; 
  else if (strstr(argument, "(") || strstr(argument, ")"))
    *ch << "Titles can't contain the ( or ) characters.\r\n";
  else if (strlen(argument) > MAX_TITLE_LENGTH)
  {
    ch->Send("Sorry, titles can't be longer than %d characters.\r\n", MAX_TITLE_LENGTH);
  }
  else
  {
    set_title(ch, argument);
    ch->Send( "Okay, you're now %s %s.\r\n", GET_NAME(ch), GET_TITLE(ch));
  }
}


int perform_group(Character *ch, Character *vict)
{

  if (!CAN_SEE(ch, vict))
    return (0);

  if (vict->master == ch)
    return (0);

  if (ch->master == vict)
    stop_follower(ch);

  if (!IS_NPC(ch))
    SET_BIT_AR(AFF_FLAGS(vict), AFF_GROUP);
  else
    REMOVE_BIT_AR(AFF_FLAGS(vict), AFF_GROUP);

  add_follower(vict, ch);

  SET_BIT_AR(AFF_FLAGS(ch), AFF_GROUP);
  total_perc(ch);
  if (ch != vict)
    act("$N is now a member of your group.", FALSE, ch, 0, vict,  TO_CHAR);
  act("You are now a member of $n's group.", FALSE, ch, 0, vict, TO_VICT);
  act("$N is now a member of $n's group.", FALSE, ch, 0, vict,TO_NOTVICT);
  return (1);
}

C_FUNC(allow_follow)
{
  Character *tch = find_char(d->character->loader);
  if (!tch)
  {
    d->Output("%s isn't in the game any longer.\r\n", pi.NameById(d->character->loader));
    return;
  }
  if ('Y' == toupper(*arg))
    perform_group(d->character, tch);
  else
  {
    d->Output("You disallow %s to join your group.\r\n", pi.NameById(d->character->loader));
    *tch << GET_NAME(d->character) << " disallows you to join " << HSHR(d->character) << " group.\r\n";
  }
  d->character->loader = -1;
}


void print_group(Character *ch)
{
  Character *k;
  struct follow_type *f;
  char buf[MAX_INPUT_LENGTH];

  if (!AFF_FLAGGED(ch, AFF_GROUP))
    ch->Send("But you are not the member of a group!\r\n");
  else
  {
    ch->Send("Your group consists of:\r\n");

    k = (ch->master ? ch->master : ch);

    if (AFF_FLAGGED(k, AFF_GROUP))
    {
      if (GET_LEVEL(k)<LVL_IMMORT)
      {
        snprintf(buf, sizeof(buf),
                 "{cw[{cgTNL %15lld{cw][{cc%5dH %5dM %5dV %dT ({cC%4.1f%%{cc)I{cw] [{cg%2d %s{cw] {cC$N{cw :: Leader{c0",
                 exp_needed(k),
                 GET_HIT(k), GET_MANA(k), GET_MOVE(k),
                 current_class_is_tier_num(k), GET_PERC(k),
                 GET_LEVEL(k), CLASS_ABBR(k));
        act(buf, FALSE, ch, 0, k, TO_CHAR);
      }
      else
      {
        snprintf(buf, sizeof(buf),
                 "{cw[{cg    * IMMORTAL *   {cw][{cc%5dH %5dM %5dV %dT ({cC%4.1f%%{cc)I{cw] [{cg%2d %s{cw] {cC$N{cw :: Leader{c0",
                 GET_HIT(k), GET_MANA(k), GET_MOVE(k),
                 current_class_is_tier_num(k), GET_PERC(k),
                 GET_LEVEL(k), CLASS_ABBR(k));
        act(buf, FALSE, ch, 0, k, TO_CHAR);
      }

    }
    else
    {
      ch->Send( "No group.\r\n");
      return;
    }

    for (f = k->followers; f; f = f->next)
    {
      if (!AFF_FLAGGED(f->follower, AFF_GROUP))
        continue;
      if (!IS_NPC(f->follower))
      {
        if (GET_LEVEL(f->follower) < LVL_IMMORT)
        {
          snprintf(buf, sizeof(buf),
                   "{cw[{cgTNL %15lld{cw][{cc%5dH %5dM %5dV %dT ({cC%4.1f%%{cc)I{cw] [{cg%2d %s{cw] {cy$N{c0",
                   exp_needed(f->follower), GET_HIT(f->follower),
                   GET_MANA(f->follower), GET_MOVE(f->follower),
                   current_class_is_tier_num(f->follower),
                   GET_PERC(f->follower), GET_LEVEL(f->follower),
                   CLASS_ABBR(f->follower));

        }
        else
        {
          snprintf(buf, sizeof(buf),
                   "{cw[{cg    * IMMORTAL *   {cw][{cc%5dH %5dM %5dV %dT ({cC%4.1f%%{cc)I{cw] [{cg%2d %s{cw] {cC$N{c0",
                   GET_HIT(f->follower), GET_MANA(f->follower), GET_MOVE(f->follower),
                   current_class_is_tier_num(f->follower), GET_PERC(f->follower),
                   GET_LEVEL(f->follower), CLASS_ABBR(f->follower));
        }
      }
      else
      {
        snprintf(buf, sizeof(buf),
                 "{cw                     [{cy%5dH %5dM %5dV %dT ({cC%4.1f%%{cy)I{cw] [{cg%2d %s{cw] {cg$N{c0",
                 GET_HIT(f->follower), GET_MANA(f->follower),
                 GET_MOVE(f->follower),current_class_is_tier_num(f->follower), GET_PERC(f->follower),
                 GET_LEVEL(f->follower),
                 CLASS_ABBR(f->follower));
      }
      act(buf, FALSE, ch, 0, f->follower, TO_CHAR);
    }
  }
}



ACMD(do_group)
{
  Character *vict;
  struct follow_type *f;
  int found;
  char buf[MAX_INPUT_LENGTH];

  one_argument(argument, buf);


  total_perc(ch);
  print_group(ch);
  return;


  //-----edited out----

  if (ch->master)
  {
    act("You can not enroll group members without being head of a group.", FALSE, ch, 0, 0, TO_CHAR);
    return;
  }

  if (!str_cmp(buf, "all"))
  {
    //perform_group(ch, ch);
    for (found = 0, f = ch->followers; f; f = f->next)
    {
      if (!IS_NPC(f->follower) && (f->follower != ch))
        found += perform_group(ch, f->follower);
    }
    if (!found)
      *ch << "Everyone following you is already in your group.\r\n";
    return;
  }

  if (!(vict = get_char_vis(ch, buf, NULL, FIND_CHAR_ROOM)))
    *ch << CONFIG_NOPERSON;
  else if ((vict->master != ch) && (vict != ch))
    act("$N must follow you to enter your group.", FALSE, ch, 0, vict,
        TO_CHAR);
  else
  {
    if (!AFF_FLAGGED(vict, AFF_GROUP) && !IS_NPC(ch))
      perform_group(ch, vict);
    else
    {
      if (ch != vict)
        act("$N is no longer a member of your group.", FALSE, ch,
            0, vict, TO_CHAR);
      stop_follower(vict);
      act("You have been kicked out of $n's group!", FALSE, ch, 0,
          vict, TO_VICT);
      act("$N has been kicked out of $n's group!", FALSE, ch, 0,
          vict, TO_NOTVICT);
      REMOVE_BIT_AR(AFF_FLAGS(vict), AFF_GROUP);

    }
  }
}



ACMD(do_ungroup)
{
  struct follow_type *f, *next_fol;
  Character *tch;

  char buf[MAX_INPUT_LENGTH];

  one_argument(argument, buf);

  if (!*buf)
  {
    if (ch->master || !(AFF_FLAGGED(ch, AFF_GROUP)))
    {
      *ch << "But you lead no group!\r\n";
      return;
    }
    for (f = ch->followers; f; f = next_fol)
    {
      next_fol = f->next;
      //if (AFF_FLAGGED(f->follower, AFF_GROUP))
      //{
        REMOVE_BIT_AR(AFF_FLAGS(f->follower), AFF_GROUP);
        *f->follower << GET_NAME(ch) << " has disbanded the group.\r\n";
        //if (!AFF_FLAGGED(f->follower, AFF_CHARM))
          stop_follower(f->follower);
      //}
    }

    REMOVE_BIT_AR(AFF_FLAGS(ch), AFF_GROUP);
    *ch << "You disband the group.\r\n";
    return;
  }
  if (!(tch = get_char_vis(ch, buf, NULL, FIND_CHAR_ROOM)))
  {
    *ch << "There is no such person!\r\n";
    return;
  }
  if (tch->master != ch)
  {
    *ch << "That person is not following you!\r\n";
    return;
  }

  if (!AFF_FLAGGED(tch, AFF_GROUP))
  {
    *ch << "That person isn't in your group.\r\n";
    return;
  }

  REMOVE_BIT_AR(AFF_FLAGS(tch), AFF_GROUP);

  act("$N is no longer a member of your group.", FALSE, ch, 0, tch,
      TO_CHAR);
  act("You have been kicked out of $n's group!", FALSE, ch, 0, tch,
      TO_VICT);
  act("$N has been kicked out of $n's group!", FALSE, ch, 0, tch,
      TO_NOTVICT);

  if (!AFF_FLAGGED(tch, AFF_CHARM))
    stop_follower(tch);
}




ACMD(do_report)
{
  char buf[MAX_INPUT_LENGTH];
  /*   Character *k;
     struct follow_type *f;*/

  /*if (!AFF_FLAGGED(ch, AFF_GROUP)) {
  send_to_char("But you are not a member of any group!\r\n", ch);
  return;
  }*/

  return;

  snprintf(buf, sizeof(buf), "$n reports: %d/%dH, %d/%dM, %d/%dV\r\n",
           GET_HIT(ch), GET_MAX_HIT(ch),
           GET_MANA(ch), GET_MAX_MANA(ch),
           GET_MOVE(ch), GET_MAX_MOVE(ch));

  CAP(buf);
  if (IS_NPC(ch) && ch->master)
  {
    act(buf, FALSE, ch, 0, ch->master, TO_VICT);
    return;
  }

  /*  k = (ch->master ? ch->master : ch);

    for (f = k->followers; f; f = f->next)
  if (AFF_FLAGGED(f->follower, AFF_GROUP) && f->follower != ch)
     send_to_char(buf, f->follower);
    if (k != ch)*/

  act(buf, FALSE, ch, 0, 0, TO_ROOM );
  *ch << "You report to the room.\r\n";
}




ACMD(do_split)
{
  gold_int amount, num, share, rest;
  Character *k;
  struct follow_type *f;

  char buf[MAX_INPUT_LENGTH];
  size_t len = 0;

  if (IS_NPC(ch))
    return;

  one_argument(argument, buf);

  if (is_number(buf))
  {
    amount = atol(buf);
    if (amount <= 0)
    {
      *ch << "Sorry, you can't do that.\r\n";
      return;
    }
    if (amount > ch->Gold(0, GOLD_HAND))
    {
      *ch << "You don't seem to have that much gold to split.\r\n";
      return;
    }
    k = (ch->master ? ch->master : ch);

    if (AFF_FLAGGED(k, AFF_GROUP) && (k->in_room == IN_ROOM(ch)))
      num = 1;
    else
      num = 0;

    for (f = k->followers; f; f = f->next)
      if (AFF_FLAGGED(f->follower, AFF_GROUP) &&
          (!IS_NPC(f->follower)) &&
          (f->follower->in_room == IN_ROOM(ch)))
        num++;

    if (num && AFF_FLAGGED(ch, AFF_GROUP))
    {
      share = amount / num;
      rest = amount % num;
    }
    else
    {
      *ch << "With whom do you wish to share your gold?\r\n";
      return;
    }

    ch->Gold(-(share * (num - 1)), GOLD_HAND);

    len += snprintf(buf + len, sizeof(buf) - len, "%s splits %lld coins; you receive %lld.\r\n",
                    GET_NAME(ch), amount, share);
    if (rest)
    {
      len += snprintf(buf + len, sizeof(buf) - len,
                      "%lld coin%s %s not splitable, so %s "
                      "keeps the money.\r\n", rest, (rest == 1) ? "" : "s",
                      (rest == 1) ? "was" : "were", GET_NAME(ch));
    }
    if (AFF_FLAGGED(k, AFF_GROUP) && (k->in_room == IN_ROOM(ch))
        && !(IS_NPC(k)) && k != ch)
    {
      k->Gold(share, GOLD_HAND);
      *k << buf;
    }
    for (f = k->followers; f; f = f->next)
    {
      if (AFF_FLAGGED(f->follower, AFF_GROUP) &&
          (!IS_NPC(f->follower)) &&
          (f->follower->in_room == IN_ROOM(ch)) &&
          f->follower != ch)
      {
        f->follower->Gold(share, GOLD_HAND);
        *f->follower << buf;
      }
    }
    if (num)
    {
      *ch << "You split " << amount << " coins among " << num << " members -- " << share << " coins each.\r\n";
      if (rest)
      {
        ch->Send("%lld coin%s%s not splitable, so you keep the money.\r\n", rest,(rest == 1) ? "" : "s", (rest == 1) ? " was" : " were");
        
        ch->Gold(rest, GOLD_HAND);
      }
    }
  }
  else
  {
    *ch << "How many coins do you wish to split with your group?\r\n";
    return;
  }

}




ACMD(do_use)
{
  struct obj_data *mag_item;

  char buf[MAX_INPUT_LENGTH];
  char arg[MAX_INPUT_LENGTH];

  half_chop(argument, arg, buf);
  if (!*arg)
  {
    *ch << "What do you want to " << CMD_NAME << "?\r\n";
    return;
  }
  mag_item = GET_EQ(ch, WEAR_HOLD);

  if (!mag_item || !isname(arg, mag_item->name))
  {
    switch (subcmd)
    {
    case SCMD_RECITE:
    case SCMD_QUAFF:
      if (!
          (mag_item =
             get_obj_in_list_vis(ch, arg, NULL, ch->carrying)))
      {
        *ch << "You don't seem to have " << AN(arg) << " "  << arg << "\r\n";
        return;
      }
      break;
    case SCMD_USE:
      *ch << "You don't seem to be holding " << AN(arg) << " " << arg << "\r\n";
      return;
    default:
      log("SYSERR: Unknown subcmd %d passed to do_use.", subcmd);
      return;
    }
  }
  switch (subcmd)
  {
  case SCMD_QUAFF:
    if (GET_OBJ_TYPE(mag_item) != ITEM_POTION &&
        GET_OBJ_TYPE(mag_item) != ITEM_ANTIDOTE_1 &&
        GET_OBJ_TYPE(mag_item) != ITEM_ANTIDOTE_2 &&
        GET_OBJ_TYPE(mag_item) != ITEM_ANTIDOTE_3)
    {
      *ch << "You can only quaff potions.\r\n";
      return;
    }
    break;
  case SCMD_RECITE:
    if (GET_OBJ_TYPE(mag_item) != ITEM_SCROLL)
    {
      *ch << "You can only recite scrolls.\r\n";
      return;
    }
    break;
  case SCMD_USE:
    if ((GET_OBJ_TYPE(mag_item) != ITEM_WAND) &&
        (GET_OBJ_TYPE(mag_item) != ITEM_STAFF))
    {
      *ch << "You can't seem to figure out how to use it.\r\n";
      return;
    }
    break;
  }

  mag_objectmagic(ch, mag_item, buf);
}



ACMD(do_wimpy)
{
  int wimp_lev;
  char arg[MAX_INPUT_LENGTH];

  /* 'wimp_level' is a player_special. -gg 2/25/98 */
  if (IS_NPC(ch))
    return;

  one_argument(argument, arg);

  if (!*arg)
  {
    if (GET_WIMP_LEV(ch))
    {
      *ch << "Your current wimp level is " << GET_WIMP_LEV(ch) << " hit points.\r\n";
      return;
    }
    else
    {
      *ch << "At the moment, you're not a wimp.  (sure, sure...)\r\n";
      return;
    }
  }
  if (isdigit(*arg))
  {
    if ((wimp_lev = atoi(arg)) != 0)
    {
      if (wimp_lev < 0)
        *ch << "Heh, heh, heh.. we are jolly funny today, eh?\r\n";
      else if (wimp_lev > GET_MAX_HIT(ch))
        *ch << "That doesn't make much sense, now does it?\r\n";
      else if (wimp_lev > (GET_MAX_HIT(ch) / 2))
        *ch << "You can't set your wimp level above half your hit points.\r\n";
      else
      {
        ch->Send(
                         "Okay, you'll wimp out if you drop below %d hit points.\r\n",
                         wimp_lev);
        GET_WIMP_LEV(ch) = wimp_lev;
      }
    }
    else
    {
      *ch << "Okay, you'll now tough out fights to the bitter end.\r\n";
      GET_WIMP_LEV(ch) = 0;
    }
  }
  else
    *ch << "Specify at how many hit points you want to wimp out at.  (0 to disable)\r\n";
}


ACMD(do_gen_write)
{
  FILE *fl;
  char *tmp;
  const char *filename;
  struct stat fbuf;
  time_t ct;

  switch (subcmd)
  {
  case SCMD_BUG:
    filename = BUG_FILE;
    break;
  case SCMD_TYPO:
    filename = TYPO_FILE;
    break;
  case SCMD_IDEA:
    filename = IDEA_FILE;
    break;
  default:
    return;
  }

  ct = time(0);
  tmp = asctime(localtime(&ct));

  if (IS_NPC(ch))
  {
    *ch << "Monsters can't have ideas - Go away.\r\n";
    return;
  }

  skip_spaces(&argument);
  delete_doubledollar(argument);

  if (!*argument)
  {
    *ch << "That must be a mistake...\r\n";
    return;
  }
new_mudlog(CMP, LVL_GOD, FALSE, "%s %s (%d): %s", GET_NAME(ch), CMD_NAME, GET_ROOM_VNUM(IN_ROOM(ch)), argument);

  if (stat(filename, &fbuf) < 0)
  {
    perror("SYSERR: Can't stat() file");
    return;
  }
  if (fbuf.st_size >= CONFIG_MAX_FILESIZE)
  {
    *ch << "Sorry, the file is full right now.. try again later.\r\n";
    return;
  }
  if (!(fl = fopen(filename, "a")))
  {
    perror("SYSERR: do_gen_write");
    *ch << "Could not open the file.  Sorry.\r\n";
    return;
  }
  fprintf(fl, "<table cellpadding=0 cellspacing=0 border=0 class='txtline'>"
          "<tr><td class='plrname'>%-13s</td><td class='date'>%6.6s</td><td class='room'>%5d</td><td class='comment'>%s</td></tr></table>\n", GET_NAME(ch), (tmp + 4),
          GET_ROOM_VNUM(IN_ROOM(ch)), argument);

  fclose(fl);
  /*new_mudlog(NRM, MAX(LVL_GOD, GET_INVIS_LEV(ch)), TRUE, "%-8s (%6.6s) [%5d] %s\n", GET_NAME(ch), (tmp + 4),             GET_ROOM_VNUM(IN_ROOM(ch)), argument);*/
  ch->Send( "Okay.  Thanks!\r\n");
}

void parse_afk(Character *ch, char *argument)
{
  char *def = (char *)"Away From Keyboard";
  char **msg;
  char buf[MAX_INPUT_LENGTH];

  msg = &(AFK_MSG(ch));

  if (argument != NULL)
  {
    skip_spaces(&argument);
    if (strlen(argument) > 79)
    {
      ch->Send( "Your afk message must be shorter than 80 characters.\r\n");
      return;
    }
    if (*msg)
      free(*msg);
    *msg = str_dup((!argument || !*argument) ? def : argument);
    snprintf(buf, sizeof(buf), "$n has gone AFK: %s", AFK_MSG(ch));
    act(buf, TRUE, ch, 0, 0, TO_ROOM);
    ch->Send( "AFK: %s\r\n", AFK_MSG(ch));
  }
  else
  {
    if (*msg)
      free(*msg);
    *msg = NULL;
    act("$n has come back from AFK.", TRUE, ch, 0, 0, TO_ROOM);
  }


}

void parse_busy(Character *ch, char *argument)
{
  char *def = (char *)"Do not disturb.";
  char **msg;
  char buf[MAX_INPUT_LENGTH];

  msg = &(BUSY_MSG(ch));

  if (argument != NULL)
  {
    skip_spaces(&argument);
    if (strlen(argument) > 79)
    {
      ch->Send( "Your busy message must be shorter than 80 characters.\r\n");
      return;
    }
    if (*msg)
      free(*msg);
    *msg = str_dup((!argument || !*argument) ? def : argument);
    snprintf(buf, sizeof(buf), "$n has gone BUSY: %s", *msg);
    act(buf, TRUE, ch, 0, 0, TO_ROOM);
    ch->Send( "BUSY: %s\r\n", BUSY_MSG(ch));
  }
  else
  {
    if (*msg)
      free(*msg);
    *msg = NULL;
    act("$n has come back from being BUSY.", TRUE, ch, 0, 0, TO_ROOM);
  }


}


#define TOG_OFF 0
#define TOG_ON  1

ACMD(do_gen_tog)
{
  long result;

  const char *tog_messages[][2] =
    {
      {"You are now safe from summoning by other players.\r\n",
       "You may now be summoned by other players.\r\n"
      },
      {"Nohassle disabled.\r\n",
       "Nohassle enabled.\r\n"},
      {"Brief mode off.\r\n",
       "Brief mode on.\r\n"},
      {"Compact mode off.\r\n",
       "Compact mode on.\r\n"},
      {"You can now hear tells.\r\n",
       "You are now deaf to tells.\r\n"},
      {"You can now hear auctions.\r\n",
       "You are now deaf to auctions.\r\n"},
      {"You can now hear shouts.\r\n",
       "You are now deaf to shouts.\r\n"},
      {"You can now hear gossip.\r\n",
       "You are now deaf to gossip.\r\n"},
      {"You can now hear the congratulation messages.\r\n",
       "You are now deaf to the congratulation messages.\r\n"},
      {"You can now hear the Wiz-channel.\r\n",
       "You are now deaf to the Wiz-channel.\r\n"},
      {"You are no longer part of the Quest.\r\n",
       "Okay, you are part of the Quest!\r\n"},
      {"You will no longer see the room flags.\r\n",
       "You will now see the room flags.\r\n"},
      {"You will now have your communication repeated.\r\n",
       "You will no longer have your communication repeated.\r\n"},
      {"HolyLight mode off.\r\n",
       "HolyLight mode on.\r\n"},
      {"Nameserver_is_slow changed to NO; IP addresses will now be resolved.\r\n",
       "Nameserver_is_slow changed to YES; sitenames will no longer be resolved.\r\n"},
      {"Autoexits disabled.\r\n",
       "Autoexits enabled.\r\n"},
      {"AFK flag is now off.\r\n",
       "AFK flag is now on.\r\n"},
      {"AutoSplit disabled.\r\n",
       "AutoSplit enabled.\r\n"},
      {"AutoLooting disabled.\r\n",
       "AutoLooting enabled.\r\n"},
      {"You will no longer AutoAssist.\r\n",
       "You will now AutoAssist.\r\n"},
      {"Autogold disabled.\r\n",
       "Autogold enabled.\r\n"},
      {"You will no longer see arena messages.\r\n",
       "You will now see arena messages.\r\n"},
      {"Ascii objects turned off.\r\n",
       "Ascii objects turned on.\r\n"},
      {"You will no longer keep your title after levelling.\r\n",
       "You will now keep your title after levelling.\r\n"},
      {"You can now hear the IC channel.\r\n",
       "You are now deaf to the IC channel.\r\n"},
      {"Will no longer track through doors.\r\n",
       "Will now track through doors.\r\n"},
      {"You will now hear the newbie channel.\r\n",
       "You will no longer hear the newbie channel.\r\n"},
      {"You will no longer see battle spam when not fighting.\r\n",
       "You will now see battle spam when not fighting.\r\n"},
      {"You will no longer see a message when you receive mail.\r\n",
       "You will now see a message when you receive mail.\r\n"},
      {"You can now hear your clan\r\n",
       "You will no longer hear your clan.\r\n"},
      {"You will no longer hear tells when afk.\r\n",
       "You will now hear tells when afk.\r\n"},
      {"You will now see movement messages.\r\n",
       "You will no longer see movement messages.\r\n"},
      {"You are unmountable now.\r\n",
       "You are mountable\r\n"},
      {"You Hero channel is now On.\r\n",
       "The Hero channel is now Off.\r\n"},
      {"You put time on your prompt.\r\n",
       "You take time off your prompt.\r\n"},
      {"You will not automaticly sacrifice corpses.\r\n",
       "You will now auto-sacrifice corpses.\r\n"},
      {"Will no longer clear screen in OLC.\r\n",
       "Will now clear screen in OLC.\r\n"},
      {"Buildwalk Off.\r\n",
       "Buildwalk On.\r\n"},
      {"Compression disabled.\r\n",
       "Compression enabled.\r\n"},
      {"Compression will not be enabled automatically.\r\n",
       "Compression will be automatically enabled if your client supports it.\r\n"},
      {"You enable the ooc channel\r\n",
       "You disable the ooc channel\r\n"},
      {"You turn off page wrapping.\r\n",
       "You turn on page wrapping\r\n"},
      {"You unlock your replyer.\r\n",
       "You unlock your replier.\r\n"},
      {"BUSY flag is now off.\r\n",
       "BUSY flag is now on.\r\n"},
      {"Aggro mode disabled.\r\n",
       "Aggro mode enabled.\r\n"},
      {"Nobrag on.\r\n",
       "Nobrag off.\r\n"},
      {"You may now be gated to by other players.\r\n",
       "You are now safe from gating by other players.\r\n"    },
      {"You are not roleplaying anymore.\r\n",
       "You are now roleplaying.\r\n"},
      {"You will no longer see a tally of how many fish you have caught.\r\n",
       "You will now see a tally of how many fish you have caught.\r\n"},
      {"You can now have the teleport spell cast on you.\r\n",
       "You can now no longer have the teleport spell cast on you.\r\n"},
      {   "You will NOT automaticly agree to group people when they request to follow.\r\n",
          "You will automaticly agree to group people when they request to follow.\r\n"}


    };


  if (IS_NPC(ch))
    return;

  switch (subcmd)
  {
  case SCMD_NOSUMMON:
    result = PRF_TOG_CHK(ch, PRF_SUMMONABLE);
    break;
  case SCMD_NOHASSLE:
    result = PRF_TOG_CHK(ch, PRF_NOHASSLE);
    break;
  case SCMD_BRIEF:
    result = PRF_TOG_CHK(ch, PRF_BRIEF);
    break;
  case SCMD_COMPACT:
    result = PRF_TOG_CHK(ch, PRF_COMPACT);
    break;
  case SCMD_NOTELL:
    result = PRF_TOG_CHK(ch, PRF_NOTELL);
    break;
  case SCMD_NOAUCTION:
    result = PRF_TOG_CHK(ch, PRF_NOAUCT);
    break;
  case SCMD_DEAF:
    result = PRF_TOG_CHK(ch, PRF_DEAF);
    break;
  case SCMD_NOGOSSIP:
    result = PRF_TOG_CHK(ch, PRF_NOGOSS);
    break;
  case SCMD_NOGRATZ:
    result = PRF_TOG_CHK(ch, PRF_NOGRATZ);
    break;
  case SCMD_NOWIZ:
    if(PLR_FLAGGED(ch,PLR_IMM_MORT) || (GET_LEVEL(ch)>LVL_HERO+1 && (CMD_FLAGGED(ch, WIZ_IMM1_GRP) || CMD_FLAGGED2(ch, WIZ_IMM1_GRP))))
      result = PRF_TOG_CHK(ch, PRF_NOWIZ);
    else {
      ch->Send("Huh?!?\r\n");
      result = -1;
    }
    break;
  case SCMD_QUEST:
    result = PRF_TOG_CHK(ch, PRF_QUEST);
    break;
  case SCMD_ROOMFLAGS:
    result = PRF_TOG_CHK(ch, PRF_ROOMFLAGS);
    break;
  case SCMD_NOREPEAT:
    result = PRF_TOG_CHK(ch, PRF_NOREPEAT);
    break;
  case SCMD_HOLYLIGHT:
    result = PRF_TOG_CHK(ch, PRF_HOLYLIGHT);
    break;
  case SCMD_SLOWNS:
    result = (CONFIG_NS_IS_SLOW = !CONFIG_NS_IS_SLOW);
    break;
  case SCMD_AUTOEXIT:
    result = PRF_TOG_CHK(ch, PRF_AUTOEXIT);
    break;
  case SCMD_AFK:
    result = PRF_TOG_CHK(ch, PRF_AFK);
    if (PRF_FLAGGED(ch, PRF_AFK))
      parse_afk(ch, argument);
    else
      parse_afk(ch, NULL);
    break;
  case SCMD_RP:
    result = PRF_TOG_CHK(ch, PRF_RP);
    break;
  case SCMD_AGGRO:
    result = PRF_TOG_CHK(ch, PRF_AGGRO);
    break;
  case SCMD_BUSY:
    result = PRF_TOG_CHK(ch, PRF_BUSY);
    if (PRF_FLAGGED(ch, PRF_BUSY))
      parse_busy(ch, argument);
    else
      parse_busy(ch, NULL);
    break;
  case SCMD_AUTOSPLIT:
    result = PRF_TOG_CHK(ch, PRF_AUTOSPLIT);
    break;
  case SCMD_AUTOLOOT:
    result = PRF_TOG_CHK(ch, PRF_AUTOLOOT);
    break;
  case SCMD_AUTOASSIST:
    result = PRF_TOG_CHK(ch, PRF_AUTOASSIST);
    break;
  case SCMD_AUTOGOLD:
    result = PRF_TOG_CHK(ch, PRF_AUTOGOLD);
    break;
  case SCMD_ARENA:
    result = PRF_TOG_CHK(ch, PRF_ARENA);
    break;
  case SCMD_KEEPTITLE:
    result = PRF_TOG_CHK(ch, PRF_KEEPTITLE);
    break;
  case SCMD_NOIC:
    result = PRF_TOG_CHK(ch, PRF_NOIC);
    break;
  case SCMD_BATTLESPAM:
    result = PRF_TOG_CHK(ch, PRF_BATTLESPAM);
    break;
  case SCMD_MAIL:
    result = PRF_TOG_CHK(ch, PRF_MAIL);
    break;
  case SCMD_NOCTALK:
    result = PRF_TOG_CHK(ch, PRF_NOCTALK);
    break;
  case SCMD_AFKTELL:
    result = PRF_TOG_CHK(ch, PRF_AFKTELL);
    break;
  case SCMD_MOVEMSG:
    result = PRF_TOG_CHK(ch, PRF_MOVEMSG);
    break;
  case SCMD_NOHERO:
    result = PRF_TOG_CHK(ch, PRF_NOHERO);
    break;
  case SCMD_NONEWBIE:
    result = PRF_TOG_CHK(ch, PRF_NONEWBIE);
    break;
  case SCMD_PTIME:
    result = PRF_TOG_CHK(ch, PRF_TIME);
    break;
  case SCMD_AUTOSAC:
    result = PRF_TOG_CHK(ch, PRF_AUTOSAC);
    break;
  case SCMD_MOUNTABLE:
    result = PRF_TOG_CHK(ch, PRF_MOUNTABLE);
    break;
  case SCMD_TRACK:
    result = (CONFIG_TRACK_T_DOORS = !CONFIG_TRACK_T_DOORS);
    break;
  case SCMD_CLS:
    result = PRF_TOG_CHK(ch, PRF_CLS);
    break;
  case SCMD_NOGATE:
    result = PRF_TOG_CHK(ch, PRF_GATEABLE);
    break;
  case SCMD_NOTELEPORT:
    if (IS_PK(ch))
      ch->Send( "As a PKer, the teleport toggle will not work vs other PK players.\r\n");
    result = PRF_TOG_CHK(ch, PRF_TELEPORTABLE);
    break;
  case SCMD_AUTOGROUP:
    result = PRF_TOG_CHK(ch, PRF_AUTOGROUP);
    break;
  case SCMD_BUILDWALK:
    if (GET_LEVEL(ch) < LVL_BUILDER)
    {
      ch->Send( "Builders only, sorry.\r\n");
      return;
    }
    result = PRF_TOG_CHK(ch, PRF_BUILDWALK);
    if (PRF_FLAGGED(ch, PRF_BUILDWALK))
      new_mudlog(CMP, GET_LEVEL(ch), TRUE,
                 "OLC: %s turned buildwalk on. Allowed zone %d", GET_NAME(ch), GET_OLC_ZONE(ch));
    else
      new_mudlog(CMP, GET_LEVEL(ch), TRUE,
                 "OLC: %s turned buildwalk off. Allowed zone %d", GET_NAME(ch), GET_OLC_ZONE(ch));
    break;
#if defined(HAVE_ZLIB)
  case SCMD_AUTOZLIB:
    result = PRF_TOG_CHK(ch, PRF_NOCOMPRESS);
    break;
#else
  case SCMD_COMPRESS:
  case SCMD_AUTOZLIB:
    *ch << "Compression not supported.\r\n";
    return;
#endif
  case SCMD_NOOOC:
    result = PRF_TOG_CHK(ch, PRF_OOC);
    break;
  case SCMD_PAGEWRAP:
    result = PRF_TOG_CHK(ch, PRF_PAGEWRAP);
    break;
  case SCMD_REPLYLOCK:
    result = PRF_TOG_CHK(ch, PRF_REPLYLOCK);
    break;
  case SCMD_NOBRAG:
    result = PRF_TOG_CHK(ch, PRF_NOBRAG);
    break;
  case SCMD_FISHTALLY:
    result = PRF_TOG_CHK(ch, PRF_FISHPROMPT);
    break;
  default:
    log("SYSERR: Unknown subcmd %d in do_gen_toggle.", subcmd);
    return;
  }
  if (result!=-1){
    if (result)
      ch->Send( "%s", tog_messages[subcmd][TOG_ON]);
    else
      ch->Send( "%s", tog_messages[subcmd][TOG_OFF]);
  }
  return;
}


ACMD(do_file)
{
  FILE *req_file;
  int cur_line = 0, num_lines = 0, req_lines = 0, i, j;
  int l;
  char field[MAX_INPUT_LENGTH], value[MAX_INPUT_LENGTH], line[READ_SIZE];
  char buf[MAX_STRING_LENGTH];
  size_t len = 0;

  struct file_struct
  {
    string cmd;
    char level;
    string file;
  }
  fields[] = {
               {"none",  55,  "Does Nothing"      },
               {"bug",        54,  BUG_FILE       },
               {"typo",  54,  TYPO_FILE      },
               {"ideas",      55,  IDEA_FILE      },
               {"xnames",     55,  "../lib/misc/xnames"     },
               {"levels",     55,  "../log/levels"          },
               {"rip",        55,  "../log/rip"        },
               {"dt",         55,  "../log/dts"        },
               {"help",  54,  "../log/missing-help"    },
               {"comlog",     54,  "../log/comlog"          },
               {"errors",     55,  "../log/errors"          },
               {"godcmds",    55,  "../log/godcmds"    },
               {"syslog",     55,  "../log/syslog"          },
               {"crash",      55,  "../syslog.CRASH"   },
               {"\n",         0,   "\n"           }
             };

  skip_spaces(&argument);

  if (!*argument)
  {
    ch->Send(
                     "USAGE: file <option> <num lines>\r\n\r\nFile options:\r\n");
    for (j = 0, i = 1; fields[i].level; i++)
      if (fields[i].level <= GET_LEVEL(ch))
        ch->Send( "%-15s%s\r\n", fields[i].cmd.c_str(),
                         fields[i].file.c_str());
    return;
  }

  two_arguments(argument, field, value);

  for (l = 0; *(fields[l].cmd.c_str()) != '\n'; l++)
    if (!strncmp(field, fields[l].cmd.c_str(), strlen(field)))
      break;

  if (*(fields[l].cmd.c_str()) == '\n')
  {
    *ch << "That is not a valid option!\r\n";
    return;
  }

  if (GET_LEVEL(ch) < fields[l].level)
  {
    *ch << "You are not godly enough to view that file!\r\n";
    return;
  }

  if (!*value)
    req_lines = 15;      /* default is the last 15 lines */
  else
    req_lines = atoi(value);

  if (!(req_file = fopen(fields[l].file.c_str(), "r")))
  {
    new_mudlog(NRM, MAX(LVL_GOD, GET_INVIS_LEV(ch)), TRUE, "SYSERR: Error opening file %s using 'file' command.",
               fields[l].file.c_str());
    return;
  }

  get_line(req_file, line);
  while (!feof(req_file))
  {
    num_lines++;
    get_line(req_file, line);
  }
  rewind(req_file);

  req_lines = MIN(MIN(req_lines, num_lines), 150);

  buf[0] = '\0';

  get_line(req_file, line);
  while (!feof(req_file))
  {
    cur_line++;
    if (cur_line > (num_lines - req_lines))
    {
      ReplaceString ( line, "<table cellpadding=0 cellspacing=0 border=0 class='txtline'><tr><td class='plrname'>", "{cc", sizeof(line));
      ReplaceString ( line, "</td><td class='date'>", " {cw- ", sizeof(line));
      ReplaceString ( line, "</td><td class='room'>", " - ", sizeof(line));
      ReplaceString ( line, "</td><td class='comment'>", " :{cg ", sizeof(line));
      ReplaceString ( line, "</td></tr></table>", " {c0", sizeof(line));

      len += snprintf(buf + len, sizeof(buf) - len, "%s\r\n", line);
    }

    get_line(req_file, line);
  }
  fclose(req_file);

  page_string(ch->desc, buf, 1);

}


ACMD(do_sac)
{
  struct obj_data *obj;
  char arg[MAX_INPUT_LENGTH];

  one_argument(argument, arg);

  // note, I like to take care of no arg and wrong args up front, not
  // at the end of a function, lets get the wrongness out of the way :)
  if (!*arg)
  {
    *ch << "OK, but what do you want to sacrifice?\r\n";
    return;
  }
  // if it's not in the room, we ain't gonna sac it
  if (!
      (obj =
         get_obj_in_list_vis(ch, arg, NULL,
                             IN_ROOM(ch)->contents)))
  {
    *ch << "There is nothing like that here. Try again.\r\n";
    return;
  }
  // nifty, got the object in the room, now check its flags
  if (!CAN_WEAR(obj, ITEM_WEAR_TAKE) || IS_OBJ_STAT(obj, ITEM_PC_CORPSE))
  {
    *ch << "You can't sacrifice that.\r\n";
    return;
  }
  // seems as if everything checks out eh? ok now do it
  act("$n sacrifices $p.", FALSE, ch, obj, 0, TO_ROOM);
  act("You sacrifice $p to your god.\r\nYou have been rewarded by your deity.\r\n", FALSE, ch, obj, 0, TO_CHAR);
  if (GET_MOVE(ch) < GET_MAX_MOVE(ch))
    alter_move(ch, -2);
  else
    alter_mana(ch, -5);
  extract_obj(obj);
}
#if 0
void write_aliases(Character *ch)
{
  FILE *file;
  char fn[127], buf1[MAX_STRING_LENGTH *2], *buf;
  struct alias_data *temp;
  int length;

  get_filename(GET_NAME(ch), fn, ALIAS_FILE);
  unlink(fn);
  if (!GET_ALIASES(ch))
    return;

  file = fopen(fn, "wt");

  temp = GET_ALIASES(ch);

  while (temp)
  {
    length = strlen(temp->alias);
    if (length <= 250)
    {
      fprintf(file, "%d\n", length);
      fprintf(file, "%s\n", temp->alias);
      snprintf(buf1, sizeof(buf1), "%s", temp->replacement);
      buf = buf1;
      while (*++buf == ' ');
      length = strlen(buf);
      fprintf(file, "%d\n", length);
      fprintf(file, "%s\n", buf);
      fprintf(file, "%d\n", temp->type);
      temp = temp->next;

    }

  }
  fclose(file);
}

void read_aliases(Character *ch)
{
  FILE *file;
  char fn[256];
  struct alias_data *t2;
  int length = 0;
  char temp_buf[MAX_INPUT_LENGTH], buf[MAX_INPUT_LENGTH];

  get_filename(GET_NAME(ch), fn, ALIAS_FILE);

  file = fopen(fn, "r");

  if (!file)
    return;

  CREATE(GET_ALIASES(ch), struct alias_data, 1);
  t2 = GET_ALIASES(ch);
  do
  {
    fscanf(file, "%d\n", &length);
    fgets(buf, length + 1, file);
    t2->alias = strdup(buf);
    fscanf(file, "%d\n", &length);

    fgets(buf, length + 1, file);

    strcpy(temp_buf, " ");
    if (length < MAX_INPUT_LENGTH)
    {
      strcat(temp_buf, buf);
    }
    t2->replacement = strdup(temp_buf);

    fscanf(file, "%d\n", &length);
    t2->type = length;
    if (!feof(file))
    {
      CREATE(t2->next, struct alias_data, 1);
      t2 = t2->next;
    }
  }
  while (!feof(file));

  fclose(file);
}
#endif

/*
void write_poofs(Character *ch)
{
    FILE *file;
    char fn[127];
    char *poofin, *poofout;
    int length;
    
    get_filename(GET_NAME(ch),fn,POOF_FILE);
    unlink(fn);
    if( !POOFIN(ch) && !POOFOUT(ch) )
        return;
 
    file = fopen(fn,"wt");
 
    poofin  = POOFIN(ch);
    poofout = POOFOUT(ch);
 
    length = strlen(poofin);
    fprintf(file,"%d\n",length);
    fprintf(file,"%s\n",poofin);
 
    length = strlen(poofout);
    fprintf(file,"%d\n",length);
    fprintf(file,"%s\n",poofout);
 
 
    fclose(file);
}
 
void read_poofs(Character *ch)
{   
    FILE *file;
    char fn[127], pin[127], pout[127];
    int length;
 
    get_filename(GET_NAME(ch),fn,POOF_FILE);
 
    file = fopen(fn,"r");
 
    if( !file ) {
      ch->player_specials->poofin = NULL;
      ch->player_specials->poofout = NULL; 
      return;
    }
 
    fscanf(file, "%d\n", &length);
    fgets(pin, length+1, file);
    ch->player_specials->poofin = strdup(pin);
 
    fscanf(file, "%d\n", &length);
    fgets(pout, length+1, file);
    POOFOUT(ch) = strdup(pout);
 
    fclose(file);
}
*/
/********************************************************************
** Written by Christopher M. Ryan 7/26/96
** do_bury & do_dig()
********************************************************************/
/**********************************************************************
** Please note that the way the code is set up all messages that are
** to be displayed to the character use send message so you can't add
** any variable types to your messages that go to the character.
**********************************************************************/
const char *msgs[][2] =
  {
    {"You start to break some floor boards when you dig.\r\n",
     "$n starts to break some floor boards as $e starts digging.\r\n"
    },

    {"You wonder if this is a good place after all, with all the gravel.\r\n",
     "$n breaks a sweat digging up all the gravel here.\r\n"},

    {"You make a nice hole while digging up a lot of dirt.\r\n",
     "$n digs a hole and goes about $s business.\r\n"},

    {"You seem to be hitting alot of roots when you dig.\r\n",
     "$n look like $e is trying to dig up a tree!\r\n"},

    {"You dig up more clay than dirt here.\r\n",
     "$n seems to be digging up alot of clay.\r\n"},

    {"You start to chip away at the rock here.\r\n",
     "$n bangs away at the side of the mountain.\r\n"},

    {"You can't dig in the water!\r\n",
     NULL},

    {"You can't dig in the water!\r\n",
     NULL},

    {"You can't dig in the water!\r\n",
     NULL},

    {"You can't dig up air!\r\n",
     NULL},

    {"You start moving sand out of the way.\r\n",
     "$n starts moving sand out of the way.\r\n"},



    //SECT_SPACE         11   /* In outer space                  */
    {"You can't dig in space!\r\n",
     NULL},
    //SECT_ROAD          12   /* On a Road                       */
    {"You can't dig the road up!\r\n",
     NULL},
    //SECT_ENTRANCE      13   /* Entrance to a zone              */
    {"You can't dig the entrance up!\r\n",
     NULL},
    //SECT_ATMOSPHERE         14   /* Entrance to a planet            */
    {"You can' dig in atmosphere!\r\n",
     NULL},
    //SECT_SUN      15   /* Into the Sun                    */
    {"You can't dig on a sun!\r\n",
     NULL},
    //SECT_BLACKHOLE          16   /* Into a Black Hole               */
    {"You can't dig in a black hole!\r\n",
     NULL},
    //SECT_VEHICLE       17   /* Internal use only               */
    {"You can't dig here!\r\n",
     NULL},
    //SECT_SWAMP         18
    {"You start moving reeds and flies out of the way.\r\n",
     "$n starts moving reeds and flies out of the way.\r\n"},
    //SECT_REEF               19
    {"You start moving coral and fish out of the way.\r\n",
     "$n starts moving coral and fish out of the way.\r\n"},
    // SECT_TUNDRA          20
    {"You start moving snow and dry grass out of the way.\r\n",
     "$n starts moving snow and dry grass out of the way.\r\n"},
    //SECT_SNOW          21
    {"You start moving snow out of the way.\r\n",
     "$n starts moving snow out of the way.\r\n"},
    //SECT_ICE      22
    {"You start hacking ice out of the way.\r\n",
     "$n starts hacking out of the way.\r\n"},
    //SECT_PRAIRIE 23
    {"You start moving dry grass and rodents out of the way.\r\n",
     "$n starts moving dry grass and rodents out of the way.\r\n"},
    //SECT_BADLANDS 24
    {"You start moving glowing rocks out of the way.\r\n",
     "$n starts moving glowing rocks out of the way.\r\n"},
#define SECT_RAIL     25
    {"You start moving huge metal rails out of the way.\r\n",
     "$n starts moving huge metal rails out of the way.\r\n"},

    /* always keep this as the last message */

    {"If you see this please tell a god.\r\n", NULL}
  };


ACMD(do_bury)
{
  struct obj_data *obj;
  char arg[MAX_INPUT_LENGTH];
  char buf[MAX_INPUT_LENGTH];

  half_chop(argument, arg, buf);

  if (!*arg)
  {
    ch->Send( "What do you want to %s?\r\n", CMD_NAME);
    return;
  }

  if (!(obj = get_obj_in_list_vis(ch, arg, NULL, ch->carrying)))
  {
    ch->Send( "You don't have %s %s.\r\n", AN(arg), arg);
    return;
  }


  /*
   ** find the sector types that you don't want people
   ** to be able to dig or bury in.
   */

  /* display the messages if available */

  if (msgs[IN_ROOM(ch)->sector_type][0] != NULL)
    *ch << msgs[IN_ROOM(ch)->sector_type][0];

  if (msgs[IN_ROOM(ch)->sector_type][1] != NULL)
    act(msgs[IN_ROOM(ch)->sector_type][1], TRUE, ch, NULL,NULL, TO_ROOM);
  else
    return;


  /* set a wait state */

  WAIT_STATE(ch, 10 RL_SEC);


  act("You bury $a $o here.\r\n", TRUE, ch, obj, NULL, TO_CHAR);
  act("$n buries $a $o here.\r\n", TRUE, ch, obj, NULL, TO_ROOM);

  obj_from_char(obj);
  SET_BIT_AR(GET_OBJ_EXTRA(obj), ITEM_BURIED);
  obj_to_room(obj, IN_ROOM(ch));
};
ACMD(do_delay)
{
  int n;
  skip_spaces(&argument);
  n = atoi(argument);
  if (n > 0 && n < 300)
    WAIT_STATE(ch, n RL_SEC);
}

ACMD(do_dig_ground)
{
  struct obj_data *obj;
  int chance = 1;

  /*
   ** find the sector types that you don't want people
   ** to be able to dig or bury in.
   */

  /* display the messages if available */

  if (msgs[IN_ROOM(ch)->sector_type][0] != NULL)
    *ch << msgs[IN_ROOM(ch)->sector_type][0];

  if (msgs[IN_ROOM(ch)->sector_type][1] != NULL)
    act(msgs[IN_ROOM(ch)->sector_type][1], TRUE, ch, NULL,       NULL, TO_ROOM);
  else
    return;

  /* set a wait state */

  WAIT_STATE(ch, 4 RL_SEC);


  /*
   ** search for an object in the room that has a ITEM_BURIED flag
   */

  obj = IN_ROOM(ch)->contents;
  while (obj != NULL)
  {
    if (IS_BURIED(obj))
    {


      chance += (GET_INT(ch) >= 17);

      if ((number(1, 6) <= chance))
      {
        /* Remove the buried bit so the player can see it. */
        REMOVE_BIT_AR(GET_OBJ_EXTRA(obj), ITEM_BURIED);
        act("You found $a $o buried here.\r\n", TRUE, ch, obj,
            NULL, TO_CHAR);
        act("$n has found $a $o buried here.\r\n", TRUE, ch, obj,
            NULL, TO_ROOM);
        obj_from_room(obj);
        obj_to_char(obj, ch);
        return;
      }
    }
    /* go to the next obj in room*/
    obj = obj->next_content;
  }
  /* if the player didn't find anything */
  *ch << "Sorry! You didn't find anything.\r\n";
}

ACMD(do_pageheight)
{
  int num;
  skip_spaces(&argument);
  num = atoi(argument);
  if (num < 5 || num > 75)
  {
    ch->Send( "Please keep the page height between 5 and 75 lines.\r\n");
    return;
  }
  ch->Send( "You set your page height to %d, default is 25\r\n", num);
  PAGEHEIGHT(ch) = num;
  ch->save();
}
ACMD(do_pagewidth)
{
  int num;
  skip_spaces(&argument);
  num = atoi(argument);
  if (num < 20 || num > 200)
  {
    ch->Send( "Please keep the page width between 20 and 200 characters.\r\n");
    return;
  }
  ch->Send( "You set your page width to %d, default is 80\r\n", num);
  PAGEWIDTH(ch) = num;
  ch->save();
}
