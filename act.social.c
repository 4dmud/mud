/* ************************************************************************
*   File: act.social.c                                  Part of CircleMUD *
*  Usage: Functions to handle socials                                     *
*                                                                         *
*  All rights reserved.  See license.doc for complete information.        *
*                                                                         *
*  Copyright (C) 1993, 94 by the Trustees of the Johns Hopkins University *
*  CircleMUD is based on DikuMUD, Copyright (C) 1990, 1991.               *
************************************************************************ */

#include "conf.h"
#include "sysdep.h"


#include "structs.h"
#include "utils.h"
#include "comm.h"
#include "interpreter.h"
#include "handler.h"
#include "db.h"
#include "spells.h"
#include "descriptor.h"
#include "strutil.h"

/* extern variables */
extern struct room_data *world_vnum[];
extern Descriptor *descriptor_list;
struct command_info *complete_cmd_info;

/* extern functions */
char *fread_action(FILE * fl, int nr);

/* local functions */
int find_action(int cmd);
ACMD(do_action);
ACMD(do_insult);
void boot_social_messages(void);
void thefree_social_messages(void);
void free_action(struct social_messg *mess);
void free_command_list(void);
const char * afar_act(int afar, const char * string, char *buf, size_t len);


#define NUM_RESERVED_CMDS     15

ACMD(do_action)
{
  int act_nr,fnum;
//   int fnumbak, chars_found=0,i;
  struct social_messg *action;
  Character *vict;
  struct obj_data *targ;
  char arg[MAX_INPUT_LENGTH], part[MAX_INPUT_LENGTH];
  char arg1[MAX_INPUT_LENGTH],*arg2;
  arg2=arg;
  if ((act_nr = find_action(cmd)) < 0)
  {
    send_to_char("That action is not supported.\r\n", ch);
    return;
  }

  action = &soc_mess_list[act_nr];

  if (!argument || !*argument)
  {
    ch->Send( "%s\r\n", action->char_no_arg);
    act(action->others_no_arg, action->hide, ch, 0, 0, TO_ROOM);
    return;
  }


  two_arguments(argument, arg, part);

  if ((!action->char_body_found) && (*part))
  {
    ch->Send( "Sorry, this social does not support body parts.\r\n");
    return;
  }

  if (!action->char_found)
    *arg = '\0';

  if (action->char_found && argument)
    one_argument(argument, arg);
  else
    *arg = '\0';
  if (!(fnum = get_number(&arg2)))
  {
    if (action->not_found)
      ch->Send( "%s\r\n", action->not_found);
    else
      ch->Send( "I don't see anything by that name here.\r\n");
    return;
  }

  strncpy(arg1,arg,MAX_INPUT_LENGTH);
//   fnumbak=fnum;
  if ((PLR_FLAGGED(ch, PLR_HERO) || PLR_FLAGGED(ch, PLR_NEWBIE_HLPR) || GET_LEVEL(ch) > LVL_HERO)){
//      vict=NULL;
//     for(i=1;i<=fnum;i++){
//       int j=i;
//       vict = get_char_vis(ch, arg1, &j, FIND_CHAR_WORLD);
//       if(!vict) break;
//       if(IS_NPC(vict) && IN_ROOM(ch)!=IN_ROOM(vict)) fnum++;
//       if(!IS_NPC(vict) || IN_ROOM(ch)==IN_ROOM(vict)) chars_found++;
//     }
//     fnum=fnumbak-chars_found;
    vict=get_char_vis(ch, arg1, &fnum,FIND_CHAR_ROOM);
    if(!vict)
      vict=get_player_vis(ch,arg1,&fnum,FIND_CHAR_NOTINROOM);
  }
  else
    vict = get_char_vis(ch, arg1, &fnum, FIND_CHAR_ROOM);
  if (!vict || (IS_NPC(vict) && IN_ROOM(vict) != IN_ROOM(ch)))
  {
    if (action->char_obj_found)
    {

      targ = get_obj_in_list_vis(ch, arg, &fnum, ch->carrying);
      if (!targ) targ = get_obj_in_list_vis(ch, arg, &fnum, ch->in_room->contents);
      if (targ)
      {
        act(action->char_obj_found, action->hide, ch, targ, 0, TO_CHAR);
        if (ch && !PLR_FLAGGED(ch, PLR_COVENTRY))
        act(action->others_obj_found, action->hide, ch, targ, 0, TO_ROOM);
        return;
      }
    }
    if (action->not_found)
      ch->Send( "%s\r\n", action->not_found);
    else
      ch->Send( "I don't see anything by that name here.\r\n");
    return;
  }
  else if (vict == ch)
  {
    if (action->char_auto)
      ch->Send( "%s\r\n", action->char_auto);
    else
      ch->Send( "Erm, no.\r\n");
    act(action->others_auto, action->hide, ch, 0, 0, TO_ROOM);
    return;
  }
  if (GET_POS(vict) < action->min_victim_position)
    act("$N is not in a proper position for that.", FALSE, ch, 0, vict, TO_CHAR | TO_SLEEP);
  else
  {
  char buf[MAX_INPUT_LENGTH];
    if (*part)
    {
      act(afar_act(!HERE(vict, ch), action->char_body_found, buf, sizeof(buf)), 0, ch, (struct obj_data *)part, vict, TO_CHAR | TO_SLEEP);
      if (ch && !PLR_FLAGGED(ch, PLR_COVENTRY))
      act(afar_act(!HERE(vict, ch), action->others_body_found, buf, sizeof(buf)), action->hide, ch, (struct obj_data *)part, vict, TO_NOTVICT);
      if (ch && !PLR_FLAGGED(ch, PLR_COVENTRY))
      act(afar_act(!HERE(vict, ch), action->vict_body_found, buf, sizeof(buf)), action->hide, ch, (struct obj_data *)part, vict, TO_VICT);
    }
    else
    {
      act(afar_act(!HERE(vict, ch), action->char_found, buf, sizeof(buf)), 0, ch, 0, vict,   TO_CHAR | TO_SLEEP);
      if (ch && !PLR_FLAGGED(ch, PLR_COVENTRY))
      act(afar_act(!HERE(vict, ch), action->others_found, buf, sizeof(buf)), action->hide, ch, 0, vict,   TO_NOTVICT);
      if (ch && !PLR_FLAGGED(ch, PLR_COVENTRY))
      act(afar_act(!HERE(vict, ch), action->vict_found, buf, sizeof(buf)), action->hide, ch, 0, vict,  TO_VICT);
    }
  }
}
//}

const char * afar_act(int afar, const char * string, char *buf, size_t len) {
if (!afar) {
return string;
} else {
snprintf(buf, len, "From afar, %s", string);
return (const char *)buf;
}
}

ACMD(do_insult)
{
  Character *victim;

  char arg[MAX_INPUT_LENGTH];
  one_argument(argument, arg);

  if (*arg)
  {
    if (!(victim = get_char_room_vis(ch, arg, NULL)))
      send_to_char("Can't hear you!\r\n", ch);
    else
    {
      if (victim != ch)
      {
        ch->Send( "You insult %s.\r\n", GET_NAME(victim));

        switch (number(0, 2))
        {
        case 0:
          if (GET_SEX(ch) == SEX_MALE)
          {
            if (GET_SEX(victim) == SEX_MALE)
              act("$n accuses you of fighting like a woman!",
                  FALSE, ch, 0, victim, TO_VICT);
            else
              act("$n says that women can't fight.", FALSE,
                  ch, 0, victim, TO_VICT);
          }
          else
          {	/* Ch == Woman */
            if (GET_SEX(victim) == SEX_MALE)
              act("$n accuses you of having the smallest... (brain?)", FALSE, ch, 0, victim, TO_VICT);
            else
              act("$n tells you that you'd lose a beauty contest against a troll.", FALSE, ch, 0, victim, TO_VICT);
          }
          break;
        case 1:
	if (ch && !PLR_FLAGGED(ch, PLR_COVENTRY))
          act("$n calls your mother a bitch!", FALSE, ch, 0,
              victim, TO_VICT);
          break;
        default:
if (ch && !PLR_FLAGGED(ch, PLR_COVENTRY))
          act("$n tells you to get lost!", FALSE, ch, 0, victim,
              TO_VICT);
          break;
        }		/* end switch */
if (ch && !PLR_FLAGGED(ch, PLR_COVENTRY))
        act("$n insults $N.", TRUE, ch, 0, victim, TO_NOTVICT);
      }
      else
      {		/* ch == victim */
        send_to_char("You feel insulted.\r\n", ch);
      }
    }
  }
  else
    send_to_char
    ("I'm sure you don't want to insult *everybody*...\r\n", ch);
}


#if 0
char *fread_action(FILE * fl, int nr)
{
  char buf[MAX_STRING_LENGTH], *rslt;

  fgets(buf, MAX_STRING_LENGTH, fl);
  if (feof(fl))
  {
    log("SYSERR: fread_action: unexpected EOF near action #%d", nr);
    exit(1);
  }
  if (*buf == '#')
    return (NULL);
  else
  {
    *(buf + strlen(buf) - 1) = '\0';
    CREATE(rslt, char, strlen(buf) + 1);
    strcpy(rslt, buf);
    return (rslt);
  }
}
#endif
void boot_social_messages(void)
{
  FILE *fl;
  int nr = 0, hide = 0, min_char_pos = 0, min_pos = 0, min_lvl = 0, curr_soc = -1, retval = 0;
  char next_soc[MAX_STRING_LENGTH], sorted[MAX_INPUT_LENGTH];

  strcpy(sorted, "");
  if (CONFIG_NEW_SOCIALS == TRUE)
  {
    /* open social file */
    if (!(fl = fopen(SOCMESS_FILE_NEW, "r")))
    {
      log("SYSERR: can't open socials file '%s': %s", SOCMESS_FILE_NEW, strerror(errno));
      exit(1);
    }
    /* count socials */
    *next_soc = '\0';
    while (!feof(fl))
    {
      fgets(next_soc, MAX_STRING_LENGTH, fl);
      if (*next_soc == '~') top_of_socialt++;
    }
  }
  else
  { /* old style */

    /* open social file */
    if (!(fl = fopen(SOCMESS_FILE, "r")))
    {
      log("SYSERR: can't open socials file '%s': %s", SOCMESS_FILE, strerror(errno));
      exit(1);
    }
    /* count socials */
    while (!feof(fl))
    {
      fgets(next_soc, MAX_STRING_LENGTH, fl);
      if (*next_soc == '\n' || *next_soc == '\r') top_of_socialt++; /* all socials are followed by a blank line */
    }
  }
  log( "Social table contains %d socials.", top_of_socialt);
  rewind(fl);

  CREATE(soc_mess_list, struct social_messg, top_of_socialt + 1);

  /* now read 'em */
  for (;;)
  {
    fscanf(fl, " %s ", next_soc);

    if (*next_soc == '$') break;

    if (fscanf(fl, " %s %d %d %d %d \n", sorted, &hide, &min_char_pos, &min_pos, &min_lvl) != 5)
    {

      log("SYSERR: format error in social file near social '%s'(Social Num:%d)(count:%d)\n",
          next_soc, curr_soc + 1, retval);
      exit(1);
    }

    curr_soc++;
    soc_mess_list[curr_soc].command = str_dup(next_soc + 1);
    soc_mess_list[curr_soc].sort_as = str_dup(sorted);
    soc_mess_list[curr_soc].hide = hide;
    soc_mess_list[curr_soc].min_char_position = min_char_pos;
    soc_mess_list[curr_soc].min_victim_position = min_pos;
    soc_mess_list[curr_soc].min_level_char = min_lvl;
    /* ------------------------------------------------------------*/
    soc_mess_list[curr_soc].char_no_arg = fread_action(fl, nr);
    soc_mess_list[curr_soc].others_no_arg = fread_action(fl, nr);
    soc_mess_list[curr_soc].char_found = fread_action(fl, nr);

    soc_mess_list[curr_soc].others_found = fread_action(fl, nr);
    soc_mess_list[curr_soc].vict_found = fread_action(fl, nr);
    soc_mess_list[curr_soc].not_found = fread_action(fl, nr);
    soc_mess_list[curr_soc].char_auto = fread_action(fl, nr);
    soc_mess_list[curr_soc].others_auto = fread_action(fl, nr);

    soc_mess_list[curr_soc].char_body_found = fread_action(fl, nr);
    soc_mess_list[curr_soc].others_body_found = fread_action(fl, nr);
    soc_mess_list[curr_soc].vict_body_found = fread_action(fl, nr);
    soc_mess_list[curr_soc].char_obj_found = fread_action(fl, nr);
    soc_mess_list[curr_soc].others_obj_found = fread_action(fl, nr);
  }

  /* close file & set top */
  fclose(fl);
  assert(curr_soc <= top_of_socialt);
  top_of_socialt = curr_soc;
}


void thefree_social_messages(void)
{
  int ac;
  for (ac = 0; ac <= top_of_socialt; ac++)
    free_action(&soc_mess_list[ac]);

  free(soc_mess_list);
}
/* this function adds in the loaded socials and assigns them a command # */
void create_command_list(void)
{
  int i, j, k;
  struct social_messg temp;
  extern const struct command_info cmd_info[];

  /* free up old command list */
  if (complete_cmd_info)
    free_command_list();

  /* re check the sort on the socials */
  for (j = 0; j < top_of_socialt; j++)
  {
    k = j;
    for (i = j + 1; i <= top_of_socialt; i++)
      if (str_cmp(soc_mess_list[i].sort_as, soc_mess_list[k].sort_as) < 0)
        k = i;
    if (j != k)
    {
      temp = soc_mess_list[j];
      soc_mess_list[j] = soc_mess_list[k];
      soc_mess_list[k] = temp;
    }
  }

  /* count the commands in the command list */
  i = 0;
  while (*cmd_info[i].command != '\n')
    i++;

  CREATE(complete_cmd_info, struct command_info, top_of_socialt + i + 2);

  /* this loop sorts the socials and commands together into one big list */
  i = 0;
  j = 0;
  k = 0;
  while ((*cmd_info[i].command != '\n') || (j <= top_of_socialt))
  {
    if ((i < NUM_RESERVED_CMDS) || (j > top_of_socialt) ||
        (str_cmp(cmd_info[i].sort_as, soc_mess_list[j].sort_as) < 1))
      complete_cmd_info[k++] = cmd_info[i++];
    else
    {
      soc_mess_list[j].act_nr = k;
      complete_cmd_info[k].command = soc_mess_list[j].command;
      complete_cmd_info[k].sort_as = soc_mess_list[j].sort_as;
      complete_cmd_info[k].minimum_position = soc_mess_list[j].min_char_position;
      complete_cmd_info[k].command_pointer = do_action;
      complete_cmd_info[k].minimum_level = soc_mess_list[j++].min_level_char;
      complete_cmd_info[k].cmd_bits = 0;
      complete_cmd_info[k++].subcmd = 0;
    }
  }


  complete_cmd_info[k].command = "\n";
  complete_cmd_info[k].sort_as = "zzzzzzz";
  complete_cmd_info[k].minimum_position = 0;
  complete_cmd_info[k].command_pointer = 0;
  complete_cmd_info[k].minimum_level = 0;
  complete_cmd_info[k].cmd_bits = 0;
  complete_cmd_info[k].subcmd = 0;
  log( "Command info rebuilt, %d total commands.", k);
}

char *fread_action(FILE *fl, int nr)
{
  char buf[MAX_STRING_LENGTH], *bpt;

  fgets(buf, MAX_STRING_LENGTH, fl);
  if (feof(fl))
  {
    log("SYSERR: fread_action: unexpected EOF near action #%d", nr);
    exit(1);
  }
  bpt = buf;
  while (isspace(*bpt)) bpt++;
  if (*bpt == '#')
    return (NULL);

  buf[strlen(buf) - 1] = '\0';
  return (strdup(buf));
}




void free_command_list(void)
{
  
  #if 0
  int i;
 for (i = 0;(complete_cmd_info[i].command[0]=='\n');i++) {

 if (complete_cmd_info[i].command[0]=='\n') {
  free_string((char *)complete_cmd_info[i].command); /* special case, the terminator */
  free_string((char *)complete_cmd_info[i].sort_as);
  }
  }
  #endif
  free(complete_cmd_info);
  complete_cmd_info = NULL;
}


void free_social_messages(void)
{
  struct social_messg *mess;
  int i;

  for (i = 0;i <= top_of_socialt;i++)
  {
    mess = &soc_mess_list[i];
    free_action(mess);
  }
  free(soc_mess_list);

}

void free_action(struct social_messg *mess)
{
  if (mess->command) free(mess->command);
  if (mess->sort_as) free(mess->sort_as);
  if (mess->char_no_arg) free(mess->char_no_arg);
  if (mess->others_no_arg) free(mess->others_no_arg);
  if (mess->char_found) free(mess->char_found);
  if (mess->others_found) free(mess->others_found);
  if (mess->vict_found) free(mess->vict_found);
  if (mess->char_body_found) free(mess->char_body_found);
  if (mess->others_body_found) free(mess->others_body_found);
  if (mess->vict_body_found) free(mess->vict_body_found);
  if (mess->not_found) free(mess->not_found);
  if (mess->char_auto) free(mess->char_auto);
  if (mess->others_auto) free(mess->others_auto);
  if (mess->char_obj_found) free(mess->char_obj_found);
  if (mess->others_obj_found) free(mess->others_obj_found);
  memset(mess, 0, sizeof(struct social_messg));
}

int find_action(int cmd)
{
  int bot, top, mid;

  bot = 0;
  top = top_of_socialt;

  if (top < 0)
    return (-1);

  for (;;)
  {
    mid = (bot + top) / 2;

    if (soc_mess_list[mid].act_nr == cmd)
      return (mid);
    if (bot >= top)
      return (-1);

    if (soc_mess_list[mid].act_nr > cmd)
      top = --mid;
    else
      bot = ++mid;
  }
}

