/*************************************************************************
*   File: act.comm.c                                    Part of CircleMUD *
*  Usage: Player-level communication commands                             *
*                                                                         *
*  All rights reserved.  See license.doc for complete information.        *
*                                                                         *
*  Copyright (C) 1993, 94 by the Trustees of the Johns Hopkins University *
*  CircleMUD is based on DikuMUD, Copyright (C) 1990, 1991.               *
************************************************************************ */

/*
 * $Log: act.comm.c,v $
 * Revision 1.4  2004/12/04 07:42:36  w4dimenscor
 * fixed the locker bug, and the format error in clan tells, and a few other cleanups
 *
 * Revision 1.3  2004/11/27 22:34:58  w4dimenscor
 * fixed the skills attacks, added an extra multi to elemental spells for when they are affected by mind enhancements
 *
 * Revision 1.2  2004/11/20 02:33:25  w4dimenscor
 * updated and cleaned up the script system
 *
 * Revision 1.1.1.1  2004/11/12 02:16:11  w4dimenscor
 * Initial clean submission of 4Dimensions src code
 *
 * Revision 1.32  2004/09/18 10:47:02  molly
 * fixed up more memory errors, added normal help back in.
 *
 * Revision 1.31  2004/08/22 00:50:46  molly
 * removed all the origional help code, added the start of the xml reader.
 *
 * Revision 1.30  2004/08/14 23:51:23  malfestus
 * test commit
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
#include "screen.h"
#include "dg_scripts.h"
#include "clan.h"
#include "improved-edit.h"

/* extern variables */
extern int level_can_shout;
extern int holler_move_cost;

void strip_color(char *inbuf);
/* extern functions */
extern int is_ignoring(struct char_data *ch, struct char_data *vict);
extern struct time_info_data time_info;

/* local functions */
void perform_tell(struct char_data *ch, struct char_data *vict, char *arg);
int is_tell_ok(struct char_data *ch, struct char_data *vict);
void add_to_comm(const char *type, const char *text);
ACMD(do_say);
ACMD(do_gsay);
ACMD(do_tell);
ACMD(do_reply);
ACMD(do_spec_comm);
ACMD(do_write);
ACMD(do_page);
ACMD(do_gen_comm);
ACMD(do_qcomm);



/*
 * Drunk struct
 */
struct drunk_struct
{
  int min_drunk_level;
  int number_of_rep;
  char *replacement[11];
};

char *makedrunk(char *string, struct char_data *ch);

/* How to make a string look drunk... by Apex (robink@htsa.hva.nl) */
/* Modified and enhanced for envy(2) by the Maniac from Mythran    */
/* Ported to Stock Circle 3.0 by Haddixx (haddixx@megamed.com)     */

char *makedrunk(char *string, struct char_data *ch)
{

  /* This structure defines all changes for a character */
  static struct drunk_struct drunk[] =
    {
      {
        3, 10,
        {"a", "a", "a", "A", "aa", "ah", "Ah", "ao", "aw", "oa",
         "ahhhh"}
      },
      {8, 5,
       {"b", "b", "b", "B", "B", "vb"}},
      {3, 5,
       {"c", "c", "C", "cj", "sj", "zj"}},
      {5, 2,
       {"d", "d", "D"}},
      {3, 3,
       {"e", "e", "eh", "E"}},
      {4, 5,
       {"f", "f", "ff", "fff", "fFf", "F"}},
      {8, 2,
       {"g", "g", "G"}},
      {9, 6,
       {"h", "h", "hh", "hhh", "Hhh", "HhH", "H"}},
      {7, 6,
       {"i", "i", "Iii", "ii", "iI", "Ii", "ehy"}},
      {9, 5,
       {"j", "j", "jj", "Jj", "jJ", "J"}},
      {7, 2,
       {"k", "k", "K"}},
      {3, 2,
       {"l", "l", "L"}},
      {5, 8,
       {"m", "m", "mm", "mmm", "mmmm", "mmmmm", "MmM", "mM", "M"}},
      {6, 6,
       {"n", "n", "nn", "Nn", "nnn", "nNn", "N"}},
      {3, 6,
       {"o", "o", "ooo", "ao", "aOoo", "Ooo", "ooOo"}},
      {3, 2,
       {"p", "p", "P"}},
      {5, 5,
       {"q", "q", "Q", "ku", "ququ", "kukeleku"}},
      {4, 2,
       {"r", "ar", "R"}},
      {2, 5,
       {"s", "ss", "zzZzssZ", "ZSssS", "sSzzsss", "sSss"}},
      {5, 2,
       {"t", "t", "T"}},
      {3, 6,
       {"u", "u", "uh", "Uh", "Uhuhhuh", "uhU", "uhhu"}},
      {4, 2,
       {"v", "v", "V"}},
      {4, 2,
       {"w", "w", "W"}},
      {5, 6,
       {"x", "x", "X", "ks", "iks", "kz", "xz"}},
      {3, 2,
       {"y", "y", "Y"}},
      {2, 9,
       {"z", "z", "ZzzZz", "Zzz", "Zsszzsz", "szz", "sZZz", "ZSz", "zZ",
        "Z"}}
    };

  char buf[MAX_STRING_LENGTH];	/* this should be enough (?) */
  char temp;
  int pos = 0;
  int randomnum;
  //char debug[256];

  if (GET_COND(ch, DRUNK) > 0)
  {	/* character is drunk */
    do
    {
      temp = toupper(*string);
      if ((temp >= 'A') && (temp <= 'Z'))
      {
        if (GET_COND(ch, DRUNK) >
            drunk[(temp - 'A')].min_drunk_level)
        {
          randomnum =
            number(0, (drunk[(temp - 'A')].number_of_rep));
          strcpy(&buf[pos],
                 drunk[(temp - 'A')].replacement[randomnum]);
          pos +=
            strlen(drunk[(temp - 'A')].replacement[randomnum]);
        }
        else
          buf[pos++] = *string;
      }
      else
      {
        if ((temp >= '0') && (temp <= '9'))
        {
          temp = '0' + number(0, 9);
          buf[pos++] = temp;
        }
        else
          buf[pos++] = *string;
      }
    }
    while (*string++);

    buf[pos] = '\0';	/* Mark end of the string... */
    strcpy(string, buf);
    return (string);
  }
  return (string);		/* character is not drunk, just return the string */
}


ACMD(do_say)
{
  void string_format(BYTE * cmd, LWORD space);
  char buf2[MAX_INPUT_LENGTH];

  skip_spaces(&argument);

  if (!*argument)
    new_send_to_char(ch, "Yes, but WHAT do you want to say?\r\n");
  else
  {
    char buf[MAX_INPUT_LENGTH + 12];

    if (IS_NPC(ch) && IS_AFFECTED(ch, AFF_CHARM))
      return;

    if (AFF_FLAGGED(ch, AFF_POLY_TOAD))
    {
      strcpy(argument, "ribbit");
    }
    else if (AFF_FLAGGED(ch, AFF_POLY_WOLF))
    {
      strcpy(argument, "howl");
    }
    else if (AFF_FLAGGED(ch, AFF_POLY_BOAR))
    {
      strcpy(argument, "snort");
    }
    else if (AFF_FLAGGED(ch, AFF_POLY_BEAR))
    {
      strcpy(argument, "growl");
    }
    else if (AFF_FLAGGED(ch, AFF_POLY_LION))
    {
      strcpy(argument, "roar");
    }
    else {}

    argument = makedrunk(argument, ch);


    if (!IS_NPC(ch) && PRF_FLAGGED(ch, PRF_NOREPEAT))
    {
      new_send_to_char(ch,  "%s", CONFIG_OK);
    }
    else
    {
      //delete_doubledollar(argument);

      if (argument[strlen(argument) - 1] == '?')
      {
        snprintf(buf, sizeof(buf), "You ask, '%s'", argument);
        snprintf(buf2, sizeof(buf), "$n asks, '%s'", argument);
      }
      else if (argument[strlen(argument) - 1] == '!' &&
               argument[strlen(argument) - 2] == '!')
      {
        snprintf(buf, sizeof(buf), "You excitedly exclaim, '%s'",
                 argument);
        snprintf(buf2, sizeof(buf), "$n excitedly exclaims, '%s'",
                 argument);
      }
      else if (argument[strlen(argument) - 1] == '!')
      {
        snprintf(buf, sizeof(buf), "You exclaim, '%s'", argument);
        snprintf(buf2, sizeof(buf), "$n exclaims, '%s'", argument);
      }

      else if (argument[strlen(argument) - 1] == '.' &&
               argument[strlen(argument) - 2] == '.' &&
               argument[strlen(argument) - 3] == '.')
      {
        snprintf(buf, sizeof(buf), "You mutter, '%s'", argument);
        snprintf(buf2, sizeof(buf), "$n mutters, '%s'", argument);
      }
      else if (argument[strlen(argument) - 1] == '.')
      {
        snprintf(buf, sizeof(buf), "You state, '%s'", argument);
        snprintf(buf2, sizeof(buf), "$n states, '%s'", argument);
      }
      else if (argument[strlen(argument) - 1] == ')' &&
               argument[strlen(argument) - 2] == ':' &&
               argument[strlen(argument) - 3] == ' ')
      {
        argument[strlen(argument) - 3] = '\0';
        snprintf(buf, sizeof(buf), "You smile and say, '%s'",
                 argument);
        snprintf(buf2, sizeof(buf), "$n smiles and says, '%s'",
                 argument);
      }
      else if (argument[strlen(argument) - 1] == '(' &&
               argument[strlen(argument) - 2] == ':' &&
               argument[strlen(argument) - 3] == ' ')
      {
        argument[strlen(argument) - 3] = '\0';
        snprintf(buf, sizeof(buf), "You frown and say, '%s'",
                 argument);
        snprintf(buf2, sizeof(buf), "$n frowns and says, '%s'",
                 argument);
      }
      else if (argument[strlen(argument) - 1] == ')'
               && argument[strlen(argument) - 2] == '-'
               && argument[strlen(argument) - 3] == ':'
               && argument[strlen(argument) - 4] == ' ')
      {
        argument[strlen(argument) - 4] = '\0';
        snprintf(buf, sizeof(buf), "You smile and say, '%s'",
                 argument);
        snprintf(buf2, sizeof(buf), "$n smiles and says, '%s'",
                 argument);
      }
      else if (argument[strlen(argument) - 1] == ')'
               && argument[strlen(argument) - 2] == '-'
               && argument[strlen(argument) - 3] == ';'
               && argument[strlen(argument) - 4] == ' ')
      {
        argument[strlen(argument) - 4] = '\0';
        snprintf(buf, sizeof(buf), "You wink and say, '%s'",
                 argument);
        snprintf(buf2, sizeof(buf), "$n winks and says, '%s'",
                 argument);
      }
      else if (argument[strlen(argument) - 1] == ')'
               && argument[strlen(argument) - 2] == ';'
               && argument[strlen(argument) - 3] == ' ')
      {
        argument[strlen(argument) - 3] = '\0';
        snprintf(buf, sizeof(buf), "You wink and say, '%s'",
                 argument);
        snprintf(buf2, sizeof(buf), "$n winks and says, '%s'",
                 argument);
      }
      else if (LOWER(argument[strlen(argument) - 1]) == 'p'
               && argument[strlen(argument) - 2] == '-'
               && argument[strlen(argument) - 3] == ':'
               && argument[strlen(argument) - 4] == ' ')
      {
        argument[strlen(argument) - 4] = '\0';
        snprintf(buf, sizeof(buf), "You make a face and say, '%s'",
                 argument);
        snprintf(buf2, sizeof(buf), "$n makes a face and says, '%s'",
                 argument);
      }
      else if (LOWER(argument[strlen(argument) - 1]) == 'p'
               && argument[strlen(argument) - 2] == ';'
               && argument[strlen(argument) - 3] == ' ')
      {
        argument[strlen(argument) - 3] = '\0';
        snprintf(buf, sizeof(buf), "You wink and say, '%s'",
                 argument);
        snprintf(buf2, sizeof(buf), "$n winks and says, '%s'",
                 argument);
      }
      else if (LOWER(argument[strlen(argument) - 1]) == 'p'
               && argument[strlen(argument) - 2] == ':'
               && argument[strlen(argument) - 3] == ' ')
      {
        argument[strlen(argument) - 3] = '\0';
        snprintf(buf, sizeof(buf), "You wink and say, '%s'",
                 argument);
        snprintf(buf2, sizeof(buf), "$n winks and says, '%s'",
                 argument);
      }
      else
      {
        snprintf(buf, sizeof(buf), "You say, '%s'", argument);
        snprintf(buf2, sizeof(buf), "$n says, '%s'", argument);
      }


      if (subcmd == SCMD_RSAY)
      {
        snprintf(buf, sizeof(buf),  "{cg[%s]{cw %s{c0", race_name(ch), argument);
        snprintf(buf2, sizeof(buf), "{cg[%s]{cw %s{c0", race_name(ch), argument);
        struct char_data *people;
        for (people = IN_ROOM(ch)->people; people; people = people->next_in_room)
        {
          if (people == ch)
            continue;
          if (IS_NPC(people))
            continue;
          if (PLR_FLAGGED(ch, PLR_COVENTRY))
            continue;
          if (GET_RACE(ch) == GET_RACE(people))
            perform_act(buf, ch, NULL, NULL, people);
          else
            perform_act("$n chitters in a language that you can't quite understand.", ch, NULL, NULL, people);

        }
        act(buf, FALSE, ch, NULL, NULL, TO_CHAR);
      }
      else
      {
        act(buf, FALSE, ch, NULL, NULL, TO_CHAR);
        if (!PLR_FLAGGED(ch, PLR_COVENTRY))
          act(buf2, FALSE, ch, NULL, NULL /*argument*/, TO_ROOM | DG_NO_TRIG);
      }
    }
    /* trigger check */
    speech_mtrigger(ch, argument);
    speech_wtrigger(ch, argument);
    speech_otrigger(ch, argument);
  }


}

ACMD(do_sayto)
{
  size_t len = 0;
  char buf[MAX_INPUT_LENGTH];
  CHAR_DATA *vict;

  argument = any_one_arg(argument, buf);
  skip_spaces(&argument);

  if (!*buf || !*argument)
    new_send_to_char(ch,  "Whom do you want to say to.. and what??\r\n");
  else if (!(vict = get_char_vis(ch, buf, NULL, FIND_CHAR_ROOM)))
    new_send_to_char(ch, "%s", CONFIG_NOPERSON);
  else if (vict == ch)
    new_send_to_char(ch, "You can't get your mouth close enough to your ear...\r\n");
  else
  {
    if (IS_NPC(ch) && IS_AFFECTED(ch, AFF_CHARM))
    {
      return;
    }
    char to_char[MAX_STRING_LENGTH];
    char to_room[MAX_STRING_LENGTH];
    char to_vict[MAX_STRING_LENGTH];


    if (AFF_FLAGGED(ch, AFF_POLY_TOAD))
    {
      strcpy(argument, "ribbit");
    }
    else if (AFF_FLAGGED(ch, AFF_POLY_WOLF))
    {
      strcpy(argument, "howl");
    }
    else if (AFF_FLAGGED(ch, AFF_POLY_BOAR))
    {
      strcpy(argument, "snort");
    }
    else if (AFF_FLAGGED(ch, AFF_POLY_BEAR))
    {
      strcpy(argument, "growl");
    }
    else if (AFF_FLAGGED(ch, AFF_POLY_LION))
    {
      strcpy(argument, "roar");
    }

    argument = makedrunk(argument, ch);

    len = strlen(argument);

    if (!IS_NPC(ch) && PRF_FLAGGED(ch, PRF_NOREPEAT))
    {
      new_send_to_char(ch,  "%s", CONFIG_OK);
    }
    else
    {
      //delete_doubledollar(argument);

      if (argument[len - 1] == '?')
      {
        snprintf(to_char, sizeof(to_char), "You ask $N, '%s'", argument);
        snprintf(to_room, sizeof(to_room), "$n asks $N, '%s'", argument);
        snprintf(to_vict, sizeof(to_vict), "$n asks you, '%s'", argument);
      }
      else if (argument[len - 1] == '!' &&
               argument[len - 2] == '!')
      {
        snprintf(to_char, sizeof(to_char), "You excitedly exclaim to $N, '%s'", argument);
        snprintf(to_room, sizeof(to_room), "$n excitedly exclaims to $N, '%s'", argument);
        snprintf(to_vict, sizeof(to_vict), "$n excitedly exclaims to you, '%s'", argument);
      }
      else if (argument[len - 1] == '!')
      {
        snprintf(to_char, sizeof(to_char), "You exclaim to $N, '%s'", argument);
        snprintf(to_room, sizeof(to_room), "$n exclaims to $N, '%s'", argument);
        snprintf(to_vict, sizeof(to_vict), "$n exclaims to you, '%s'", argument);
      }
      else if (argument[len - 1] == '.' &&
               argument[len - 2] == '.' &&
               argument[len - 3] == '.')
      {
        snprintf(to_char, sizeof(to_char), "You mutter to $N, '%s'", argument);
        snprintf(to_room, sizeof(to_room), "$n mutters to you, '%s'", argument);
        snprintf(to_vict, sizeof(to_vict), "$n mutters you, '%s'", argument);
      }
      else if (argument[len - 1] == '.')
      {
        snprintf(to_char, sizeof(to_char), "You state to $N, '%s'", argument);
        snprintf(to_room, sizeof(to_room), "$n states to $N, '%s'", argument);
        snprintf(to_vict, sizeof(to_vict), "$n states to you, '%s'", argument);
      }
      else if (argument[len - 1] == ')' &&
               argument[len - 2] == ':' &&
               argument[len - 3] == ' ')
      {
        argument[len - 3] = '\0';
        snprintf(to_char, sizeof(to_char), "You smile at $N and say, '%s'", argument);
        snprintf(to_room, sizeof(to_room), "$n smiles at $N and says, '%s'", argument);
        snprintf(to_vict, sizeof(to_vict), "$n smiles at you and says, '%s'", argument);
      }
      else if (argument[len - 1] == ')'
               && argument[len - 2] == '-'
               && argument[len - 3] == ':'
               && argument[len - 4] == ' ')
      {
        argument[len - 4] = '\0';
        snprintf(to_char, sizeof(to_char), "You smile at $N and say, '%s'", argument);
        snprintf(to_room, sizeof(to_room), "$n smiles at $N and says, '%s'", argument);
        snprintf(to_vict, sizeof(to_vict), "$n smiles at you and says, '%s'", argument);
      }
      else if (argument[len - 1] == ')'
               && argument[len - 2] == '-'
               && argument[len - 3] == ';'
               && argument[len - 4] == ' ')
      {
        argument[len - 4] = '\0';
        snprintf(to_char, sizeof(to_char), "You wink and say, '%s'", argument);
        snprintf(to_room, sizeof(to_room), "$n winks and says, '%s'", argument);
        snprintf(to_vict, sizeof(to_vict), "$n winks at you and says, '%s'", argument);
      }
      else if (argument[len - 1] == ')'
               && argument[len - 2] == ';'
               && argument[len - 3] == ' ')
      {
        argument[len - 3] = '\0';
        snprintf(to_char, sizeof(to_char), "You wink at $N and say, '%s'", argument);
        snprintf(to_room, sizeof(to_room), "$n winks at $N and says, '%s'", argument);
        snprintf(to_vict, sizeof(to_vict), "$n winks at you and says, '%s'", argument);
      }
      else if (argument[len - 1] == 'P'
               && argument[len - 2] == '-'
               && argument[len - 3] == ':'
               && argument[len - 4] == ' ')
      {
        argument[len - 4] = '\0';
        snprintf(to_char, sizeof(to_char), "You make a face at $N and say, '%s'", argument);
        snprintf(to_room, sizeof(to_room), "$n makes a face at $N and says, '%s'", argument);
        snprintf(to_vict, sizeof(to_vict), "$n makes a face at you and asks, '%s'", argument);
      }
      else
      {
        snprintf(to_char, sizeof(to_char), "You say to $N, '%s'", argument);
        snprintf(to_room, sizeof(to_room), "$n says to $N, '%s'", argument);
        snprintf(to_vict, sizeof(to_vict), "$n says to you, '%s'", argument);
      }

      //string_format((BYTE*)buf, 2);
      //string_format((BYTE*)buf2, 2);
      act(to_char, FALSE, ch, NULL, vict, TO_CHAR);
      if (!PLR_FLAGGED(ch, PLR_COVENTRY))
      {
        act(to_vict, FALSE, ch, NULL, vict, TO_VICT);
        act(to_room, FALSE, ch, NULL, vict, TO_NOTVICT | DG_NO_TRIG);
      }
    }
    /* trigger check */
    speech_mtrigger(ch, argument);
    speech_wtrigger(ch, argument);
    speech_otrigger(ch, argument);
  }


}




ACMD(do_gsay)
{
  struct char_data *k;
  struct follow_type *f;

  skip_spaces(&argument);
  if (IS_NPC(ch) && IS_AFFECTED(ch, AFF_CHARM))
    return;

  if (!AFF_FLAGGED(ch, AFF_GROUP))
  {
    send_to_char("But you are not the member of a group!\r\n", ch);
    return;
  }
  if (!*argument)
    send_to_char("Yes, but WHAT do you want to group-say?\r\n", ch);
  else
  {
    char buf[MAX_STRING_LENGTH];
    if (AFF_FLAGGED(ch, AFF_POLY_TOAD))
    {
      strcpy(argument, "ribbit");
    }
    else if (AFF_FLAGGED(ch, AFF_POLY_WOLF))
    {
      strcpy(argument, "howl");
    }
    else if (AFF_FLAGGED(ch, AFF_POLY_BOAR))
    {
      strcpy(argument, "snort");
    }
    else if (AFF_FLAGGED(ch, AFF_POLY_BEAR))
    {
      strcpy(argument, "growl");
    }
    else if (AFF_FLAGGED(ch, AFF_POLY_LION))
    {
      strcpy(argument, "roar");
    }

    if (ch->master)
      k = ch->master;
    else
      k = ch;
    if (!PLR_FLAGGED(ch, PLR_COVENTRY))
    {
      argument = makedrunk(argument, ch);
      snprintf(buf, sizeof(buf), "$n tells the group, '%s'", argument);

      if (AFF_FLAGGED(k, AFF_GROUP) && (k != ch))
        act(buf, FALSE, ch, 0, k, TO_VICT | TO_SLEEP);
      for (f = k->followers; f; f = f->next)
        if (AFF_FLAGGED(f->follower, AFF_GROUP) && (f->follower != ch))
          act(buf, FALSE, ch, 0, f->follower, TO_VICT | TO_SLEEP);
    }
    if (PRF_FLAGGED(ch, PRF_NOREPEAT))
      new_send_to_char(ch,  "%s", CONFIG_OK);
    else
      new_send_to_char(ch, "You tell the group, '%s'\r\n", argument);
  }
}


void perform_tell(struct char_data *ch, struct char_data *vict, char *arg)
{
  char buf[MAX_INPUT_LENGTH];
  if (!PLR_FLAGGED(ch, PLR_COVENTRY))
  {
    arg = makedrunk(arg, ch);
    snprintf(buf, sizeof(buf), "$n tells you, '%s%s%s'",
             CBWHT(vict, C_CMP), arg, CCNRM(vict, C_CMP));
    act(buf, FALSE, ch, 0, vict, TO_VICT | TO_SLEEP);
  }
  if (!IS_NPC(ch) && PRF_FLAGGED(ch, PRF_NOREPEAT))
    new_send_to_char(ch, "%s", CONFIG_OK);
  else
  {
    snprintf(buf, sizeof(buf), "You tell $N, '%s%s%s'",
             CBWHT(ch, C_CMP), arg, CCNRM(ch, C_CMP));
    act(buf, FALSE, ch, 0, vict, TO_CHAR | TO_SLEEP);
  }

  if (!IS_NPC(vict) && !IS_NPC(ch) && !PRF_FLAGGED(vict, PRF_REPLYLOCK))
    GET_LAST_TELL(vict) = GET_IDNUM(ch);
}



int is_tell_ok(struct char_data *ch, struct char_data *vict)
{
  if (IS_NPC(ch) && IS_AFFECTED(ch, AFF_CHARM))
  return FALSE;
  
  if (ch == vict)
  {
    send_to_char("You try to tell yourself something.\r\n", ch);
    return (FALSE);
  }
  if (!IS_NPC(ch) && PRF_FLAGGED(ch, PRF_NOTELL))
  {
    send_to_char("You can't tell other people while you have notell on.\r\n", ch);
    return (FALSE);
  }
  if (ROOM_FLAGGED(ch->in_room, ROOM_SOUNDPROOF))
  {
    send_to_char("The walls seem to absorb your words.\r\n", ch);
    return (FALSE);
  }
  if (!IS_NPC(vict) && !vict->desc)
  {	/* linkless */
    act("$E's linkless at the moment.", FALSE, ch, 0, vict, TO_CHAR | TO_SLEEP);
    return (FALSE);
  }
  if (PLR_FLAGGED(vict, PLR_WRITING))
  {
    act("$E's writing a message right now; try again later.", FALSE, ch, 0, vict, TO_CHAR | TO_SLEEP);
    return (FALSE);
  }
  if ((!IS_NPC(vict) && PRF_FLAGGED(vict, PRF_NOTELL))
      || ROOM_FLAGGED(IN_ROOM(vict), ROOM_SOUNDPROOF))
  {
    act("$E can't hear you.", FALSE, ch, 0, vict, TO_CHAR | TO_SLEEP);
    return (FALSE);
  }
  if (!IS_NPC(ch) && PRF_FLAGGED(vict, PRF_AFK))
  {
    if (!AFK_MSG(vict))
      act("$E is afk right now, try again later.", FALSE, ch, 0, vict, TO_CHAR | TO_SLEEP);
    else
      new_send_to_char(ch, "AFK %s: %s\r\n", GET_NAME(vict), AFK_MSG(vict));
    if (!PRF_FLAGGED(vict, PRF_AFKTELL))
      return (FALSE);
  }
  else if (!IS_NPC(ch) && PRF_FLAGGED(vict, PRF_BUSY))
  {
    if (!BUSY_MSG(vict))
      act("$E is busy right now.", FALSE, ch, 0, vict, TO_CHAR | TO_SLEEP);
    else
      new_send_to_char(ch, "BUSY %s: %s\r\n", GET_NAME(vict), BUSY_MSG(vict));
  }

  return (TRUE);
}

/*
 * Yes, do_tell probably could be combined with whisper and ask, but
 * called frequently, and should IMHO be kept as tight as possible.
 */
ACMD(do_tell)
{
  struct char_data *vict = NULL;
  char buf[MAX_INPUT_LENGTH], buf2[MAX_INPUT_LENGTH];

  half_chop(argument, buf, buf2);


  if (!*buf || !*buf2)
    send_to_char("Who do you wish to tell what??\r\n", ch);
  else if (!(vict = get_player_vis(ch, buf, NULL, FIND_CHAR_WORLD)))
    new_send_to_char(ch, "%s", CONFIG_NOPERSON);
  else if (PRF_FLAGGED(ch, PRF_AFK))
    send_to_char("Impossible...you are afk.\r\n", ch);
  else if (is_ignoring(ch, vict) && (GET_LEVEL(ch) <= LVL_GOD))
    act("$E's ignoring you.", FALSE, ch, 0, vict, TO_CHAR);
  else if (is_tell_ok(ch, vict))
    perform_tell(ch, vict, buf2);
}


ACMD(do_reply)
{
  struct char_data *tch = character_list;

  if (IS_NPC(ch))
    return;

  skip_spaces(&argument);

  if (GET_LAST_TELL(ch) == NOBODY)
    send_to_char("You have no-one to reply to!\r\n", ch);
  else if (!*argument)
    send_to_char("What is your reply?\r\n", ch);
  else
  {
    /*
     * Make sure the person you're replying to is still playing by searching
     * for them.  Note, now last tell is stored as player IDnum instead of
     * a pointer, which is much better because it's safer, plus will still
     * work if someone logs out and back in again.
     */

    /*
     * XXX: A descriptor list based search would be faster although
     *      we could not find link dead people.  Not that they can
     *      hear tells anyway. :) -gg 2/24/98
     */
    while (tch != NULL
           && (IS_NPC(tch) || GET_IDNUM(tch) != GET_LAST_TELL(ch)))
      tch = tch->next;

    if (tch == NULL)
      send_to_char("They are no longer playing.\r\n", ch);
    else if (is_ignoring(ch, tch) && (GET_LEVEL(ch) <= LVL_GOD))
      act("$E's ignoring you.", FALSE, ch, 0, tch, TO_CHAR);
    else if (is_tell_ok(ch, tch))
    {
      argument = makedrunk(argument, ch);
      perform_tell(ch, tch, argument);
    }
  }
}


ACMD(do_spec_comm)
{
  struct char_data *vict;
  const char *action_sing, *action_plur, *action_others;

  char buf[MAX_INPUT_LENGTH], buf2[MAX_INPUT_LENGTH];
if (IS_NPC(ch) && IS_AFFECTED(ch, AFF_CHARM))
  return;

  switch (subcmd)
  {
  case SCMD_WHISPER:
    action_sing = "whisper to";
    action_plur = "whispers to";
    action_others = "$n whispers something to $N.";
    break;
  case SCMD_ASK:
    action_sing = "ask";
    action_plur = "asks";
    action_others = "$n asks $N a question.";
    break;
  default:
    action_sing = "oops";
    action_plur = "oopses";
    action_others = "$n is tongue-tied trying to speak with $N.";
    break;
  }

  half_chop(argument, buf, buf2);

  if (!*buf || !*buf2)
    new_send_to_char(ch,  "Whom do you want to %s.. and what??\r\n", action_sing);
  else if (!(vict = get_char_vis(ch, buf, NULL, FIND_CHAR_ROOM)))
    new_send_to_char(ch, "%s", CONFIG_NOPERSON);
  else if (vict == ch)
    new_send_to_char(ch, "You can't get your mouth close enough to your ear...\r\n");
  else
  {
    if (!PLR_FLAGGED(ch, PLR_COVENTRY))
    {
      argument = makedrunk(buf2, ch);
      snprintf(buf, sizeof(buf), "$n %s you, '%s'", action_plur, buf2);
      act(buf, FALSE, ch, 0, vict, TO_VICT);
    }
    if (PRF_FLAGGED(ch, PRF_NOREPEAT))
      new_send_to_char(ch, "%s", CONFIG_OK);
    else
      new_send_to_char(ch, "You %s %s, '%s'\r\n", action_sing,
                       GET_NAME(vict), buf2);

    act(action_others, FALSE, ch, 0, vict, TO_NOTVICT);
  }
}

#define MAX_NOTE_LENGTH 1000	/* arbitrary */

ACMD(do_write)
{
  struct obj_data *paper, *pen = NULL;
  char *papername, *penname;

  char buf2[MAX_INPUT_LENGTH],buf1[MAX_INPUT_LENGTH];


  papername = buf1;
  penname = buf2;

  two_arguments(argument, papername, penname);

  if (!ch->desc)
    return;

  if (!*papername)
  {		/* nothing was delivered */
    send_to_char
    ("Write?  With what?  ON what?  What are you trying to do?!?\r\n",
     ch);
    return;
  }
  if (*penname)
  {		/* there were two arguments */
    if (!
        (paper =
           get_obj_in_list_vis(ch, papername, NULL, ch->carrying)))
    {
      new_send_to_char(ch, "You have no %s.\r\n", papername);

      return;
    }
    if (!(pen = get_obj_in_list_vis(ch, penname, NULL, ch->carrying)))
    {
      new_send_to_char(ch, "You have no %s.\r\n", penname);
      return;
    }
  }
  else
  {			/* there was one arg.. let's see what we can find */
    if (!
        (paper =
           get_obj_in_list_vis(ch, papername, NULL, ch->carrying)))
    {
      new_send_to_char(ch, "There is no %s in your inventory.\r\n",
                       papername);
      return;
    }
    if (GET_OBJ_TYPE(paper) == ITEM_PEN)
    {	/* oops, a pen.. */
      pen = paper;
      paper = NULL;
    }
    else if (GET_OBJ_TYPE(paper) != ITEM_NOTE)
    {
      send_to_char("That thing has nothing to do with writing.\r\n",
                   ch);
      return;
    }
    /* One object was found.. now for the other one. */
    if (!GET_EQ(ch, WEAR_HOLD))
    {
      new_send_to_char(ch, "You can't write with %s %s alone.\r\n",
                       AN(papername), papername);

      return;
    }
    if (PLR_FLAGGED(ch, PLR_COVENTRY))
    {
      new_send_to_char(ch, "That thing has nothing to do with writing.\r\n");
      return;
    }
    if (!CAN_SEE_OBJ(ch, GET_EQ(ch, WEAR_HOLD)))
    {
      send_to_char
      ("The stuff in your hand is invisible!  Yeech!!\r\n", ch);
      return;
    }
    if (pen)
      paper = GET_EQ(ch, WEAR_HOLD);
    else
      pen = GET_EQ(ch, WEAR_HOLD);
  }

  /* ok.. now let's see what kind of stuff we've found */
  if (GET_OBJ_TYPE(pen) != ITEM_PEN)
    act("$p is no good for writing with.", FALSE, ch, pen, 0, TO_CHAR);
  else if (GET_OBJ_TYPE(paper) != ITEM_NOTE)
    act("You can't write on $p.", FALSE, ch, paper, 0, TO_CHAR);

  else
  {
    char *backstr = NULL;

    /* Something on it, display it as that's in input buffer. */
    if (paper->action_description)
    {
      backstr = strdup(paper->action_description);
      new_send_to_char(ch, "There's something written on it already:\r\n");
      new_send_to_char(ch, "%s", paper->action_description);
    }

    /* we can write - hooray! */

    act("$n begins to jot down a note.", TRUE, ch, 0, 0, TO_ROOM);
    send_editor_help(ch->desc);
    string_write(ch->desc, &paper->action_description, MAX_NOTE_LENGTH, 0, backstr);
    SET_BIT_AR(GET_OBJ_EXTRA(paper), ITEM_UNIQUE_SAVE);
  }
}

ACMD(do_comm)
{
  struct comm_data * com;
  int i;
  skip_spaces(&argument);
  if (IS_NPC(ch))
  {
    new_send_to_char(ch, "Not mobs sorry!");
    return;
  }
  if (!argument || !*argument)
  {
    new_send_to_char(ch, "\"<channel>\r\n");
    return;
  }
  if (is_prefix(argument, "hero") && !(PLR_FLAGGED(ch, PLR_HERO) || GET_LEVEL(ch) > LVL_HERO))
  {
    new_send_to_char(ch, "Sorry, but you aren't heroic enough!");
    return;
  }
  for (com = comlist, i = 0; com && i < 15; com = com->next)
    if (is_prefix(argument, com->type))
      new_send_to_char(ch, "%2d: %s\r\n",i++, com->text);

  if (i == 0)
    new_send_to_char(ch, "None.\r\n");

}



ACMD(do_page)
{
  struct descriptor_data *d;
  struct char_data *vict;
  char arg[MAX_INPUT_LENGTH], buf2[MAX_INPUT_LENGTH];
  char buf[MAX_STRING_LENGTH];


  half_chop(argument, arg, buf2);

  if (IS_NPC(ch))
    send_to_char("Monsters can't page.. go away.\r\n", ch);
  else if (!*arg)
    send_to_char("Whom do you wish to page?\r\n", ch);
  else
  {
    snprintf(buf, sizeof(buf), "\007\007*$n* %s", buf2);
    if (!str_cmp(arg, "all"))
    {
      if (GET_LEVEL(ch) > LVL_GOD)
      {
        for (d = descriptor_list; d; d = d->next)
          if (STATE(d) == CON_PLAYING && d->character)
            act(buf, FALSE, ch, 0, d->character, TO_VICT);
      }
      else
        send_to_char("You will never be godly enough to do that!\r\n", ch);
      return;
    }
    if ((vict = get_char_vis(ch, arg, NULL, FIND_CHAR_WORLD)) != NULL)
    {
      if (IS_NPC(vict) || (is_ignoring(ch, vict) && GET_LEVEL(ch) <= LVL_GOD))
      {
        act("$E's ignoring you.", FALSE, ch, 0, vict, TO_CHAR);
        return;
      }

      if (!ROOM_FLAGGED(vict->in_room, ROOM_SOUNDPROOF))
      {
        if (!PLR_FLAGGED(ch, PLR_COVENTRY))
          act(buf, FALSE, ch, 0, vict, TO_VICT);
        if (PRF_FLAGGED(ch, PRF_NOREPEAT))
          new_send_to_char(ch, "%s", CONFIG_OK);
        else
          act(buf, FALSE, ch, 0, vict, TO_CHAR);
      }
      else
        send_to_char("That person is in a soundproof room!\r\n",
                     ch);
    }
    else
      send_to_char("That person is not in the game!\r\n", ch);
  }
}


/**********************************************************************
 * generalized communication func, originally by Fred C. Merkel (Torg)*
 **********************************************************************/
int has_char(const char *txt, char character)
{
  int i;

  for (i = 0; txt[i]; i++)
    if (txt[i] == character)
      return 1;

  return 0;
}


ACMD(do_gen_comm)
{
  struct descriptor_data *i;
  char color_on[24];
  char buf[MAX_INPUT_LENGTH];
  char buf1[MAX_INPUT_LENGTH];

  /* Array of flags which must _not_ be set in order for comm to be heard */
  int channels[] = {
                     0,
                     PRF_DEAF,
                     PRF_NOGOSS,
                     PRF_NOAUCT,
                     PRF_NOGRATZ,
                     PRF_NOIC,
                     PRF_NONEWBIE,
                     PRF_NONEWBIE,
                     PRF_NOHERO,
                     PRF_NONEWBIE,
                     PRF_OOC,
                     0
                   };
  /*
      * com_msgs: [0] Message if you can't perform the action because of noshout
      *           [1] name of the action
      *           [2] message if you're not on the channel
      *           [3] a color string.
      */
  const char *com_msgs[][6] =
    {
      {"You cannot holler!!\r\n",
       "holler",
       "",
       KYEL
      },

      {"You cannot shout!!\r\n",
       "shout",
       "Turn off your noshout flag first!\r\n",
       KYEL},

      {"You cannot gossip!!\r\n",
       "gossip",
       "You aren't even on the channel!\r\n",
       KYEL},

      {"You cannot auction!!\r\n",
       "auction",
       "You aren't even on the channel!\r\n",
       KMAG},

      {"You cannot congratulate!\r\n",
       "congrat",
       "You aren't even on the channel!\r\n",
       KGRN},

      {"You cannot IC!\r\n",
       "IC",
       "You aren't even on the channel!\r\n",
       KCYN},

      {"You cannot use the answer [helper] channel!\r\n",
       "answer",
       "You aren't even on the channel!\r\n",
       KGRN},

      {"You cannot use the question [helper] channel!\r\n",
       "ask",
       "You aren't even on the channel!\r\n",
       KGRN},

      {"You cannot use the hero channel!\r\n",
       "hero gossip",
       "You aren't even on the channel!\r\n",
       "{cL"},

      {"You cannot use the newbie channel!\r\n",
       "newbie",
       "You aren't even on the channel!\r\n",
       "{cg"},

      {"You cannot use the ooc channel!\r\n",
       "{ccOOC{c0",
       "You aren't even on the channel!\r\n",
       "{cw"}

    };
  /* to keep pets, etc from being ordered to shout */
  if ((!ch->desc) && (!HAS_SCRIPT(ch) || IS_AFFECTED(ch, AFF_CHARM)))
    return;

  if (PLR_FLAGGED(ch, PLR_NOSHOUT))
  {
    send_to_char(com_msgs[subcmd][0], ch);
    return;
  }
  if (ROOM_FLAGGED(ch->in_room, ROOM_SOUNDPROOF))
  {
    send_to_char("The walls seem to absorb your words.\r\n", ch);
    return;
  }
  if (!PLR_FLAGGED(ch, PLR_ROLEPLAYER) && subcmd == SCMD_IC)
  {
    send_to_char("This channel reserved for roleplay use only.\r\n",
                 ch);
    return;
  }
  if (GET_LEVEL(ch) < LVL_HERO)
    if (!PLR_FLAGGED(ch, PLR_HERO) && subcmd == SCMD_HERO)
    {
      send_to_char("This channel reserved for hero use only.\r\n",
                   ch);
      return;
    }
  /* level_can_shout defined in config.c */
  if (GET_LEVEL(ch) < CONFIG_LEVEL_CAN_SHOUT)
  {
    new_send_to_char(ch, "You must be at least level %d before you can %s.\r\n", CONFIG_LEVEL_CAN_SHOUT, com_msgs[subcmd][1]);

    return;
  }
  /* make sure the char is on the channel */
  if (PRF_FLAGGED(ch, channels[subcmd]))
  {
    new_send_to_char(ch, "%s", com_msgs[subcmd][2]);
    return;
  }
  /* skip leading spaces */
  if (has_char(argument, '{'))
  {
    new_send_to_char(ch, "No color code in the open channels please.\r\n");
    return;
  }
  skip_spaces(&argument);

  /* make sure that there is something there to say! */
  if (!*argument)
  {
    new_send_to_char(ch, "Yes, %s, fine, %s we must, but WHAT???\r\n",
                     com_msgs[subcmd][1], com_msgs[subcmd][1]);
    return;
  }
  if (AFF_FLAGGED(ch, AFF_POLY_TOAD))
  {
    strcpy(argument, "ribbit");
  }
  else if (AFF_FLAGGED(ch, AFF_POLY_WOLF))
    {
      strcpy(argument, "howl");
    }
    else if (AFF_FLAGGED(ch, AFF_POLY_BOAR))
      {
        strcpy(argument, "snort");
      }
      else if (AFF_FLAGGED(ch, AFF_POLY_BEAR))
        {
          strcpy(argument, "growl");
        }
        else if (AFF_FLAGGED(ch, AFF_POLY_LION))
          {
            strcpy(argument, "roar");
          }
  argument = makedrunk(argument, ch);
  if (subcmd == SCMD_HOLLER)
  {
    if (GET_MOVE(ch) < CONFIG_HOLLER_MOVE_COST)
    {
      send_to_char("You're too exhausted to holler.\r\n", ch);
      return;
    }
    else
      alter_move(ch, CONFIG_HOLLER_MOVE_COST);
  }
  /* set up the color on code */
  strcpy(color_on, com_msgs[subcmd][3]);

  if (first_word_is_name(ch, argument))
  {
    /* first, set up strings to be given to the communicator */
    if (PRF_FLAGGED(ch, PRF_NOREPEAT))
      new_send_to_char(ch, "%s", CONFIG_OK);
    else
      new_send_to_char(ch, "%s%s: %s%s\r\n", COLOR_LEV(ch) >= C_CMP ? color_on : "", com_msgs[subcmd][1], CAP(argument), CCNRM(ch, C_CMP));

    if (!PLR_FLAGGED(ch, PLR_COVENTRY))

      snprintf(buf, sizeof(buf), "%s: %s",com_msgs[subcmd][1],CAP( argument));
    else
      return;
  }
  else
  {

    /* first, set up strings to be given to the communicator */
    if (PRF_FLAGGED(ch, PRF_NOREPEAT))
      new_send_to_char(ch, "%s", CONFIG_OK);
    else
      new_send_to_char(ch, "%sYou %s, '%s'%s\r\n", COLOR_LEV(ch) >= C_CMP ? color_on : "", com_msgs[subcmd][1], argument, CCNRM(ch, C_CMP));

    if (!PLR_FLAGGED(ch, PLR_COVENTRY))
    {
      snprintf(buf, sizeof(buf), "$n %ss, '%s'", com_msgs[subcmd][1], argument);
    }
    else
    {
      return;
    }
  }

  snprintf(buf1, sizeof(buf1), "%s%s %ss, '%s'%s",color_on, GET_NAME(ch), com_msgs[subcmd][1], argument, KNRM);
  comlog("%s", buf1);
  add_to_comm( com_msgs[subcmd][1], buf1);

  /* now send all the strings out */
  for (i = descriptor_list; i; i = i->next)
  {
    if (STATE(i) == CON_PLAYING && i != ch->desc && i->character &&
        !PRF_FLAGGED(i->character, channels[subcmd]) &&
        !PLR_FLAGGED(i->character, PLR_WRITING) &&
        !ROOM_FLAGGED(i->character->in_room, ROOM_SOUNDPROOF))
    {
      if (subcmd == SCMD_IC
          && !PLR_FLAGGED(i->character, PLR_ROLEPLAYER))
        continue;

      if (GET_LEVEL(i->character) < LVL_HERO && (subcmd == SCMD_HERO && !PLR_FLAGGED(i->character, PLR_HERO)))
        continue;

      if (subcmd == SCMD_SHOUT &&
          ((ch->in_room->zone != i->character->in_room->zone)
           || !AWAKE(i->character)))
        continue;

      if (COLOR_LEV(i->character) >= C_NRM)
        send_to_char(color_on, i->character);
      act(buf, FALSE, ch, 0, i->character, TO_VICT | TO_SLEEP);
      if (COLOR_LEV(i->character) >= C_NRM)
        send_to_char(KNRM, i->character);
    }
  }
}



ACMD(do_qcomm)
{
  struct descriptor_data *i;
  char buf[MAX_INPUT_LENGTH];
  char buf1[MAX_INPUT_LENGTH];

  if (!PRF_FLAGGED(ch, PRF_QUEST))
  {
    send_to_char("You aren't even part of the quest!\r\n", ch);
    return;
  }
  if (IS_NPC(ch) && IS_AFFECTED(ch, AFF_CHARM))
  return;
  skip_spaces(&argument);

  if (!*argument)
    new_send_to_char(ch, "%s?  Yes, fine, %s we must, but WHAT??\r\n",
                     CMD_NAME, CMD_NAME);
  else
  {
    if (ROOM_FLAGGED(ch->in_room, ROOM_SOUNDPROOF))
      send_to_char("The walls seem to absorb your words.\r\n", ch);
    else if (PRF_FLAGGED(ch, PRF_NOREPEAT))
      new_send_to_char(ch, "%s", CONFIG_OK);
    else
    {
      if (subcmd == SCMD_QSAY)
        snprintf(buf, sizeof(buf), "You quest-say, '%s'",
                 argument);
      else
        strcpy(buf, argument);
      act(buf, FALSE, ch, 0, argument, TO_CHAR);
    }
    if (!PLR_FLAGGED(ch, PLR_COVENTRY))
    {
      if (subcmd == SCMD_QSAY)
      {
        snprintf(buf, sizeof(buf), "$n quest-says, '%s'", argument);

        snprintf(buf1, sizeof(buf1), "%s quest-says, '%s'",GET_NAME(ch), argument);
        comlog("%s", buf1);

        add_to_comm( "quest", buf1);
      }
      else
        strcpy(buf, argument);

      for (i = descriptor_list; i; i = i->next)
        if (STATE(i) == CON_PLAYING && i != ch->desc &&
            PRF_FLAGGED(i->character, PRF_QUEST) &&
            !ROOM_FLAGGED(i->character->in_room, ROOM_SOUNDPROOF))
          act(buf, 0, ch, 0, i->character, TO_VICT | TO_SLEEP);

    }
  }
}

/*
 * I didn't write this command, i just modified it, all credits should
 * go to original coder
 */
ACMD(do_ctell)
{
  struct descriptor_data *i;
  int minlev = 1, c = 0;
  char level_string[10];

  skip_spaces(&argument);
  if (IS_NPC(ch) && IS_AFFECTED(ch, AFF_CHARM))
  return;
  /*
   * The syntax of ctell for imms is different then for morts
   * mort: ctell <bla bla bla>    imms: ctell <clan_num> <bla bla bla>
   * Imms cannot actually see ctells but they can send them
   *
   if (GET_LEVEL(ch) >= LVL_GOD){
   c = atoi (argument);
   if ((c <= 0) || (c > num_of_clans)){
   send_to_char ("There is no clan with that number.\r\n", ch);
   return;
   }
   while ((*argument != ' ') && (*argument != '\0'))
   argument++;
   while (*argument == ' ') argument++;
   }
   else
   */
  if ((c = GET_CLAN(ch)) == 0 || GET_CLAN_RANK(ch) == 0)
  {
    send_to_char("You're not part of a clan.\r\n", ch);
    return;
  }
  if (PRF_FLAGGED(ch, PRF_NOCTALK))
  {
    new_send_to_char(ch, "You can now hear you clan again.\r\n");
    REMOVE_BIT_AR(PRF_FLAGS(ch), PRF_NOCTALK);
  }

  skip_spaces(&argument);

  if (!*argument)
  {
    send_to_char("What do you want to tell your clan?\r\n", ch);
    return;
  }

  if (*argument == '#')
  {
  char loc_a[MAX_INPUT_LENGTH];
    argument++;
    argument = one_argument(argument, loc_a);
    minlev = atoi(loc_a);
    if (minlev > clan[c].ranks)
    {
      send_to_char
      ("No one has a clan rank high enough to hear you!\r\n",
       ch);
      return;
    }
    /*while (*argument != ' ')
      argument++;
    while (*argument == ' ')
      argument++;*/
    snprintf(level_string, sizeof(level_string), " (to ranks > %d)", minlev);
  }

  if (ROOM_FLAGGED(ch->in_room, ROOM_SOUNDPROOF))
  {
    new_send_to_char(ch, "The walls seem to absorb your words.\r\n");
    return;
  }
  else if (PRF_FLAGGED(ch, PRF_NOREPEAT))
    new_send_to_char(ch, "%s", CONFIG_OK);
  else
    if (level_string)
      new_send_to_char(ch, "You tell your clan%s, '%s'\r\n", level_string, argument);
    else
      new_send_to_char(ch, "You tell your clan, '%s'\r\n", argument);
  if (!PLR_FLAGGED(ch, PLR_COVENTRY))
  {
    for (i = descriptor_list; i; i = i->next)
    {
      if (STATE(i) == CON_PLAYING && i->character &&
          !ROOM_FLAGGED(i->character->in_room, ROOM_SOUNDPROOF))
      {
        if (i->character->player_specials->saved.clan == c && (!PRF_FLAGGED(i->character, PRF_NOCTALK)) )
        {
          if (i->character->player_specials->saved.clan_rank >=minlev)
          {
            if ((i->character) != ch)
            {
              write_to_output(i, "%s tells your clan%s, '%s'\r\n",
                              PERS(ch, i->character), level_string, argument);
            }
          }
        }
      }
    }
  }

  return;
}

