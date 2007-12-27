/**************************************************************************
 * File: clan.c                       Intended to be used with CircleMUD  *
 * Usage: This is the code for clans                                      *
 * By Mehdi Keddache (Heritsun on Eclipse of Fate eclipse.argy.com 7777)  *
 * CircleMUD is based on DikuMUD, Copyright (C) 1990, 1991.               *
 * CircleMUD (C) 1993, 94 by the Trustees of the Johns Hopkins University *
 **************************************************************************/

#include "conf.h"
#include "sysdep.h"

#include "structs.h"
#include "utils.h"
#include "comm.h"
#include "db.h"
#include "interpreter.h"
#include "spells.h"
#include "handler.h"
#include "clan.h"

int num_of_clans;
struct clan_rec clan[MAX_CLANS];
void tag_argument(char *argument, char *tag);
extern struct descriptor_data *descriptor_list;
void smash_tilde(char *str);
void strip_cr(char *);
void clan_to_store(int i);
int store_to_clan(int i);
char *clan_name(int idnum);
void update_clan_member(char * name, int rank, int clan);
void remove_clan_member(char * name, int clan);
void add_clan_member(char * name, int rank, int clan);
void free_clan_lists();
extern int TEMP_LOAD_CHAR;

struct clan_list_data *clan_list[MAX_CLANS];


char clan_privileges[NUM_CP + 1][20] = {
                                         "setplan", "enroll", "expel", "promote", "demote", "setfees",
                                         "withdraw", "setapplev"
                                       };

void send_clan_format(struct char_data *ch)
{
  int c, r;

  send_to_char("Clan commands available to you:\n\r"
               "   clan who\r\n"
               "   clan status\r\n"
               "   clan list\r\n" "   clan info <clan>\r\n", ch);
  if (GET_LEVEL(ch) >= LVL_CLAN_GOD)
    send_to_char("   clan create     <leader> <clan name>\r\n"
                 "   clan destroy    <clan>\r\n"
                 "   clan enroll     <player> <clan>\r\n"
                 "   clan expel      <player> <clan>\r\n"
                 "   clan promote    <player> <clan>\r\n"
                 "   clan demote     <player> <clan>\r\n"
                 "   clan withdraw   <amount> <clan>\r\n"
                 "   clan deposit    <amount> <clan>\r\n"
                 "   clan set ranks  <rank>   <clan>\r\n"
                 "   clan set appfee <amount> <clan>\r\n"
                 "   clan set dues   <amount> <clan>\r\n"
                 "   clan set applev <level>  <clan>\r\n"
                 "   clan set plan   <clan>\r\n"
                 "   clan set war    <clan> <clan>\r\n"
                 "   clan set peace  <clan> <clan>\r\n"
                 "   clan set privilege  <privilege>   <rank> <clan>\r\n"
                 "   clan set title  <clan number> <rank> <title>\r\n"
                 "   clan set board <board vnum> <clan>\r\n"
                 "   clan set recall <vnum of recall-to room> <clan>\r\n",
                 ch);
  else
  {
    c = find_clan_by_id(GET_CLAN(ch));
    r = GET_CLAN_RANK(ch);
    if (r < 1)
      send_to_char("   clan apply      <clan>\r\n", ch);
    if (c >= 0)
    {
      send_to_char("   clan deposit    <amount>\r\n", ch);
      if (r >= clan[c].privilege[CP_WITHDRAW])
        send_to_char("   clan withdraw   <amount>\r\n", ch);
      if (r >= clan[c].privilege[CP_ENROLL])
        send_to_char("   clan enroll     <player>\r\n", ch);
      if (r >= clan[c].privilege[CP_EXPEL])
        send_to_char("   clan expel      <player>\r\n", ch);
      if (r >= clan[c].privilege[CP_PROMOTE])
        send_to_char("   clan promote    <player>\r\n", ch);
      if (r >= clan[c].privilege[CP_DEMOTE])
        send_to_char("   clan demote     <player>\r\n", ch);
      if (r >= clan[c].privilege[CP_SET_APPLEV])
        send_to_char("   clan set applev <level>\r\n", ch);
      if (r >= clan[c].privilege[CP_SET_FEES])
        send_to_char("   clan set appfee <amount>\r\n"
                     "   clan set dues   <amount>\r\n", ch);
      if (r >= clan[c].privilege[CP_SET_WAR])
        send_to_char("   clan set peace <clan>\r\n"
                     "   clan set war   <clan>\r\n", ch);
      if (r >= clan[c].privilege[CP_SET_PLAN])
        send_to_char("   clan set plan\r\n", ch);
      if (r == clan[c].ranks)
        send_to_char("   clan set ranks  <rank>\r\n"
                     "   clan set title  <rank> <title>\r\n"
                     "   clan set privilege  <privilege> <rank>\r\n",
                     ch);

    }
  }
}

/* clan code */
struct char_data *is_playing(char *vict_name)
      //char *is_playing(char *vict_name)
{
  extern struct descriptor_data *descriptor_list;
  struct descriptor_data *i, *next_i;

  for (i = descriptor_list; i; i = next_i)
  {
    next_i = i->next;
    if (IS_PLAYING(i)
        && !strcmp(i->character->player.name, CAP(vict_name)))
      return i->character;
  }
  return NULL;
}

void do_clan_create(struct char_data *ch, char *arg)
{
  struct char_data *leader = NULL;
  char arg1[MAX_INPUT_LENGTH], arg2[MAX_INPUT_LENGTH];
  int new_id = 0, i;

  if (!*arg)
  {
    send_clan_format(ch);
    return;
  }

  if (GET_LEVEL(ch) < LVL_CLAN_GOD)
  {
    send_to_char("You are not mighty enough to create new clans!\r\n",
                 ch);
    return;
  }

  if (num_of_clans == MAX_CLANS)
  {
    send_to_char("Max clans reached. WOW!\r\n", ch);
    return;
  }

  half_chop(arg, arg1, arg2);

  if (!(leader = get_char_vis(ch, arg1, NULL, FIND_CHAR_WORLD)))
  {
    send_to_char("The leader of the new clan must be present.\r\n",
                 ch);
    return;
  }

  if (strlen(arg2) >= 32)
  {
    send_to_char("Clan name too long! (32 characters max)\r\n", ch);
    return;
  }
  /*
      if(GET_LEVEL(leader)>=LVL_GOD) {
          send_to_char("You cannot set an immortal as the leader of a clan.\r\n",ch);
          return; }
  */
  if (GET_CLAN(leader) != 0 && GET_CLAN_RANK(leader) != 0)
  {
    send_to_char("The leader already belongs to a clan!\r\n", ch);
    return;
  }

  if (find_clan(arg2) != -1)
  {
    send_to_char("That clan name alread exists!\r\n", ch);
    return;
  }

  strncpy(clan[num_of_clans].name, CAP((char *) arg2), 32);
  for (i = 0; i < num_of_clans; i++)
    if (new_id < clan[i].id)
      new_id = clan[i].id;
  clan[num_of_clans].id = new_id + 1;
  clan[num_of_clans].ranks = 2;
  strcpy(clan[num_of_clans].rank_name[0], "Member");
  strcpy(clan[num_of_clans].rank_name[1], "Leader");
  clan[num_of_clans].treasure = 0;
  clan[num_of_clans].members = 1;
  clan[num_of_clans].power = GET_LEVEL(leader);
  clan[num_of_clans].app_fee = 0;
  clan[num_of_clans].dues = 0;
  clan[num_of_clans].app_level = DEFAULT_APP_LVL;
  clan[num_of_clans].recall = NOWHERE;
  clan[num_of_clans].board = NOTHING;
  for (i = 0; i < MAX_CLAN_SPELLS; i++)
    clan[num_of_clans].spells[i] = TYPE_UNDEFINED;
  for (i = 0; i < NUM_CLAN_PRIVILEGE; i++)
    clan[num_of_clans].privilege[i] = clan[num_of_clans].ranks;
  for (i = 0; i < NUM_AT_CLAN_WAR; i++)
    clan[num_of_clans].at_war[i] = 0;
  for (i = 0; i < NUM_CLAN_EQ; i++)
    clan[num_of_clans].clan_eq[i] = NOTHING;
  num_of_clans++;
  save_clans();
  send_to_char("Clan created\r\n", ch);
  GET_CLAN(leader) = clan[num_of_clans - 1].id;
  GET_CLAN_RANK(leader) = clan[num_of_clans - 1].ranks;
  save_char(leader);

  return;
}
void do_clan_recall(struct char_data *ch, char *arg)
{
obj_vnum brd;
  int clan_num = -1;
  char arg1[MAX_INPUT_LENGTH];
  char arg2[MAX_INPUT_LENGTH];

  if (!(*arg))
  {
    send_clan_format(ch);
    return;
  }


    if (GET_LEVEL(ch) < LVL_CLAN_GOD)
    {
      send_to_char("You do not have that clan privilege.\r\n", ch);
      return;
    }
    two_arguments(arg, arg1, arg2);
    if (!arg2 && !*arg2 && (clan_num = find_clan(arg2)) < 0)
    {
      send_to_char("Unknown clan.\r\n", ch);
      return;
    }
  

  if (!(*arg1))
  {
    send_to_char("clan set recall <VNUM> <clan>\r\n", ch);
    return;
  }

  if (!is_number(arg1))
  {
    send_to_char("That isnt a vnum!\r\n", ch);
    return;
  }

  brd = atoi(arg1);

  if (brd == clan[clan_num].recall)
  {
    send_to_char("The clan already has this as its recall point.\r\n", ch);
    return;
  }

  clan[clan_num].recall = brd;
  
  new_send_to_char(ch, "Done.\r\n");

  save_clans();
  return;


}
void do_clan_board(struct char_data *ch, char *arg)
{
obj_vnum brd;
  int clan_num = -1;
  char arg1[MAX_INPUT_LENGTH];
  char arg2[MAX_INPUT_LENGTH];

  if (!(*arg))
  {
    send_clan_format(ch);
    return;
  }


    if (GET_LEVEL(ch) < LVL_CLAN_GOD)
    {
      send_to_char("You do not have that clan privilege.\r\n", ch);
      return;
    }
    two_arguments(arg, arg1, arg2);
    if (!arg2 && !*arg2) {
    if ((clan_num = find_clan(arg2)) < 0)
    {
      send_to_char("Unknown clan.\r\n", ch);
      return;
      }
    }
  

  if (!(*arg1))
  {
    send_to_char("clan set board <VNUM> <clan>\r\n", ch);
    return;
  }

  if (!is_number(arg1))
  {
    send_to_char("That isnt a vnum!\r\n", ch);
    return;
  }

  brd = atoi(arg1);

  if (brd == clan[clan_num].board)
  {
    send_to_char("The clan already has this as its board.\r\n", ch);
    return;
  }

  clan[clan_num].board = brd;
  
  new_send_to_char(ch, "Done.\r\n");

  save_clans();
  return;


}

void do_clan_destroy(struct char_data *ch, char *arg)
{

  int i, j;
  extern int top_of_p_table;
  extern struct player_index_element *player_table;
  struct char_data *victim = NULL, *cbuf = NULL;

  if (!*arg)
  {
    send_clan_format(ch);
    return;
  }

  if ((i = find_clan(arg)) < 0)
  {
    send_to_char("Unknown clan.\r\n", ch);
    return;
  }

  if (GET_LEVEL(ch) < LVL_CLAN_GOD)
  {
    send_to_char("Your not mighty enough to destroy clans!\r\n", ch);
    return;
  }

  for (j = 0; j <= top_of_p_table; j++)
  {
    if ((victim = is_playing((player_table + j)->name)))
    {
      if (GET_CLAN(victim) == clan[i].id)
      {
        GET_CLAN(victim) = 0;
        GET_CLAN_RANK(victim) = 0;
        save_char(victim);
      }
    }
    else
    {
      CREATE(cbuf, struct char_data, 1);
      CREATE(cbuf->player_specials, struct player_special_data, 1);
      clear_char(cbuf);
      if (load_char((player_table + j)->name, cbuf) >= 0) {
      if (GET_CLAN(cbuf) == clan[i].id)
      {
        GET_CLAN(cbuf) = 0;
        GET_CLAN_RANK(cbuf) = 0;
        save_char(cbuf);
      }
      free_char(victim);
      } else
      free(victim);
    }
  }

  memset(&clan[i], sizeof(struct clan_rec), 0);

  for (j = i; j < num_of_clans - 1; j++)
    clan[j] = clan[j + 1];

  num_of_clans--;

  send_to_char("Clan deleted.\r\n", ch);
  save_clans();
  return;
}

void do_clan_enroll(struct char_data *ch, char *arg)
{
  struct char_data *vict = NULL;
  int clan_num, immcom = 0;
  char arg1[MAX_INPUT_LENGTH];
  char arg2[MAX_INPUT_LENGTH];

  if (!(*arg))
  {
    send_clan_format(ch);
    return;
  }

  if (GET_LEVEL(ch) < LVL_GOD)
  {
    if ((clan_num = find_clan_by_id(GET_CLAN(ch))) < 0)
    {
      send_to_char("You don't belong to any clan!\r\n", ch);
      return;
    }
  }
  else
  {
    if (GET_LEVEL(ch) < LVL_CLAN_GOD)
    {
      send_to_char("You do not have clan privileges.\r\n", ch);
      return;
    }
    immcom = 1;
    half_chop(arg, arg1, arg2);
    strcpy(arg, arg1);
    if ((clan_num = find_clan(arg2)) < 0)
    {
      send_to_char("Unknown clan.\r\n", ch);
      return;
    }
  }

  if (GET_CLAN_RANK(ch) < clan[clan_num].privilege[CP_ENROLL] && !immcom)
  {
    send_to_char
    ("You're not influent enough in the clan to do that!\r\n", ch);
    return;
  }

  if (!(vict = get_char_room_vis(ch, arg, NULL)))
  {
    send_to_char("Er, Who ??\r\n", ch);
    return;
  }
  else
  {
    if (GET_CLAN(vict) != clan[clan_num].id)
    {
      if (GET_CLAN_RANK(vict) > 0)
      {
        send_to_char("They're already in a clan.\r\n", ch);
        return;
      }
      else
      {
        send_to_char("They didn't request to join your clan.\r\n",
                     ch);
        return;
      }
    }
    else if (GET_CLAN_RANK(vict) > 0)
    {
      send_to_char("They're already in your clan.\r\n", ch);
      return;
    }
    if (GET_LEVEL(vict) >= LVL_GOD)
    {
      send_to_char("You cannot enroll immortals in clans.\r\n", ch);
      return;
    }
  }

  GET_CLAN_RANK(vict)++;
  save_char(vict);
  clan[clan_num].power += GET_LEVEL(vict);
  clan[clan_num].members++;
  send_to_char("You've been enrolled in the clan you chose!\r\n", vict);
  send_to_char("Done.\r\n", ch);

  return;
}

void do_clan_expel(struct char_data *ch, char *arg)
{
  struct char_data *vict = NULL;
  int clan_num, immcom = 0;
  char arg1[MAX_INPUT_LENGTH];
  char arg2[MAX_INPUT_LENGTH];

  if (!(*arg))
  {
    send_clan_format(ch);
    return;
  }

  if (GET_LEVEL(ch) < LVL_GOD)
  {
    if ((clan_num = find_clan_by_id(GET_CLAN(ch))) < 0)
    {
      send_to_char("You don't belong to any clan!\r\n", ch);
      return;
    }
  }
  else
  {
    if (GET_LEVEL(ch) < LVL_CLAN_GOD)
    {
      send_to_char("You do not have clan privileges.\r\n", ch);
      return;
    }
    immcom = 1;
    half_chop(arg, arg1, arg2);
    strcpy(arg, arg1);
    if ((clan_num = find_clan(arg2)) < 0)
    {
      send_to_char("Unknown clan.\r\n", ch);
      return;
    }
  }

  if (GET_CLAN_RANK(ch) < clan[clan_num].privilege[CP_EXPEL] && !immcom)
  {
    send_to_char
    ("You're not influent enough in the clan to do that!\r\n", ch);
    return;
  }

  if (!(vict = get_char_room_vis(ch, arg, NULL)))
  {
    send_to_char("Er, Who ??\r\n", ch);
    return;
  }
  else
  {
    if (GET_CLAN(vict) != clan[clan_num].id)
    {
      send_to_char("They're not in your clan.\r\n", ch);
      return;
    }
    else
    {
      if (GET_CLAN_RANK(vict) >= GET_CLAN_RANK(ch) && !immcom)
      {
        send_to_char("You cannot kick out that person.\r\n", ch);
        return;
      }
    }
  }

  GET_CLAN(vict) = 0;
  GET_CLAN_RANK(vict) = 0;
  save_char(vict);
  clan[clan_num].members--;
  clan[clan_num].power -= GET_LEVEL(vict);
  send_to_char("You've been kicked out of your clan!\r\n", vict);
  send_to_char("Done.\r\n", ch);

  return;
}

int at_war(int clan, int war_clan)
{

  return 1;
}

void declare_war(int clan, int war_clan)
{}

void do_clan_war(struct char_data *ch, char *arg)
{
  //    struct char_data *vict = NULL;
  int clan_num, immcom = 0, war_num = 0;
  char arg1[MAX_INPUT_LENGTH];
  char arg2[MAX_INPUT_LENGTH];

  if (!(*arg))
  {
    send_clan_format(ch);
    return;
  }

  if (GET_LEVEL(ch) < LVL_GOD)
  {
    if ((clan_num = find_clan_by_id(GET_CLAN(ch))) < 0)
    {
      send_to_char("You don't belong to any clan!\r\n", ch);
      return;
    }
  }
  else
  {
    if (GET_LEVEL(ch) < LVL_CLAN_GOD)
    {
      send_to_char("You do not have clan privileges.\r\n", ch);
      return;
    }
    immcom = 1;
    half_chop(arg, arg1, arg2);
    strcpy(arg, arg1);
    if ((clan_num = find_clan(arg2)) < 0)
    {
      send_to_char("Unknown clan.\r\n", ch);
      return;
    }
  }

  if (GET_CLAN_RANK(ch) < clan[clan_num].privilege[CP_SET_WAR] && !immcom)
  {
    send_to_char
    ("You're not influent enough in the clan to do that!\r\n", ch);
    return;
  }

  war_num = find_clan(arg);
  if (at_war(clan_num, war_num))
  {
    new_send_to_char
    (ch, "You're already AT WAR with %s!\r\n", clan[war_num].name);
    return;
  }

  declare_war(clan_num, war_num);

  send_to_all("{cRThe %s have declared war on the %s!{c0",clan[clan_num].name,clan[war_num].name );
  send_to_char("Done.\r\n", ch);

  return;
}


void do_clan_demote(struct char_data *ch, char *arg)
{
  struct char_data *vict = NULL;
  int clan_num, immcom = 0;
  char arg1[MAX_INPUT_LENGTH];
  char arg2[MAX_INPUT_LENGTH];

  if (!(*arg))
  {
    send_clan_format(ch);
    return;
  }

  if (GET_LEVEL(ch) < LVL_GOD)
  {
    if ((clan_num = find_clan_by_id(GET_CLAN(ch))) < 0)
    {
      send_to_char("You don't belong to any clan!\r\n", ch);
      return;
    }
  }
  else
  {
    if (GET_LEVEL(ch) < LVL_CLAN_GOD)
    {
      send_to_char("You do not have clan privileges.\r\n", ch);
      return;
    }
    immcom = 1;
    half_chop(arg, arg1, arg2);
    strcpy(arg, arg1);
    if ((clan_num = find_clan(arg2)) < 0)
    {
      send_to_char("Unknown clan.\r\n", ch);
      return;
    }
  }

  if (GET_CLAN_RANK(ch) < clan[clan_num].privilege[CP_DEMOTE] && !immcom)
  {
    send_to_char
    ("You're not influent enough in the clan to do that!\r\n", ch);
    return;
  }

  if (!(vict = get_char_room_vis(ch, arg, NULL)))
  {
    send_to_char("Er, Who ??\r\n", ch);
    return;
  }
  else
  {
    if (GET_CLAN(vict) != clan[clan_num].id)
    {
      send_to_char("They're not in your clan.\r\n", ch);
      return;
    }
    else
    {
      if (GET_CLAN_RANK(vict) == 1)
      {
        send_to_char
        ("They can't be demoted any further, use expel now.\r\n",
         ch);
        return;
      }
      if (GET_CLAN_RANK(vict) >= GET_CLAN_RANK(ch) && !immcom)
      {
        send_to_char
        ("You cannot demote a person of this rank!\r\n", ch);
        return;
      }
    }
  }

  GET_CLAN_RANK(vict)--;
  update_clan_member(GET_NAME(vict), GET_CLAN_RANK(vict), clan_num);
  save_char(vict);
  send_to_char("You've demoted within your clan!\r\n", vict);
  send_to_char("Done.\r\n", ch);
  return;
}

void do_clan_promote(struct char_data *ch, char *arg)
{
  struct char_data *vict = NULL;
  int clan_num, immcom = 0;
  char arg1[MAX_INPUT_LENGTH];
  char arg2[MAX_INPUT_LENGTH];

  if (!(*arg))
  {
    send_clan_format(ch);
    return;
  }

  if (GET_LEVEL(ch) < LVL_CLAN_GOD)
  {
    if ((clan_num = find_clan_by_id(GET_CLAN(ch))) < 0)
    {
      send_to_char("You don't belong to any clan!\r\n", ch);
      return;
    }
  }
  else
  {
    immcom = 1;
    half_chop(arg, arg1, arg2);
    strcpy(arg, arg1);
    if ((clan_num = find_clan(arg2)) < 0)
    {
      send_to_char("Unknown clan.\r\n", ch);
      return;
    }
  }

  if (GET_CLAN_RANK(ch) < clan[clan_num].privilege[CP_PROMOTE]
      && !immcom)
  {
    send_to_char
    ("You're not influent enough in the clan to do that!\r\n", ch);
    return;
  }

  if (!(vict = get_char_room_vis(ch, arg, NULL)))
  {
    send_to_char("Er, Who ??\r\n", ch);
    return;
  }
  else
  {
    if (GET_CLAN(vict) != clan[clan_num].id)
    {
      send_to_char("They're not in your clan.\r\n", ch);
      return;
    }
    else
    {
      if (GET_CLAN_RANK(vict) == 0)
      {
        send_to_char("They're not enrolled yet.\r\n", ch);
        return;
      }
      if ((GET_CLAN_RANK(vict) + 1) > GET_CLAN_RANK(ch) && !immcom)
      {
        send_to_char
        ("You cannot promote that person over your rank!\r\n",
         ch);
        return;
      }
      if (GET_CLAN_RANK(vict) == clan[clan_num].ranks)
      {
        send_to_char
        ("You cannot promote someone over the top rank!\r\n",
         ch);
        return;
      }
    }
  }

  GET_CLAN_RANK(vict)++;
  update_clan_member(GET_NAME(vict), GET_CLAN_RANK(vict), clan_num);
  save_char(vict);
  send_to_char("You've been promoted within your clan!\r\n", vict);
  send_to_char("Done.\r\n", ch);
  return;
}

void do_clan_who(struct char_data *ch)
{
  struct descriptor_data *d;
  struct char_data *tch;

  if (GET_CLAN_RANK(ch) == 0)
  {
    send_to_char("You do not belong to a clan!\r\n", ch);
    return;
  }

  send_to_char("\r\nList of your clan members\r\n", ch);
  send_to_char("-------------------------\r\n", ch);
  for (d = descriptor_list; d; d = d->next)
  {
    if (!IS_PLAYING(d))
      continue;
    if ((tch = d->character)) {
    if (CAN_SEE(ch, tch))
      if (GET_CLAN(tch) == GET_CLAN(ch) && GET_CLAN_RANK(tch) > 0 )
        new_send_to_char(ch, "%s\r\n", GET_NAME(tch));
	
	}

  }
  return;
}

void do_clan_status(struct char_data *ch)
{
  int clan_num;



  clan_num = find_clan_by_id(GET_CLAN(ch));

  if (GET_CLAN_RANK(ch) == 0)
  {
    if (clan_num >= 0)
    {
      new_send_to_char(ch, "You applied to %s\r\n",clan[clan_num].name);
      return;
    }
    else
    {
      send_to_char("You do not belong to a clan!\r\n", ch);
      return;
    }
  }

  new_send_to_char(ch,  "You are %s (Rank %d) of %s (ID %d)\r\n",
                   clan[clan_num].rank_name[GET_CLAN_RANK(ch) - 1],
                   GET_CLAN_RANK(ch), clan[clan_num].name, clan[clan_num].id);

  return;
}

void do_clan_apply(struct char_data *ch, char *arg)
{
  int clan_num;

  if (!(*arg))
  {
    send_clan_format(ch);
    return;
  }


  if (GET_CLAN_RANK(ch) > 0)
  {
    send_to_char("You already belong to a clan!\r\n", ch);
    return;
  }
  else
  {
    if ((clan_num = find_clan(arg)) < 0)
    {
      send_to_char("Unknown clan.\r\n", ch);
      return;
    }
  }

  if (GET_LEVEL(ch) < clan[clan_num].app_level)
  {
    send_to_char
    ("You are not mighty enough to apply to this clan.\r\n", ch);
    return;
  }
  if (clan[clan_num].app_fee < 0)
    {
    send_to_char("That clan is far to exclusive for you!!\r\n", ch);
    return;
  }

  if (char_gold(ch, 0, GOLD_ALL) < (clan[clan_num].app_fee))
  {
    send_to_char("You cannot afford the application fee!\r\n", ch);
    return;
  }

  char_gold(ch, -clan[clan_num].app_fee, GOLD_ALL) ;
  clan[clan_num].treasure += clan[clan_num].app_fee;
  save_clans();
  GET_CLAN(ch) = clan[clan_num].id;
  save_char(ch);
  new_send_to_char(ch, "You've applied to the %s!\r\n", clan[clan_num].name);

  return;
}

void do_clan_info(struct char_data *ch, char *arg)
{
  int i = 0, j;

  if (num_of_clans == 0)
  {
    send_to_char("No clans have formed yet.\r\n", ch);
    return;
  }

  if (!(*arg))
  {
    new_send_to_char(ch, "\r\n");
    for (i = 0; i < num_of_clans; i++)
      new_send_to_char(ch,
                       "[%-3d]  %-20s Id: %3d Members: %3d  Power: %5d  Appfee: %lld\r\n",
                       i, clan[i].name, clan[i].id, clan[i].members,
                       clan[i].power, clan[i].app_fee);
    return;
  }
  else if ((i = find_clan(arg)) < 0)
  {
    send_to_char("Unknown clan.\r\n", ch);
    return;
  }

  new_send_to_char(ch,
                   "\r\n   <----------------[%-14s]--------------->\r\n"
                   "   O-----------------------------------------------O\r\n"
                   "   |    Ranks  : %-3d    |    Power   : %-5d       |\r\n"
                   "   |    Members: %-3d    |    Treasure: {cy%-10lld{c0  |\r\n"
                   "   O-----------------------------------------------O\r\n",
                   clan[i].name, clan[i].ranks, clan[i].power,
                   clan[i].members, clan[i].treasure);

  new_send_to_char(ch,
                   "   |         Points          |      Tokens         |\r\n"
                   "   |   Quartz 0  Amethyst 0  |  Brass 0 Bronze 0   |\r\n"
                   "   | Sapphire 0      Ruby 0  | Silver 0   Gold 0   |\r\n"
                   "   O-----------------------------------------------O\r\n");

  new_send_to_char(ch,
                   "   |    Enroll   : %-3d  |    Expel       : %-3d     |\r\n"
                   "   |    Promote  : %-3d  |    Demote      : %-3d     |\r\n"
                   "   |    Setplan  : %-3d  |    Setfees     : %-3d     |\r\n"
                   "   |    Withdraw : %-3d  |    Setapplev   : %-3d     |\r\n"
                   "   O-----------------------------------------------O\r\n",
                   clan[i].privilege[1], clan[i].privilege[2],
                   clan[i].privilege[3], clan[i].privilege[4],
                   clan[i].privilege[0], clan[i].privilege[5],
                   clan[i].privilege[6], clan[i].privilege[7]);

  new_send_to_char(ch,
                   "   |    Application Fee  : {cc%-10lld {c0             |\r\n"
                   "   |    Monthly Dues     : {cc%-10lld{c0              |\r\n"
                   "   |    Application level: {cc%-3d{c0                     |\r\n"
                   "   O-----------------------------------------------O\r\n",
                   clan[i].app_fee, clan[i].dues, clan[i].app_level);
  if ((clan[i].at_war[0] == 0) && (clan[i].at_war[1] == 0)
      && (clan[i].at_war[2] == 0) && (clan[i].at_war[3] == 0))
    send_to_char
    ("   [     This clan is at peace with all others.    ]\r\n",
     ch);
  else
    send_to_char
    ("   [             This clan is at war.              ]\r\n",
     ch);
  send_to_char
  ("   <-------------------[Titles]-------------------->\r\n", ch);

  for (j = 0; j < clan[i].ranks; j++)
  {
    new_send_to_char(ch, "({cg%3d{c0 - {cy%-20s{c0)%s", j + 1,
                     clan[i].rank_name[j], (j % 2 ? "\r\n" : " "));
  };

  return;
}


sh_int find_clan_by_id(int idnum)
{
  int i;
  for (i = 0; i < num_of_clans; i++)
    if (idnum == clan[i].id)
      return i;
  return -1;
}

char *clan_name(int idnum)
{
  if (idnum > num_of_clans || idnum < 0)
    return "none";

  return clan[idnum].name;
}

sh_int find_clan(char *name)
{
  int i;
  for (i = 0; i < num_of_clans; i++)
    if (is_abbrev(name, clan[i].name))
      //if(strcmp(CAP(name), CAP(clan[i].name))==0)
      return i;
  return -1;
}

void init_clan_index(void)
{


  FILE *index_file;
  num_of_clans = 0;
  if ((index_file = fopen(CLAN_INDEX_FILE, "r")) == NULL)
    log("SYSERR: Can't read from '%s' clan index file.", CLAN_INDEX_FILE);
  else
  {
    fscanf(index_file, "%d\n", &num_of_clans);
    fclose(index_file);
  }

}

void save_clan_index(void)
{

  FILE *index_file;

  if (!(index_file = fopen(CLAN_INDEX_FILE, "w")))
  {
    log("SYSERR:  Could not write clan index file");
    ALERT_2;
    return;
  }
  else
  {
    fprintf(index_file, "%d\n", num_of_clans);
    fclose(index_file);
  }
}

void save_clans()
{
  int i;
  for (i = 0; i < num_of_clans; i++)
    clan_to_store(i);

  save_clan_index();
}

void new_init_clans()
{
  int i;
  for (i = 0; i < num_of_clans; i++)
    store_to_clan(i);

}

void clan_to_store(int i)
{
  FILE *fl;
  char outname[40], buf[MAX_STRING_LENGTH];
  int thing = 0;


  snprintf(outname, sizeof(outname), "%s/clan.%d", CLAN_DIR, i);
  if (!(fl = fopen(outname, "w")))
  {
    new_mudlog(NRM, LVL_GOD, TRUE, "SYSERR: Couldn't open clan file %s for write",outname);
    return;
  }


  fprintf(fl, "Name: %s\n", clan[i].name);
  fprintf(fl, "Id  : %d\n", clan[i].id);
  fprintf(fl, "Rnks: %d\n", clan[i].ranks);
  fprintf(fl, "RnkN:\n");
  for (thing = 0; thing < clan[i].ranks; thing++)
  {
    fprintf(fl, "%d: %-.20s\n", thing, clan[i].rank_name[thing]);
  }
  fprintf(fl, "-1: none\n");

  fprintf(fl, "Tres: %lld\n", clan[i].treasure);
  fprintf(fl, "Memb: %d\n", clan[i].members);
  fprintf(fl, "Powr: %d\n", clan[i].power);
  fprintf(fl, "Appf: %lld\n", clan[i].app_fee);
  fprintf(fl, "Dues: %lld\n", clan[i].dues);
  fprintf(fl, "Reca: %d\n", clan[i].recall);
  fprintf(fl, "Bord: %d\n", clan[i].board);
  fprintf(fl, "Spel:\n");
  for (thing = 0; thing < MAX_CLAN_SPELLS; thing++)
    fprintf(fl, "%d %d\n", thing, clan[i].spells[thing]);
  fprintf(fl, "-1 -1\n");
  fprintf(fl, "ALev: %d\n", clan[i].app_level);
  fprintf(fl, "Priv:\n");
  for (thing = 0; thing < NUM_CLAN_PRIVILEGE; thing++)
    fprintf(fl, "%d %d\n", thing, clan[i].privilege[thing]);
  fprintf(fl, "-1 -1\n");

  fprintf(fl, "War :\n");
  for (thing = 0; thing < NUM_AT_CLAN_WAR; thing++)
    fprintf(fl, "%d %d\n", thing, clan[i].at_war[thing]);
  fprintf(fl, "-1 -1\n");
  fprintf(fl, "Eq  :\n");
  for (thing = 0; thing < NUM_CLAN_EQ; thing++)
    fprintf(fl, "%d %d\n", thing, clan[i].clan_eq[thing]);
  fprintf(fl, "-1 -1\n");
  if (clan[i].description && *clan[i].description)
  {
    strcpy(buf, clan[i].description);
    strip_cr(buf);
    smash_tilde(buf);
    fprintf(fl, "Desc:\n%s~\n", buf);
  }

  fclose(fl);
}

int store_to_clan(int i)
{
  int cnt, num = 0, num2 = 0;
  gold_int num6 = 0;
  FILE *fl;
  char filename[40];
  char buffer[128], line[MAX_INPUT_LENGTH + 1], tag[6];
  char buf2[MAX_INPUT_LENGTH];

  sprintf(filename, "%s/clan.%d", CLAN_DIR, i);
  if (!(fl = fopen(filename, "r")))
  {
    new_mudlog(NRM, LVL_GOD, TRUE, "SYSERR: Couldn't open clan file %s for read",filename);
    return -1;
  }


  clan[i].id = 0;
  clan[i].name[0] = 0;
  clan[i].ranks = 0;
  for (cnt =0 ; cnt < 20; cnt++)
    clan[i].rank_name[cnt][0] = 0;
  clan[i].treasure = 0;
  clan[i].members = 0;
  clan[i].power = 0;
  clan[i].app_fee = 0;
  clan[i].dues = 0;
  for (cnt =0 ; cnt < MAX_CLAN_SPELLS; cnt++)
    clan[i].spells[cnt] = TYPE_UNDEFINED;
  clan[i].app_level = LVL_IMPL;
  for (cnt =0 ; cnt < NUM_CLAN_PRIVILEGE; cnt++)
    clan[i].privilege[cnt] = 0;
  for (cnt =0 ; cnt < NUM_AT_CLAN_WAR; cnt++)
    clan[i].at_war[cnt] = 0;
  for (cnt =0 ; cnt < NUM_CLAN_EQ; cnt++)
    clan[i].clan_eq[cnt] = NOTHING;
  clan[i].description[0] = 0;
  clan[i].board = NOTHING;
  clan[i].recall = NOWHERE;

  while (get_line(fl, line))
  {
    tag_argument(line, tag);
    num = atoi(line);
    num6 = atoll(line);


    switch (*tag)
    {
    case 'A':
      if (!strcmp(tag, "Appf"))
        clan[i].app_fee = num6;
      else if (!strcmp(tag, "ALev"))
        clan[i].app_level = num;
      break;

    case 'B':
      if (!strcmp(tag, "Bord"))
        clan[i].board = num;
      break;

    case 'D':
      if (!strcmp(tag, "Desc"))
      {
        strcpy(clan[i].description, fread_string(fl, buf2));
      }
      else if (!strcmp(tag, "Dues"))
        clan[i].dues = num;
      break;
    case 'E':
      if (!strcmp(tag, "Eq  "))
      {
        do
        {
          get_line(fl, line);
          sscanf(line, "%d %d", &num, &num2);
          if (num != -1)
          {
            clan[i].clan_eq[num] =(num2);
          }
        }
        while (num != -1);
      }
      break;
    case 'I':
      if (!strcmp(tag, "Id  "))
        clan[i].id = num;
      break;

    case 'M':
      if (!strcmp(tag, "Memb"))
      {
        clan[i].members = num;
        break;
      case 'N':
        if (!strcmp(tag, "Name"))
          strcpy(clan[i].name, line);
        break;

      case 'P':
        if (!strcmp(tag, "Powr"))
          clan[i].power = num;
        else if (!strcmp(tag, "Priv"))
        {
          do
          {
            get_line(fl, line);
            sscanf(line, "%d %d", &num, &num2);
            if (num != -1)
            {
              clan[i].privilege[num] = num2;
            }
          }
          while (num != -1);
        }

        break;

      case 'R':


        if (!strcmp(tag, "Rnks"))
          clan[i].ranks = num;
        else if (!strcmp(tag, "Reca"))
          clan[i].recall = num;
        else if (!strcmp(tag, "RnkN"))
        {
          do
          {
            get_line(fl, line);
            sscanf(line, "%d: %20[^\f\n\r\t\v]", &num, (char *) &buffer);
            if (num != -1)
            {
              strcpy(clan[i].rank_name[num],  buffer);
            }
          }
          while (num != -1);
        }
        break;

      case 'S':
        if (!strcmp(tag, "Spel"))
        {
          do
          {
            get_line(fl, line);
            sscanf(line, "%d %d", &num, &num2);
            if (num != -1)
            {
              clan[i].spells[num]=  (num2);
            }
          }
          while (num != -1);
        }
        break;

      case 'T':
        if (!strcmp(tag, "Tres"))
          clan[i].treasure = num6;
        break;

      case 'W':
        if (!strcmp(tag, "War "))
        {
          do
          {
            get_line(fl, line);
            sscanf(line, "%d %d", &num, &num2);
            if (num != -1)
            {
              clan[i].at_war[num] =(num2);
            }
          }
          while (num != -1);
        }
        break;

      default:
        sprintf(buffer, "SYSERR: Unknown tag %s in clan %d", tag, i);
      }
    }
  }

  fclose(fl);
  return 1;
}



void init_clans()
{
  FILE *fl;
  int i, j;
  extern int top_of_p_table;
  extern struct player_index_element *player_table;

  init_clan_index();

  memset(clan, 0, sizeof(struct clan_rec) * MAX_CLANS);
  if (num_of_clans == 0)
  {
    i = 0;

    if (!(fl = fopen(CLAN_FILE, "rb")))
    {
      log("   Clan file does not exist. Will create a new one");
      save_clans();
      return;
    }

    fread(&num_of_clans, sizeof(int), 1, fl);
    fread(clan, sizeof(struct clan_rec), num_of_clans, fl);
    fclose(fl);
  }
  else
  {
    new_init_clans();
  }
  save_clans();

  log("   Calculating powers and members");
  for (i = 0; i < num_of_clans; i++)
  {
    clan[i].power = 0;
    clan[i].members = 0;
    clan_list[i] = NULL;
  }

  for (j = 0; j <= top_of_p_table; j++)
  {
#if 0
    {
      struct char_data *victim;
      CREATE(victim, struct char_data, 1);
      clear_char(victim);
      TEMP_LOAD_CHAR = TRUE;

      if (store_to_char((player_table + j)->name, victim) > -1)
      {
        if ((i = find_clan_by_id(GET_CLAN(victim))) >= 0)
        {
          add_clan_member(GET_NAME(victim), GET_CLAN_RANK(victim), i);
          clan[i].power += GET_LEVEL(victim);
          clan[i].members++;
        }
        free_char(victim);

      }
      else
        free(victim);

      TEMP_LOAD_CHAR = FALSE;
    }
#else
    if (player_table[j].name &&
        *player_table[j].name &&
        (i = find_clan_by_id(player_table[j].clan)) >= 0)
    {
      add_clan_member(player_table[j].name, player_table[j].rank, i);
      clan[i].power += player_table[j].level;
      clan[i].members++;
    }

#endif

  }
  return;
}

/*
void do_clan_list(struct char_data *ch, char *arg)
{
  send_to_char("This command has been temporarily disabled.\r\n", ch);
  return;
}
 
 
 
*/

void do_clan_list(struct char_data *ch, char *arg)
{
  int i;
  char buf[MAX_STRING_LENGTH];
  struct clan_list_data *temp;

  DYN_DEFINE;
  *buf = '\0';
  DYN_CREATE;
  *dynbuf = 0;

  if (*arg && GET_LEVEL(ch) >= LVL_SEN)
    i = find_clan(arg);
  else
    i = find_clan_by_id(GET_CLAN(ch));

  new_send_to_char(ch, "Members of the %s clan\r\n", clan_name(i));
  send_to_char("-------------------------------\r\n", ch);


  if (i==NOTHING || i > (MAX_CLANS-1 ))
  {
    new_send_to_char(ch, "Error.\r\n");
    return;
  }

  temp = clan_list[i];
  while (temp)
  {
    snprintf(buf, sizeof(buf), "%-20s -- Rank: %d.\r\n",
             temp->name,
             temp->rank);
    DYN_RESIZE(buf);

    temp = temp->next;
  }



  page_string(ch->desc, dynbuf, DYN_BUFFER);
  return;

}

void do_clan_bank(struct char_data *ch, char *arg, int action)
{
  int clan_num, immcom = 0;
  gold_int amount = 0;
  char arg1[MAX_INPUT_LENGTH];
  char arg2[MAX_INPUT_LENGTH];

  if (!(*arg))
  {
    send_clan_format(ch);
    return;
  }

  if (GET_LEVEL(ch) < LVL_GOD)
  {
    if ((clan_num = find_clan_by_id(GET_CLAN(ch))) < 0)
    {
      send_to_char("You don't belong to any clan!\r\n", ch);
      return;
    }
  }
  else
  {
    if (GET_LEVEL(ch) < LVL_CLAN_GOD)
    {
      send_to_char("You do not have clan privileges.\r\n", ch);
      return;
    }
    immcom = 1;
    half_chop(arg, arg1, arg2);
    strcpy(arg, arg1);
    if ((clan_num = find_clan(arg2)) < 0)
    {
      send_to_char("Unknown clan.\r\n", ch);
      return;
    }
  }

  if ((IS_HERO(ch)) ||( GET_CLAN_RANK(ch) < clan[clan_num].privilege[CP_WITHDRAW]
                        && !immcom && action == CB_WITHDRAW) )
  {
    send_to_char
    ("You're not influent enough in the clan to do that!\r\n", ch);
    return;
  }

  if (!(*arg))
  {
    send_to_char("Deposit how much?\r\n", ch);
    return;
  }

  if (!is_number(arg))
  {
    send_to_char("Deposit what?\r\n", ch);
    return;
  }

  amount = atoll(arg);
  if (amount < 0)
  {
    new_send_to_char(ch, "Don't be silly.\r\n");
    return;
  }

  if (!immcom && action == CB_DEPOSIT && char_gold(ch, 0, GOLD_ALL) < amount)
  {
    send_to_char("You do not have that kind of money!\r\n", ch);
    return;
  }
/*
  if ((((int)(clan[clan_num].treasure + amount)) > 2000000000 || ((int)(clan[clan_num].treasure + amount)) < 0)&& action == CB_DEPOSIT)
  {
    send_to_char("Clan can't handle that much!\r\n", ch);
    return;
  }
*/

  if (action == CB_WITHDRAW && clan[clan_num].treasure < amount)
  {
    send_to_char("The clan is not wealthy enough for your needs!\r\n",
                 ch);
    return;
  }
  if (clan[clan_num].treasure < 0)
  {
    send_to_char("The clan's account has been frozen because of abuse!!\r\n",
                 ch);
    return;
  }

  if (action == CB_WITHDRAW && (clan[clan_num].treasure - amount) < 0)
  {
    send_to_char("That transaction can't take place!!\r\n", ch);
    return;
  }

  switch (action)
  {
  case CB_WITHDRAW:
    char_gold(ch, amount, GOLD_HAND);
    clan[clan_num].treasure -= amount;
    send_to_char("You withdraw from the clan's treasure.\r\n", ch);
    break;
  case CB_DEPOSIT:
    if (!immcom)
      char_gold(ch, -amount, GOLD_ALL);
    clan[clan_num].treasure += amount;
    send_to_char("You add to the clan's treasure.\r\n", ch);
    break;
  default:
    send_to_char("Problem in command, please report.\r\n", ch);
    break;
  }
  save_char(ch);
  save_clans();
  return;
}

void do_clan_money(struct char_data *ch, char *arg, int action)
{
  int clan_num, immcom = 0;
  long amount = 0;
  char arg1[MAX_INPUT_LENGTH];
  char arg2[MAX_INPUT_LENGTH];

  if (!(*arg))
  {
    send_clan_format(ch);
    return;
  }

  if (GET_LEVEL(ch) < LVL_GOD)
  {
    if ((clan_num = find_clan_by_id(GET_CLAN(ch))) < 0)
    {
      send_to_char("You don't belong to any clan!\r\n", ch);
      return;
    }
  }
  else
  {
    if (GET_LEVEL(ch) < LVL_CLAN_GOD)
    {
      send_to_char("You do not have clan privileges.\r\n", ch);
      return;
    }
    immcom = 1;
    half_chop(arg, arg1, arg2);
    strcpy(arg, arg1);
    if ((clan_num = find_clan(arg2)) < 0)
    {
      send_to_char("Unknown clan.\r\n", ch);
      return;
    }
  }

  if (GET_CLAN_RANK(ch) < clan[clan_num].privilege[CP_SET_FEES]
      && !immcom)
  {
    send_to_char
    ("You're not influent enough in the clan to do that!\r\n", ch);
    return;
  }

  if (!(*arg))
  {
    send_to_char("Set it to how much?\r\n", ch);
    return;
  }

  if (!is_number(arg))
  {
    send_to_char("Set it to what?\r\n", ch);
    return;
  }

  amount = atoll(arg);

  switch (action)
  {
  case CM_APPFEE:
    clan[clan_num].app_fee = amount;
    send_to_char("You change the application fee.\r\n", ch);
    break;
  case CM_DUES:
    clan[clan_num].dues = amount;
    send_to_char("You change the monthly dues.\r\n", ch);
    break;
  default:
    send_to_char("Problem in command, please report.\r\n", ch);
    break;
  }

  save_clans();
  return;
}

void do_clan_ranks(struct char_data *ch, char *arg)
{
  int i, j;
  int clan_num, immcom = 0;
  int new_ranks;
  char arg1[MAX_INPUT_LENGTH];
  char arg2[MAX_INPUT_LENGTH];
  extern int top_of_p_table;
  extern struct player_index_element *player_table;
  struct char_data *victim = NULL;

  if (!(*arg))
  {
    send_clan_format(ch);
    return;
  }

  if (GET_LEVEL(ch) < LVL_GOD)
  {
    if ((clan_num = find_clan_by_id(GET_CLAN(ch))) < 0)
    {
      send_to_char("You don't belong to any clan!\r\n", ch);
      return;
    }
  }
  else
  {
    if (GET_LEVEL(ch) < LVL_CLAN_GOD)
    {
      send_to_char("You do not have clan privileges.\r\n", ch);
      return;
    }
    immcom = 1;
    half_chop(arg, arg1, arg2);
    strcpy(arg, arg1);
    if ((clan_num = find_clan(arg2)) < 0)
    {
      send_to_char("Unknown clan.\r\n", ch);
      return;
    }
  }

  if (GET_CLAN_RANK(ch) != clan[clan_num].ranks && !immcom)
  {
    send_to_char
    ("You're not influent enough in the clan to do that!\r\n", ch);
    return;
  }

  if (!(*arg))
  {
    send_to_char("Set how many ranks?\r\n", ch);
    return;
  }

  if (!is_number(arg))
  {
    send_to_char("Set the ranks to what?\r\n", ch);
    return;
  }

  new_ranks = atoi(arg);

  if (new_ranks == clan[clan_num].ranks)
  {
    send_to_char("The clan already has this number of ranks.\r\n", ch);
    return;
  }

  if (new_ranks < 2 || new_ranks > 20)
  {
    send_to_char("Clans must have from 2 to 20 ranks.\r\n", ch);
    return;
  }

  if (char_gold(ch, 0, GOLD_ALL) < 7500000 && !immcom)
  {
    send_to_char
    ("Changing the clan hierarchy requires 7,500,000 coins!\r\n",
     ch);
    return;
  }

  if (!immcom)
    char_gold(ch, -7500000, GOLD_ALL);

  for (j = 0; j <= top_of_p_table; j++)
  {
    if ((victim = is_playing((player_table + j)->name)))
    {
      if (GET_CLAN(victim) == clan[clan_num].id)
      {
        if (GET_CLAN_RANK(victim) < clan[clan_num].ranks
            && GET_CLAN_RANK(victim) > 0)
          GET_CLAN_RANK(victim) = 1;
        if (GET_CLAN_RANK(victim) == clan[clan_num].ranks)
          GET_CLAN_RANK(victim) = new_ranks;
        save_char(victim);
      }
    }
    else
    {
      CREATE(victim, struct char_data, 1);      
      CREATE(victim->player_specials, struct player_special_data, 1);
      clear_char(victim);
      if (load_char((player_table + j)->name, victim) >= 0) {
      if (GET_CLAN(victim) == clan[clan_num].id)
      {
        if (GET_CLAN_RANK(victim) < clan[clan_num].ranks
            && GET_CLAN_RANK(victim) > 0)
          GET_CLAN_RANK(victim) = 1;
        if (GET_CLAN_RANK(victim) == clan[clan_num].ranks)
          GET_CLAN_RANK(victim) = new_ranks;
        save_char(victim);
      }
      free_char(victim);
      } else
      free(victim);
    }
  }

  clan[clan_num].ranks = new_ranks;
  for (i = 0; i < clan[clan_num].ranks - 1; i++)
    strcpy(clan[clan_num].rank_name[i], "Member");
  strcpy(clan[clan_num].rank_name[clan[clan_num].ranks - 1], "Leader");
  for (i = 0; i < NUM_CP; i++)
    clan[clan_num].privilege[i] = new_ranks;

  save_clans();
  return;
}

void do_clan_titles(struct char_data *ch, char *arg)
{
  char arg1[MAX_INPUT_LENGTH], arg2[MAX_INPUT_LENGTH];
  int clan_num = 0, rank;

  if (!(*arg))
  {
    send_clan_format(ch);
    return;
  }

  if (GET_LEVEL(ch) < LVL_GOD)
  {
    if ((clan_num = find_clan_by_id(GET_CLAN(ch))) < 0)
    {
      send_to_char("You don't belong to any clan!\r\n", ch);
      return;
    }
    if (GET_CLAN_RANK(ch) != clan[clan_num].ranks)
    {
      send_to_char
      ("You're not influent enough in the clan to do that!\r\n",
       ch);
      return;
    }
  }
  else
  {
    if (GET_LEVEL(ch) < LVL_CLAN_GOD)
    {
      send_to_char("You do not have clan privileges.\r\n", ch);
      return;
    }
    half_chop(arg, arg1, arg2);
    strcpy(arg, arg2);
    if (!is_number(arg1))
    {
      send_to_char("You need to specify a clan number.\r\n", ch);
      return;
    }
    if ((clan_num = atoi(arg1)) < 0 || clan_num >= num_of_clans)
    {
      send_to_char("There is no clan with that number.\r\n", ch);
      return;
    }
  }

  half_chop(arg, arg1, arg2);

  if (!is_number(arg1))
  {
    send_to_char("You need to specify a rank number.\r\n", ch);
    return;
  }

  rank = atoi(arg1);

  if (rank < 1 || rank > clan[clan_num].ranks)
  {
    send_to_char("This clan has no such rank number.\r\n", ch);
    return;
  }

  if (strlen(arg2) < 1 || strlen(arg2) > 19)
  {
    send_to_char("You need a clan title of under 20 characters.\r\n",
                 ch);
    return;
  }

  strcpy(clan[clan_num].rank_name[rank - 1], arg2);
  save_clans();
  send_to_char("Done.\r\n", ch);
  return;
}

void do_clan_application(struct char_data *ch, char *arg)
{
  int clan_num, immcom = 0;
  int applevel;
  char arg1[MAX_INPUT_LENGTH];
  char arg2[MAX_INPUT_LENGTH];

  if (!(*arg))
  {
    send_clan_format(ch);
    return;
  }

  if (GET_LEVEL(ch) < LVL_GOD)
  {
    if ((clan_num = find_clan_by_id(GET_CLAN(ch))) < 0)
    {
      send_to_char("You don't belong to any clan!\r\n", ch);
      return;
    }
  }
  else
  {
    if (GET_LEVEL(ch) < LVL_CLAN_GOD)
    {
      send_to_char("You do not have clan privileges.\r\n", ch);
      return;
    }
    immcom = 1;
    half_chop(arg, arg1, arg2);
    strcpy(arg, arg1);
    if ((clan_num = find_clan(arg2)) < 0)
    {
      send_to_char("Unknown clan.\r\n", ch);
      return;
    }
  }

  if (GET_CLAN_RANK(ch) < clan[clan_num].privilege[CP_SET_APPLEV]
      && !immcom)
  {
    send_to_char
    ("You're not influent enough in the clan to do that!\r\n", ch);
    return;
  }

  if (!(*arg))
  {
    send_to_char("Set to which level?\r\n", ch);
    return;
  }

  if (!is_number(arg))
  {
    send_to_char("Set the application level to what?\r\n", ch);
    return;
  }

  applevel = atoi(arg);

  if (applevel < 1 || applevel > 999)
  {
    send_to_char("The application level can go from 1 to 999.\r\n",
                 ch);
    return;
  }

  clan[clan_num].app_level = applevel;
  save_clans();

  return;
}

void do_clan_sp(struct char_data *ch, char *arg, int priv)
{
  int clan_num, immcom = 0;
  int rank;
  char arg1[MAX_INPUT_LENGTH];
  char arg2[MAX_INPUT_LENGTH];

  if (!(*arg))
  {
    send_clan_format(ch);
    return;
  }


  if (GET_LEVEL(ch) < LVL_GOD)
  {
    if ((clan_num = find_clan_by_id(GET_CLAN(ch))) < 0)
    {
      send_to_char("You don't belong to any clan!\r\n", ch);
      return;
    }
  }
  else
  {
    if (GET_LEVEL(ch) < LVL_CLAN_GOD)
    {
      send_to_char("You do not have clan privileges.\r\n", ch);
      return;
    }
    immcom = 1;
    half_chop(arg, arg1, arg2);
    strcpy(arg, arg1);
    if ((clan_num = find_clan(arg1)) < 0)
    {
      send_to_char("Unknown clan.\r\n", ch);
      return;
    }
  }

  if (GET_CLAN_RANK(ch) != clan[clan_num].ranks && !immcom)
  {
    send_to_char
    ("You're not influent enough in the clan to do that!\r\n", ch);
    return;
  }

  if (!(*arg))
  {
    send_to_char("Set the privilege to which rank?\r\n", ch);
    return;
  }

  if (!is_number(arg))
  {
    send_to_char("Set the privilege to what?\r\n", ch);
    return;
  }

  rank = atoi(arg);

  if (rank < 1 || rank > clan[clan_num].ranks)
  {
    send_to_char("There is no such rank in the clan.\r\n", ch);
    return;
  }

  clan[clan_num].privilege[priv] = rank;
  save_clans();

  return;
}

void do_clan_plan(struct char_data *ch, char *arg)
{
  int clan_num;

  send_to_char("Command not ready yet\r\n", ch);
  return;

  if (GET_LEVEL(ch) < LVL_GOD)
  {
    if ((clan_num = find_clan_by_id(GET_CLAN(ch))) < 0)
    {
      send_to_char("You don't belong to any clan!\r\n", ch);
      return;
    }
    if (GET_CLAN_RANK(ch) < clan[clan_num].privilege[CP_SET_PLAN])
    {
      send_to_char
      ("You're not influent enough in the clan to do that!\r\n",
       ch);
      return;
    }
  }
  else
  {
    if (GET_LEVEL(ch) < LVL_CLAN_GOD)
    {
      send_to_char("You do not have clan privileges.\r\n", ch);
      return;
    }
    if (!(*arg))
    {
      send_clan_format(ch);
      return;
    }
    if ((clan_num = find_clan(arg)) < 0)
    {
      send_to_char("Unknown clan.\r\n", ch);
      return;
    }
  }

  if (strlen(clan[clan_num].description) == 0)
  {
    new_send_to_char(ch,
                     "Enter the description, or plan for clan <<%s>>.\r\n",
                     clan[clan_num].name);
  }
  else
  {
    new_send_to_char(ch, "Old plan for clan <<%s>>:\r\n",
                     clan[clan_num].name);
    send_to_char(clan[clan_num].description, ch);
    send_to_char("Enter new plan:\r\n", ch);
  }
  send_to_char("End with @ on a line by itself.\r\n", ch);
  ch->desc->str = (char **) clan[clan_num].description;
  ch->desc->max_str = CLAN_PLAN_LENGTH;
  save_clans();
  return;
}

void do_clan_privilege(struct char_data *ch, char *arg)
{
  char arg1[MAX_INPUT_LENGTH], arg2[MAX_INPUT_LENGTH];
  int i;

  half_chop(arg, arg1, arg2);

  if (is_abbrev(arg1, "setplan"))
  {
    do_clan_sp(ch, arg2, CP_SET_PLAN);
    return;
  }
  if (is_abbrev(arg1, "enroll"))
  {
    do_clan_sp(ch, arg2, CP_ENROLL);
    return;
  }
  if (is_abbrev(arg1, "expel"))
  {
    do_clan_sp(ch, arg2, CP_EXPEL);
    return;
  }
  if (is_abbrev(arg1, "promote"))
  {
    do_clan_sp(ch, arg2, CP_PROMOTE);
    return;
  }
  if (is_abbrev(arg1, "demote"))
  {
    do_clan_sp(ch, arg2, CP_DEMOTE);
    return;
  }
  if (is_abbrev(arg1, "withdraw"))
  {
    do_clan_sp(ch, arg2, CP_WITHDRAW);
    return;
  }
  if (is_abbrev(arg1, "setfees"))
  {
    do_clan_sp(ch, arg2, CP_SET_FEES);
    return;
  }
  if (is_abbrev(arg1, "setapplev"))
  {
    do_clan_sp(ch, arg2, CP_SET_APPLEV);
    return;
  }
  send_to_char("\r\nClan privileges:\r\n", ch);
  for (i = 0; i < NUM_CP; i++)
    new_send_to_char(ch, "\t%s\r\n", clan_privileges[i]);

}

void do_clan_set(struct char_data *ch, char *arg)
{
  char arg1[MAX_INPUT_LENGTH], arg2[MAX_INPUT_LENGTH];

  half_chop(arg, arg1, arg2);

  if (is_abbrev(arg1, "plan"))
  {
    do_clan_plan(ch, arg2);
    return;
  }
  if (is_abbrev(arg1, "ranks"))
  {
    do_clan_ranks(ch, arg2);
    return;
  }
  if (is_abbrev(arg1, "title"))
  {
    do_clan_titles(ch, arg2);
    return;
  }
  if (is_abbrev(arg1, "privilege"))
  {
    do_clan_privilege(ch, arg2);
    return;
  }
  if (is_abbrev(arg1, "dues"))
  {
    do_clan_money(ch, arg2, CM_DUES);
    return;
  }
  if (is_abbrev(arg1, "appfee"))
  {
    do_clan_money(ch, arg2, CM_APPFEE);
    return;
  }
  if (is_abbrev(arg1, "applev"))
  {
    do_clan_application(ch, arg2);
    return;
  }
    if (is_abbrev(arg1, "recall"))
  {
    do_clan_recall(ch, arg2);
    return;
  }
    if (is_abbrev(arg1, "board"))
  {
    do_clan_board(ch, arg2);
    return;
  }
  
  send_clan_format(ch);
}

ACMD(do_clan)
{
  char arg1[MAX_INPUT_LENGTH], arg2[MAX_INPUT_LENGTH];

  half_chop(argument, arg1, arg2);

  if (is_abbrev(arg1, "create"))
  {
    do_clan_create(ch, arg2);
    return;
  }
  if (is_abbrev(arg1, "destroy"))
  {
    do_clan_destroy(ch, arg2);
    return;
  }
  if (is_abbrev(arg1, "enroll"))
  {
    do_clan_enroll(ch, arg2);
    return;
  }
  if (is_abbrev(arg1, "expel"))
  {
    do_clan_expel(ch, arg2);
    return;
  }
  if (is_abbrev(arg1, "who"))
  {
    do_clan_who(ch);
    return;
  }
  if (is_abbrev(arg1, "status"))
  {
    do_clan_status(ch);
    return;
  }
  if (is_abbrev(arg1, "info"))
  {
    do_clan_info(ch, arg2);
    return;
  }
  if (is_abbrev(arg1, "apply"))
  {
    do_clan_apply(ch, arg2);
    return;
  }
  if (is_abbrev(arg1, "demote"))
  {
    do_clan_demote(ch, arg2);
    return;
  }
  if (is_abbrev(arg1, "promote"))
  {
    do_clan_promote(ch, arg2);
    return;
  }
  if (is_abbrev(arg1, "set"))
  {
    do_clan_set(ch, arg2);
    return;
  }
  if (is_abbrev(arg1, "withdraw"))
  {
    do_clan_bank(ch, arg2, CB_WITHDRAW);
    return;
  }
  if (is_abbrev(arg1, "deposit"))
  {
    do_clan_bank(ch, arg2, CB_DEPOSIT);
    return;
  }
  if (is_abbrev(arg1, "list"))
  {
    do_clan_list(ch, arg2);
    return;
  }
  send_clan_format(ch);
}

void add_clan_member(char * name, int rank, int clan)
{
  struct clan_list_data *temp;

  if ( (name == NULL) || !(*name))
    return;

  CREATE(temp, struct clan_list_data, 1);
  temp->name = strdup(name);
  temp->rank = rank;
  temp->next = clan_list[clan];
  clan_list[clan] = temp;
}

void remove_clan_member(char * name, int clan)
{
  struct clan_list_data *temp, *find;
  if (clan_list[clan] == NULL || !name || !*name)
    return;
  find = clan_list[clan];
  while (find)
  {
    if (!strcmp(find->name, name))
    {
      REMOVE_FROM_LIST(find, clan_list[clan], next);
      free_string(&find->name);
      free(find);
      return;
    }
    find = find->next;
  }

}

void update_clan_member(char * name, int rank, int clan)
{
  struct clan_list_data  *find;
  if (clan_list[clan] == NULL || !name || !*name)
    return;
  find = clan_list[clan];
  while (find)
  {
    if (!strcmp(find->name, name))
    {
      find->rank = rank;
      return;
    }
    find = find->next;
  }

}

void free_clan_list(struct clan_list_data *find)
{

  if (!find)
    return;
  if (find->next != NULL)
    free_clan_list(find->next);

  free_string(&find->name);
  free(find);
}

void free_clan_lists()
{
  int i;
  for (i = 0; i < num_of_clans; i++)
    free_clan_list(clan_list[i]);

}



