/* ************************************************************************
 *   File: arena.c                                   Addition to CircleMUD *
 *  Usage: Implementation of a event driven arena where players pay to kill*
 *                                                                         *
 *  Writen by:  Kevin Hoogheem aka Goon                                    *
 *              Modified by Billy H. Chan (STROM)                          *
 *                                                                         *
 * Using this code without consent by Goon will make your guts spill       *
 * out or worse.. Maybe I will hire Lauraina Bobbet to come visit you      *
 ************************************************************************ */

#include "conf.h"
#include "sysdep.h"

#include "structs.h"
#include "utils.h"
#include "comm.h"
#include "screen.h"
#include "spells.h"
#include "handler.h"
#include "interpreter.h"
#include "db.h"
#include "arena.h"
#include "fight.h"

/*  external vars  */
extern int restrict;

int in_arena = ARENA_OFF;
int start_time;
int game_length;
int lo_lim;
int hi_lim;
int cost_per_lev;
int time_to_start;
int time_left_in_game;
long arena_pot;
long bet_pot;

struct hall_of_fame_element *fame_list = NULL;



ACMD(do_bet)
{
  gold_int newbet;
  Character *bet_on;
  char arg[MAX_INPUT_LENGTH];
  char buf1[MAX_INPUT_LENGTH];

  two_arguments(argument, arg, buf1);

  if (IS_NPC(ch))
  {
    send_to_char("Mobs cant bet on the arena.\r\n", ch);
    return;
  }

  if (!*arg)
  {
    if (in_arena == ARENA_OFF)
    {
      send_to_char("Sorry no arena is in going on.\r\n", ch);
      return;
    }
    else if (in_arena == ARENA_START)
    {
      send_to_char("Usage: wager <player> <amt>\r\n", ch);
      return;
    }
    else if (in_arena == ARENA_RUNNING)
    {
      send_to_char
      ("Fighting has already begun - no more wagers.\r\n", ch);
      return;
    }
  }

  if (in_arena == ARENA_OFF)
  {
    send_to_char
    ("The arena is closed - wait until it opens to bet.\r\n", ch);
  }
  else if (in_arena == ARENA_RUNNING)
  {
    send_to_char("Arena is in session, no more bets.\r\n", ch);
  }
  else if (!(bet_on = get_char_vis(ch, arg, NULL, FIND_CHAR_WORLD)))
    ch->Send( "%s", CONFIG_NOPERSON);
  else if (!(bet_on->in_room->zone == ARENA_ZONE &&
             ROOM_FLAGGED(IN_ROOM(bet_on), ROOM_ARENA)))
    send_to_char("Sorry that person is not in the arena.\r\n", ch);
  else
  {
    if (GET_AMT_BET(ch) > 0)
    {
      send_to_char("Sorry you have already bet.\r\n", ch);
      return;
    }
    GET_BETTED_ON(ch) = GET_IDNUM(bet_on);
    newbet = abs(atoi(buf1));

    if (newbet == 0)
    {
      send_to_char("Bet some gold why dont you!\r\n", ch);
      return;
    }
    if (newbet > char_gold(ch, 0, GOLD_ALL))
    {
      send_to_char("You don't have that much money!\r\n", ch);
      return;
    }
    if (newbet > MAX_BET * 10)
    {
      send_to_char("Sorry the house will not accept that much.\r\n",
                   ch);
      return;
    }

    char_gold(ch, -newbet, GOLD_ALL);   /* substract the gold */
    arena_pot += (newbet / 2);
    bet_pot += (newbet / 2);
    GET_AMT_BET(ch) = newbet;
    ch->Send( "You place %lld coins on %s.\r\n", newbet,
                     GET_NAME(bet_on));
    send_to_arena("## %s has placed %lld coins on %s.", GET_NAME(ch),
                  newbet, GET_NAME(bet_on));
  }
}

// x12Il.dj

ACMD(do_arena)
{

  if (IS_NPC(ch))
  {
    send_to_char("Mobs cant play in the arena.\r\n", ch);
    return;
  }
  if (in_arena == ARENA_OFF)
  {
    send_to_char("The killing fields are closed right now.\r\n", ch);
  }
  else if (GET_LEVEL(ch) < lo_lim)
  {
    ch->Send(
                     "You must be at least level %d to enter this arena.\r\n",
                     lo_lim);
  }
  else if (PLR_FLAGGED(ch, PLR_KILLER) || PLR_FLAGGED(ch, PLR_THIEF))
  {
    send_to_char("Wanted criminals can not play in the arena.\r\n",
                 ch);
  }
  else if (GET_LEVEL(ch) > hi_lim)
  {
    send_to_char("Sorry the killing fields are not open to you.\r\n",
                 ch);
  }
  else if (char_gold(ch, 0, GOLD_ALL) < (unsigned) (cost_per_lev * GET_LEVEL(ch)))
  {
    ch->Send( "Sorry but you need %d coins to enter the arena.\r\n",
                     (cost_per_lev * GET_LEVEL(ch)));
  }
  else if (in_arena == ARENA_RUNNING)
  {
    send_to_char("The fighting has already begun.\r\n", ch);
  }
  else if (ROOM_FLAGGED(IN_ROOM(ch), ROOM_ARENA))
  {
    send_to_char("You are already in the arena.\r\n", ch);
  }
  else
  {
    act("$n has been taken to the killing fields.", FALSE, ch, 0, 0,  TO_ROOM);
    if (!move_char_to(ch,real_room(number(ARENA_PREP_START, ARENA_PREP_END)))) return;
    act("$n is dropped from the sky.", FALSE, ch, 0, 0, TO_ROOM);
    send_to_char("You have been taken to the killing fields\r\n", ch);
    look_at_room(ch, 0);
    send_to_arena("%s has joined the blood bath.\r\n", GET_NAME(ch));
    char_gold(ch, - (cost_per_lev * GET_LEVEL(ch)), GOLD_ALL);
    arena_pot += (cost_per_lev * GET_LEVEL(ch));
    ch->Send( "You pay %d coins to enter the arena\r\n",
                     (cost_per_lev * GET_LEVEL(ch)));
    alter_hit(ch, -GET_MAX_HIT(ch));
    alter_mana(ch, -GET_MAX_MANA(ch));
    alter_move(ch, -GET_MAX_MOVE(ch));
    remove_all_normal_affects(ch);
  }
}


ACMD(do_chaos)
{
  char cost[MAX_INPUT_LENGTH], lolimit[MAX_INPUT_LENGTH];
  char hilimit[MAX_INPUT_LENGTH], start_delay[MAX_INPUT_LENGTH];
  char length[MAX_INPUT_LENGTH];
  char buf[MAX_INPUT_LENGTH];

  /* Usage: chaos lo hi start_delay cost/lev length */

  if (in_arena != ARENA_OFF)
  {
    send_to_char("There is an arena running already.\r\n", ch);
    return;
  }
  half_chop(argument, lolimit, buf);
  lo_lim = atoi(lolimit);
  half_chop(buf, hilimit, buf);
  hi_lim = atoi(hilimit);
  half_chop(buf, start_delay, buf);
  start_time = atoi(start_delay);
  half_chop(buf, cost, buf);
  cost_per_lev = atoi(cost);
  strcpy(length, buf);
  game_length = atoi(length);

  if (hi_lim > LVL_IMPL)
  {
    send_to_char("Please choose a hi_lim under the Imps level\r\n",
                 ch);
    return;
  }
  if (lolimit < 0)
    silent_end();
  if (!*lolimit || !*hilimit || !*start_delay || !*cost || !*length)
  {
    send_to_char("Usage: chaos lo hi start_delay cost/lev length\r\n",
                 ch);
    return;
  }
  if (lo_lim > hi_lim)
  {
    send_to_char("Sorry, low limit must be lower than hi limit.\r\n",
                 ch);
    return;
  }
  if ((hi_lim < 0) || (cost_per_lev < 0) || (game_length < 0))
  {
    send_to_char("I like positive numbers thank you.\r\n", ch);
    return;
  }
  if (start_time <= 0)
  {
    send_to_char("Lets at least give them a chance to enter!\r\n", ch);
    return;
  }
  if ((GET_LEVEL(ch) < LVL_IMPL) && (cost_per_lev < MIN_ARENA_COST))
  {
    send_to_char("The minimum cost per level is 100.\r\n", ch);
    return;
  }
  in_arena = ARENA_START;
  time_to_start = start_time;
  time_left_in_game = 0;
  arena_pot = 0;
  bet_pot = 0;
  start_arena();
}


void start_arena(void)
{

  if (time_to_start == 0)
  {
    show_jack_pot();
    in_arena = ARENA_RUNNING; /* start the blood shed */
    time_left_in_game = game_length;
    start_game();
  }
  else
  {
    send_to_arena(
      "The Killing Fields are open to levels %d thru %d, \r\n"
      "%d coins/level to enter.\r\n%d hour%s to start.\r\n"
      "Type arena to enter.",
      lo_lim, hi_lim,cost_per_lev, time_to_start, time_to_start > 1 ? "s" : "");

    time_to_start--;
  }
}


void show_jack_pot(void)
{
  send_to_arena("\007\007Lets get ready to RUMBLE!!!!!!!!\r\n"
                "The jackpot for this arena is %ld coins.\r\n",
                arena_pot);
  send_to_arena("There are %ld coins in the betting pool.\r\n", bet_pot);
}


void start_game(void)
{
  register Character *i;
  Descriptor *d;

  for (d = descriptor_list; d; d = d->next)
  {
    if (!d->connected)
    {
      i = d->character;
      if (i->in_room->zone == ARENA_ZONE &&
          ROOM_FLAGGED(IN_ROOM(i), ROOM_ARENA) &&
          (i->in_room != NULL))
      {
        send_to_char
        ("\r\nThe floor opens, droping you in the arena.\r\n",
         i);
        move_char_to(i,
                     real_room(number
                               (ARENA_START_ROOM,
                                ARENA_END_ROOM)));
        look_at_room(i, 0);
      }
    }
  }
  do_game();
}


void do_game(void)
{

  if (num_in_arena() == 1)
  {
    in_arena = ARENA_OFF;
    find_game_winner();
  }
  else if (time_left_in_game == 0)
  {
    in_arena = ARENA_OFF;
    do_end_game();
  }
  else if (num_in_arena() == 0)
  {
    in_arena = ARENA_OFF;
    silent_end();
  }
  else if ((time_left_in_game % 5) || time_left_in_game <= 4)
  {
    send_to_arena(
      "With %d hours left in the game there are %d players left.\r\n",
      time_left_in_game, num_in_arena());
  }
  else if (time_left_in_game == 1)
  {
    send_to_arena(
      "With 1 hour left in the game there are %d players left.\r\n",
      num_in_arena());
  }
  time_left_in_game--;
}


void find_game_winner(void)
{
  register Character *i;
  Descriptor *d;
  struct hall_of_fame_element *fame_node;

  for (d = descriptor_list; d; d = d->next)
    if (!d->connected)
    {
      i = d->character;
      if (i->in_room->zone == ARENA_ZONE &&
          ROOM_FLAGGED(IN_ROOM(i), ROOM_ARENA) &&
          (!IS_NPC(i)) && (i->in_room != NULL) &&
          GET_LEVEL(i) < LVL_HERO)
      {
        alter_hit(i, -GET_MAX_HIT(i));
        alter_mana(i, -GET_MAX_MANA(i));
        alter_move(i, -GET_MAX_MOVE(i));
        remove_all_normal_affects(i);
        move_char_to(i, real_room(3001));
        look_at_room(i, 0);
        act("$n falls from the sky.", FALSE, i, 0, 0, TO_ROOM);
        if (game_length - time_left_in_game == 1)
        {
          send_to_arena(
            "After 1 brief hour, %s is declared the winner.\r\n",
            GET_NAME(i));
        }
        else
        {
          send_to_arena(
            "After %d hours, %s is declared the winner.\r\n",
            game_length - time_left_in_game, GET_NAME(i));
        }
        char_gold(i, arena_pot / 2, GOLD_ALL);
        new_send_to_char(i,
                         "You have earned %ld coins for winning the arena.\r\n",
                         (arena_pot / 2));
        send_to_arena(
          "%s has been awarded %ld coins for winning arena.\r\n",
          GET_NAME(i), (arena_pot / 2));
        CREATE(fame_node, struct hall_of_fame_element, 1);
        strncpy(fame_node->name, GET_NAME(i), MAX_NAME_LENGTH);
        fame_node->name[MAX_NAME_LENGTH] = '\0';
        fame_node->date = time(0);
        fame_node->award = (arena_pot / 2);
        fame_node->next = fame_list;
        fame_list = fame_node;
        write_fame_list();
        find_bet_winners(i);
      }
    }
}


void silent_end(void)
{
  in_arena = ARENA_OFF;
  start_time = 0;
  game_length = 0;
  time_to_start = 0;
  time_left_in_game = 0;
  arena_pot = 0;
  bet_pot = 0;
  send_to_arena(
    "It looks like no one was brave enough to enter the Arena.");
}


void do_end_game(void)
{
  register Character *i;
  Descriptor *d;

  for (d = descriptor_list; d; d = d->next)
    if (!d->connected)
    {
      i = d->character;
      if (ROOM_FLAGGED(IN_ROOM(i), ROOM_ARENA) &&
          (i->in_room != NULL) && (!IS_NPC(i)))
      {
        alter_hit(i, -GET_MAX_HIT(i));
        alter_mana(i, -GET_MAX_MANA(i));
        alter_move(i, -GET_MAX_MOVE(i));
        remove_all_normal_affects(i);
        move_char_to(i, real_room(1205));
        look_at_room(i, 0);
        act("$n falls from the sky.", FALSE, i, 0, 0, TO_ROOM);
      }
    }
  send_to_arena( "After %d hours of battle the Match is a draw.",
                 game_length);
  time_left_in_game = 0;
}


int num_in_arena(void)
{
  register Character *i;
  Descriptor *d;
  int num = 0;

  for (d = descriptor_list; d; d = d->next)
    if (!d->connected)
    {
      i = d->character;
      if (i->in_room->zone == ARENA_ZONE &&
          ROOM_FLAGGED(IN_ROOM(i), ROOM_ARENA)
          && (i->in_room != NULL))
      {
        if (GET_LEVEL(i) < LVL_HERO)
          num++;
      }
    }
  return num;
}


ACMD(do_awho)
{
  Descriptor *d;
  Character *tch;
  int num = 0;

  if (in_arena == ARENA_OFF)
  {
    send_to_char("There is no Arena going on right now.\r\n", ch);
    return;
  }
  ch->Send( "  Gladiators in the Arena\r\n"
                   "---------------------------------------\r\n"
                   "Game Length = %-3d   Time To Start %-3d\r\n",  game_length, time_to_start);
  ch->Send( "Level Limits %d to %d\r\n", lo_lim, hi_lim);
  ch->Send( "         Jackpot = %ld\r\n",  arena_pot);
  ch->Send( "---------------------------------------\r\n");

  for (d = descriptor_list; d; d = d->next)
    if (!d->connected)
    {
      tch = d->character;
      if (IN_ROOM(tch)->zone == ARENA_ZONE &&
          ROOM_FLAGGED(IN_ROOM(tch), ROOM_ARENA) &&
          (IN_ROOM(tch) != NULL) && GET_LEVEL(tch) < LVL_HERO)
      {
        ch->Send( "%-20.20s%s",
                         GET_NAME(tch), (!(++num % 3) ? "\r\n" : ""));
      }
    }
  ch->Send( "\r\n\r\n");
}


ACMD(do_ahall)
{
  char site[MAX_INPUT_LENGTH],  *timestr;
  struct hall_of_fame_element *fame_node;

  if (!fame_list)
  {
    send_to_char("No-one is in the Hall of Fame.\r\n", ch);
    return;
  }

  ch->Send( "%s|---------------------------------------|%s\r\n",
                   CCBLU(ch, C_NRM), CCNRM(ch, C_NRM));
  ch->Send(
                   "%s|%sPast Winners of Arena%s                  |%s\r\n",
                   CCBLU(ch, C_NRM), CCNRM(ch, C_NRM), CCBLU(ch, C_NRM),
                   CCNRM(ch, C_NRM));
  ch->Send(
                   "%s|---------------------------------------|%s\r\n\r\n",
                   CCBLU(ch, C_NRM), CCNRM(ch, C_NRM));


  ch->Send( "%-10.10s  %-15.15s  %-40s\r\n", "Date", "Award Amt", "Name");

  ch->Send( "%-10.10s  %-15.15s  %-40s\r\n",
                   "---------------------------------",
                   "--------------------------------",
                   "-------------------------------------------------");


  for (fame_node = fame_list; fame_node; fame_node = fame_node->next)
  {
    if (fame_node->date)
    {
      timestr = asctime(localtime(&(fame_node->date)));
      *(timestr + 10) = 0;
      strcpy(site, timestr);
    }
    else
      strcpy(site, "Unknown");
    ch->Send( "%-10.10s  %-16ld %s\r\n", site, fame_node->award,
                     CAP(fame_node->name));
  }

  return;
}


void load_hall_of_fame(void)
{
  FILE *fl;
  int date, award;
  char name[MAX_NAME_LENGTH + 1];
  struct hall_of_fame_element *next_node;

  fame_list = 0;

  if (!(fl = fopen(HALL_FAME_FILE, "r")))
  {
    perror("Unable to open hall of fame file");
    return;
  }
  while (fscanf(fl, "%s %d %d", name, &date, &award) == 3)
  {
    CREATE(next_node, struct hall_of_fame_element, 1);
    strncpy(next_node->name, name, MAX_NAME_LENGTH);
    next_node->name[MAX_NAME_LENGTH] = '\0';
    next_node->date = date;
    next_node->award = award;
    next_node->next = fame_list;
    fame_list = next_node;
  }

  fclose(fl);
}


void write_fame_list(void)
{
  FILE *fl;

  if (!(fl = fopen(HALL_FAME_FILE, "w")))
  {
    /* syserrlog("Error writing _hall_of_fame_list", FALSE); */
    log("Error writing _hall_of_fame_list");
    return;
  }
  write_one_fame_node(fl, fame_list);   /* recursively write from end to start */
  fclose(fl);

  return;
}


void write_one_fame_node(FILE * fp, struct hall_of_fame_element *node)
{
  if (node)
  {
    write_one_fame_node(fp, node->next);
    fprintf(fp, "%s %ld %ld\n", node->name, (long) node->date,
            node->award);
  }
}


void find_bet_winners(Character *winner)
{
  register Character *i;
  Descriptor *d;


  for (d = descriptor_list; d; d = d->next)
    if (!d->connected)
    {
      i = d->character;
      if ((!IS_NPC(i)) && (i->in_room != NULL) &&
          (GET_BETTED_ON(i) == GET_IDNUM(winner))
          && GET_AMT_BET(i) > 0)
      {
        new_send_to_char(i,  "You have won %d coins on your bet.\r\n",
                         GET_AMT_BET(i) * 2);
        char_gold(i, GET_AMT_BET(i) * 2, GOLD_ALL);
        GET_BETTED_ON(i) = 0;
        GET_AMT_BET(i) = 0;
      }
      else          /* they bet and did't win, or didn't bet at all */
        GET_AMT_BET(i) = 0;
    }
}


int arena_ok(Character *ch, Character *victim)
{
  if (ROOM_FLAGGED(IN_ROOM(ch), ROOM_ARENA) &&
      ROOM_FLAGGED(victim->in_room, ROOM_ARENA))
    return TRUE;
  else if (both_pk(ch,victim))
    return TRUE;
  else
    return FALSE;
}


void arena_kill(Character *ch)
{
  room_rnum rm = real_room(1205);
  if (FIGHTING(ch))
    stop_fighting(ch);

  remove_all_normal_affects(ch);
  if (IN_ROOM(ch)->zone == ARENA_ZONE &&
      ROOM_FLAGGED(IN_ROOM(ch), ROOM_ARENA))
  {

    GET_HIT(ch) = GET_MAX_HIT(ch);
    GET_MANA(ch) = GET_MAX_MANA(ch);
    GET_MOVE(ch) = GET_MAX_MOVE(ch);
  }
  else
  {
    GET_HIT(ch)  = GET_MAX_HIT(ch);
  }
  check_regen_rates(ch);

  update_pos(ch);

  if (!move_char_to(ch, rm))
    move_char_to(ch, CONFIG_MORTAL_START);
  look_at_room(ch, 0);
  act("$n is dropped from the sky.", FALSE, ch, 0, 0, TO_ROOM);
}
