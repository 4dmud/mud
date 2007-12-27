/* ************************************************************************
*   File: act.movement.c                                Part of CircleMUD *
*  Usage: movement commands, door handling, & sleep/rest/etc state        *
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
#include "house.h"
#include "constants.h"
#include "dg_scripts.h"
#include "kalten.h"

/* external functs */
void eq_to_room(CHAR_DATA *ch);
int Crash_delete_file(char *name);
void add_follower(struct char_data *ch, struct char_data *leader);
void death_cry(struct char_data *ch);
void dismount_char(struct char_data *ch);
void mount_char(struct char_data *ch, struct char_data *mount);
void improve_skill(struct char_data *ch, int skill);
int special(struct char_data *ch, int cmd, char *arg);
int find_eq_pos(struct char_data *ch, struct obj_data *obj, char *arg);
int ok_damage_shopkeeper(struct char_data *ch, struct char_data *victim);
int has_metal_detector(struct char_data *ch);
void spill_gold(struct char_data *ch);
int can_fly(struct char_data *ch);
ACMD(do_sac);
int buildwalk(struct char_data *ch, int dir);
int move_fusion(CHAR_DATA *ch, int dir);
int delete_pobj_file(char *name);
void hit_death_trap(CHAR_DATA *ch);
void raw_kill(CHAR_DATA *ch, CHAR_DATA * vict);


/* MatingMod Defines */
#define NINE_MONTHS     6000	/* 6000 realtime minutes TO GO */
#define MONTHS_8        5333
#define MONTHS_7        4666	/* Note: These are MONTHS REMAINING */
#define MONTHS_6        4000
#define MONTHS_5        3333
#define MONTHS_4        2666
#define MONTHS_3        2000
#define MONTHS_2        1333
#define MONTH_1         666

/* local functions */
int use_stamina(struct char_data *ch, int amount);
void do_doorcmd(struct char_data *ch, struct obj_data *obj, int door,
                int scmd);
int has_boat(struct char_data *ch);
int find_door(struct char_data *ch, const char *type, char *dir,
              const char *cmdname);
int has_key(struct char_data *ch, obj_vnum key);
int ok_pick(struct char_data *ch, obj_vnum keynum, int pickproof,
            int scmd);
int skill_cost(int h, int m, int v, struct char_data *ch);
OBJ_DATA  *is_trapped(room_rnum rm);
int can_enter(struct char_data *ch, room_vnum room, room_vnum to_room);

ACMD(do_gen_door);
ACMD(do_enter);
ACMD(do_leave);
ACMD(do_stand);
ACMD(do_sit);
ACMD(do_rest);
ACMD(do_sleep);
ACMD(do_wake);
ACMD(do_follow);
ASKILL(skill_mount);
ASKILL(skill_blackjack);
ASKILL(skill_snare);
ASKILL(skill_tame);

C_FUNC(allow_follow);

ACMD(do_recall)
{
  if (IS_NPC(ch))
    return;

  if ((GET_LEVEL(ch) <= 40 && REMORTS(ch) == 0) || GET_LEVEL(ch) > LVL_IMMORT)
  {
    act("A gap in time appears and $n steps through.", FALSE, ch, 0, 0, TO_ROOM);
    move_char_to(ch, CONFIG_MORTAL_START);
    act("Through a gap in space and time $n arrives at recall point.", FALSE, ch, 0, 0, TO_ROOM);
    act("Through a gap in space and time you arrive at recall point.", FALSE, ch, 0, 0, TO_CHAR);
    if (GET_LEVEL(ch) < LVL_IMMORT && GET_LEVEL(ch) > 35)
      new_send_to_char(ch, "{cRThis service is available until level 40 only.{c0\r\n");
    look_at_room(ch, 0);
    entry_memory_mtrigger(ch);
    greet_mtrigger(ch, -1);
    greet_memory_mtrigger(ch);
    enter_wtrigger(IN_ROOM(ch), ch, -1);
  }
}

int can_move(struct char_data *ch)
{
  if (AFF_FLAGGED(ch, AFF_STUCK))
    return (0);
  if (AFF_FLAGGED(ch, AFF_HOLD))
    return (0);
  if (get_sub_status(ch, SUB_JUGGLE) == STATUS_ON)
    return (0);


  return 1;
}
/* simple function to determine if char can walk on water */
int has_boat(struct char_data *ch)
{
  struct obj_data *obj;
  int i;

  if (GET_LEVEL(ch) >= LVL_IMMORT)
    return (1);

  if (AFF_FLAGGED(ch, AFF_WATERWALK))
    return (1);
  if (can_fly(ch))
    return (1);

  /* non-wearable boats in inventory will do it */
  for (obj = ch->carrying; obj; obj = obj->next_content)
    if (GET_OBJ_TYPE(obj) == ITEM_BOAT
        && (find_eq_pos(ch, obj, NULL) < 0))
      return (1);

  /* and any boat you're wearing will do it too */
  for (i = 0; i < NUM_WEARS; i++)
    if (HAS_BODY(ch, i) && GET_EQ(ch, i) &&
        GET_OBJ_TYPE(GET_EQ(ch, i)) == ITEM_BOAT)
      return (1);

  return (0);
}

int can_fly(struct char_data *ch)
{
  sh_int i;

  if (GET_LEVEL(ch) >= LVL_IMPL)
    return (1);

  for (i = 0; i < NUM_WEARS; i++)
  {
    if (HAS_BODY(ch, i) && (GET_EQ(ch, i)) &&
        (GET_OBJ_TYPE(GET_EQ(ch, i)) == ITEM_WINGS))
      return (1);
  }

  if (AFF_FLAGGED(ch, AFF_FLY))
    return (1);

  return (0);
}

int has_space_suit(struct char_data *ch)
{
  sh_int i;

  if (GET_LEVEL(ch) >= LVL_IMPL)
    return (1);

  for (i = 0; i < NUM_WEARS; i++)
    if (HAS_BODY(ch, i) && GET_EQ(ch, i) &&
        GET_OBJ_TYPE(GET_EQ(ch, i)) == ITEM_SPACESUIT)
      return (1);

  return (0);
}

int has_sun_protection(struct char_data *ch)
{
  sh_int i;

  if (GET_LEVEL(ch) >= LVL_IMPL)
    return (1);

  for (i = 0; i < NUM_WEARS; i++)
    if (HAS_BODY(ch, i) && GET_EQ(ch, i) &&
        GET_OBJ_TYPE(GET_EQ(ch, i)) == ITEM_THERMAL_PROT)
      return (1);

  return (0);
}

int can_breathe_underwater(struct char_data *ch)
{
  sh_int i;

  for (i = 0; i < NUM_WEARS; i++)
    if (HAS_BODY(ch, i) && GET_EQ(ch, i) &&
        GET_OBJ_TYPE(GET_EQ(ch, i)) == ITEM_AQUALUNG)
      return (1);

  if (AFF_FLAGGED(ch, AFF_GILLS))
    return (1);

  return (0);
}

OBJ_DATA  *is_trapped(room_rnum rm)
{
  OBJ_DATA *trap = NULL;

  if (VALID_ROOM_RNUM(rm))
  {
    /* mark vehicles on the map */
    for (trap = rm->contents; trap; trap = trap->next_content)
    {
      if (GET_OBJ_TYPE(trap) == ITEM_TRAP)
        if (TRAP_IS_SET(trap))
          return trap;
    }
  }

  return trap;
}

int move_cost(struct char_data *ch, int dir)
{
  int need_movement = 0;
  if (!ch)
    return 0;
  /* move points needed is avg. move loss for src and destination sect type */
  need_movement = (movement_loss[SECT(IN_ROOM(ch))] +
                   movement_loss[SECT(EXIT(ch, dir)->to_room)]) >> 1;
  if (!IS_NPC(ch))
  {
    /* MatingMod Additions -- Extra "Encumbrance" */
    if ((PREG(ch) < (MONTHS_5)) && (PREG(ch) >= (MONTHS_4)))
    {
      /* 5-6th Month */
      need_movement = need_movement + 1;
    }
    else if ((PREG(ch) < (MONTHS_4)) && (PREG(ch) >= (MONTHS_3)))
    {
      /* 7th */
      need_movement = need_movement + 3;
    }
    else if ((PREG(ch) < (MONTHS_3)) && (PREG(ch) >= (MONTHS_2)))
    {
      /* 8th */
      need_movement = need_movement * 2;
      if (need_movement <= 4)
        need_movement = 5;
    }
    else if ((PREG(ch) < (MONTHS_2)) && (PREG(ch) >= (MONTH_1)))
    {
      /* 9th */
      need_movement = (need_movement * 2) + 2;
      if (need_movement <= 5)
        need_movement = 6;
    }
    else if (PREG(ch) < (MONTH_1) && PREG(ch) > NOT_PREG)
    {
      /* Very close to birth */
      need_movement = need_movement * 3;
      if (need_movement <= 6)
        need_movement = 7;
    }
    else if (PREG(ch) <= 6 && PREG(ch) > NOT_PREG)
    {
      act("You can't move! You're giving birth!\r\n", TRUE, ch, 0, 0,
          TO_CHAR);
      return (-1);
    }
    /* End Mating Mod changes */
  }


  if (AFF_FLAGGED(ch, AFF_FLY))
    need_movement -= (need_movement * 0.30);


  if (IS_CARRYING_W(ch) > CAN_CARRY_W(ch))
  {
    new_send_to_char(ch, "You drag your feet under that amount of weight!\r\n");
    need_movement += GET_MAX_MOVE(ch) * 0.5;
  }
if (has_vehicle(ch))
return 0;
else
  return need_movement;
}

/* do_simple_move assumes
 *    1. That there is no master and no followers.
 *    2. That the direction exists.
 *
 *   Returns :
 *   1 : If success.
 *   0 : If fail
 */
int do_simple_move(struct char_data *ch, int dir, int need_specials_check)
{
  int same_room = 0, riding = 0, ridden_by = 0;
  struct room_data * was_in = IN_ROOM(ch);
  int need_movement = 0, need_m_movement = 0;
  int vnum, chance, has_moved = FALSE;
  int stam = 0;
  char local_buf[MAX_INPUT_LENGTH];
  char buf2[MAX_INPUT_LENGTH];
  struct obj_data *bike = NULL;
  /*    struct char_data *herd, *next;
      bool FOUND = FALSE;*/

  int special(struct char_data *ch, int cmd, char *arg);

  /*
   * Check for special routines (North is 1 in command list, but 0 here) Note
   * -- only check if following; this avoids 'double spec-proc' bug
   */
  if (need_specials_check && special(ch, dir + 1, ""))
    return (0);

  if (!can_move(ch))
  {
    new_send_to_char(ch, "You can't go anywhere in that state!\r\n");
    return 0;
  }
  // check if they're mounted
  if (RIDING(ch))
    riding = 1;
  if (RIDDEN_BY(ch))
    ridden_by = 1;

  // if they're mounted, are they in the same room w/ their mount(ee)?
  if (riding && HERE(RIDING(ch),ch))
    same_room = 1;
  else if (ridden_by && HERE(RIDDEN_BY(ch), ch))
    same_room = 1;

  // tamed mobiles cannot move about (DAK)
  if (ridden_by && same_room && AFF_FLAGGED(ch, AFF_TAMED))
  {
    send_to_char("You've been tamed.  Now act it!\r\n", ch);
    return (0);
  }
  // charmed?
  if (IS_AFFECTED(ch, AFF_CHARM) && ch->master && HERE(ch, ch->master))
  {
    new_send_to_char(ch, "The thought of leaving your master makes you weep.\r\n");
    return (0);
  }
  if (IS_NPC(ch) && MOB_FLAGGED(ch, MOB_STAY_SECTOR) && (SECT(IN_ROOM(ch)) != SECT(EXIT(ch, dir)->to_room)))
  {
    if (ch->desc)
      new_send_to_char(ch, "You can't move out of this sector.\r\n");
    return 0;
  }
  if (!IS_IMM(ch))
  {
    if ((ZONE_FLAGGED(IN_ROOM(ch)->zone, ZONE_CLOSED)))
    {
      new_send_to_char(ch, "That area isn't open yet. Try back later.\r\n");
      return (0);
    }
    /* if this room or the one we're going to needs a boat, check for one */
    if ((SECT(IN_ROOM(ch)) == SECT_WATER_NOSWIM) ||
        (SECT(EXIT(ch, dir)->to_room) == SECT_WATER_NOSWIM))
    {
      if (riding && same_room && !has_boat(RIDING(ch)))
      {
        new_send_to_char(ch, "Your mount needs a boat to go there.\r\n");
        return (0);
      }
      else if (!has_boat(ch))
      {
        new_send_to_char(ch, "You need a boat to go there.\r\n");
        return 0;
      }
    }


    /* if this room or the one we're going to needs a boat, check for one */
    if (((SECT(IN_ROOM(ch)) == SECT_WATER_SWIM) ||
         (SECT(EXIT(ch, dir)->to_room) == SECT_WATER_SWIM)))
    {
      if (riding && same_room && can_fly(RIDING(ch)))
      {
        new_send_to_char(ch, "Your mount can't fly into that room!\r\n");
        return (0);
      }
      else if (can_fly(ch))
      {
        new_send_to_char(ch, "You can't fly into that room!\r\n");
        return 0;
      }
    }

    /* if this room or the one we're going to is in the air, are we flying? */
    if ((SECT(IN_ROOM(ch)) == SECT_FLYING) ||
        (SECT(EXIT(ch, dir)->to_room) == SECT_FLYING))
    {
      if (riding && same_room && !can_fly(RIDING(ch)))
      {
        new_send_to_char(ch, "Your mount needs to be able to fly to go there.\r\n");
        return (0);
      }
      else if (!can_fly(ch))
      {
        new_send_to_char(ch, "You need to be able to fly to go there.\r\n");
        return (0);
      }
    }
  }//is imm
  
  
    if (SECT(EXIT(ch, dir)->to_room) == SECT_SPACE)
    {
      struct obj_data *v;
      if ((v = has_vehicle(ch)) == NULL)
      {
        need_movement = 1000;
	stam = use_stamina(ch, 100);
      }
      else
      {
        if (GET_FUEL(v) == 0)
        {
          act("Your $p is out of fuel.", FALSE, ch, v, NULL, TO_CHAR);
          return 0;
        }
        GET_FUEL(v)--;

      }
    }
    else
    {
      struct obj_data *v;
      if ((v = has_vehicle(ch)) != NULL)
      {
        if (GET_FUEL(v) == 0)
        {
          act("Your $p is out of fuel.", FALSE, ch, v, NULL, TO_CHAR);
          return 0;
        }
        GET_FUEL(v)--;
      }
    }

  /*
   * if this room or the one we're going to is in space, 
   * do we have a spacesuit?
   * I think I'll let them enter the room, but they will be damaged. -- kalten
   if ((SECT(IN_ROOM(ch)) == SECT_SPACE) ||
   (SECT(EXIT(ch, dir)->to_room) == SECT_SPACE)) {
   if (!has_space_suit(ch)) { // && (!GET_RACE(ch) == RACE_MARTIAN)) {
   send_to_char("You need to be wearing a spacesuit to go there.\r\n", ch);
   return (0);
   }
   }
   */

  /*
   * if this room or the one we're going to is underwater,
   * can we breath?
   * I think I'll let them enter the room, but they will be damaged. -- kalten
   if((SECT(IN_ROOM(ch)) == SECT_UNDERWATER) ||
   (SECT(EXIT(ch, dir)->to_room) == SECT_UNDERWATER)) {
   if (!can_breathe_underwater(ch)) {
   send_to_char("You can't breath underwater so that's not a good idea.\r\n", ch);
   return (0);
   }
   }
   */
   if (!need_movement)
  need_movement = move_cost(ch, dir);
  if (need_movement == -1) //giving birth
    return 0;


  if ((RIDING(ch) && HERE(RIDING(ch), ch)))
  {
    need_m_movement = move_cost(RIDING(ch), dir);
    if (need_m_movement == -1)//pregnant horse?
      return 0;
    if (GET_MOVE(RIDING(ch)) < need_m_movement)
    {
      send_to_char("Your mount is too exhausted.\r\n", ch);
      return (0);
    }
  }
  else
  {
    if (GET_MOVE(ch) < need_movement && !IS_NPC(ch))
    {
      if (need_specials_check && ch->master)
        send_to_char("You are too exhausted to follow.\r\n", ch);
      else
        send_to_char("You are too exhausted.\r\n", ch);
      return (0);
    }
  }
  {
    
    if (!IS_NPC(ch) && GET_RACE(ch) == RACE_CENTAUR && !RIDDEN_BY(ch))
      stam += use_stamina(ch, 1);
    else if (RIDING(ch) && HERE(RIDING(ch), ch))
    {
      if (use_stamina(RIDING(ch),  MIN(need_m_movement, 15)  * (AFF_FLAGGED(RIDING(ch), AFF_HASTE) ? 0.5 : 1)) < 0)
      {
        act("Your mount collapses in exhaustion, feeling sick.", FALSE, ch, 0, 0, TO_CHAR);
        act("$n falls over in exhaustion, panting.", FALSE, RIDING(ch), 0, 0, TO_ROOM);

        GET_POS(RIDING(ch)) = POS_RESTING;
        dismount_char(ch);
        return 0;
      }
      stam += use_stamina(ch, 1);
    }
    else
      stam += use_stamina(ch,  MIN(need_movement, 15)  * (AFF_FLAGGED(ch, AFF_HASTE) ? 0.5 : 1));



    if (stam < 0)
    {
      act("You fall over in exhaustion, feeling sick.", FALSE, ch, 0, 0, TO_CHAR);
      act("$n falls over in exhaustion, panting.", FALSE, ch, 0, 0, TO_ROOM);
      GET_POS(ch) = POS_RESTING;
      dismount_char(ch);
      return 0;
    }

  }
  if ((RIDING(ch) && HERE(RIDING(ch), ch))
      && GET_SKILL(ch, SKILL_RIDING) < number(1, GET_LEVEL(RIDING(ch)) * 2) - number(-4, need_movement)
      && !AFF_FLAGGED(RIDING(ch), AFF_TAMED))
  {
    act("$N rears backwards, throwing you to the ground.", FALSE, ch,
        0, RIDING(ch), TO_CHAR);
    act("You rear backwards, throwing $n to the ground.", FALSE, ch, 0,
        RIDING(ch), TO_VICT);
    act("$N rears backwards, throwing $n to the ground.", FALSE, ch, 0,
        RIDING(ch), TO_NOTVICT);
    dismount_char(ch);
    set_fighting(RIDING(ch), ch);
    return (0);
  }
  else
  {
    if (RIDING(ch) && HERE(RIDING(ch), ch))
      improve_skill(ch, SKILL_RIDING);
  }

  vnum = EXIT(ch, dir)->to_room->number;

  if (IS_SET_AR(ROOM_FLAGS(IN_ROOM(ch)), ROOM_ATRIUM))
  {
    if (!House_can_enter(ch, vnum))
    {
      send_to_char("That's private property -- no trespassing!\r\n",  ch);
      return (0);
    }
  }

  if ((RIDING(ch) || RIDDEN_BY(ch))
      && IS_SET_AR(ROOM_FLAGS(EXIT(ch, dir)->to_room), ROOM_TUNNEL))
  {
    send_to_char("There isn't enough room there, while mounted.\r\n",    ch);
    return (0);
  }
  else
  {
    if (IS_SET_AR(ROOM_FLAGS(EXIT(ch, dir)->to_room), ROOM_TUNNEL) &&
        num_pc_in_room((EXIT(ch, dir)->to_room)) > CONFIG_TUNNEL_SIZE)
    {
      if (CONFIG_TUNNEL_SIZE > 1)
        new_send_to_char(ch, "There isn't enough room for you to go there!\r\n");
      else
        new_send_to_char(ch, "There isn't enough room there for more than one person!\r\n");
      return (0);
    }
  }



  if (IS_SET_AR(ROOM_FLAGS(EXIT(ch, dir)->to_room), ROOM_ROLEPLAY) &&
      !PLR_FLAGGED(ch, PLR_ROLEPLAYER) && !IS_NPC(ch))
  {
    send_to_char
    ("That direction is off limits to non-roleplayers.\r\n", ch);
    return (0);
  }
  if (!enter_wtrigger(EXIT(ch, dir)->to_room, ch, dir))
    return 0;


  if (!leave_wtrigger(IN_ROOM(ch), ch, dir) || IN_ROOM(ch) != was_in) /* prevent teleport crashes */
    return 0;

  if (!leave_otrigger(IN_ROOM(ch), ch, dir) || IN_ROOM(ch) != was_in) /* prevent teleport crashes */
    return 0;

  if (!leave_mtrigger(ch, dir) || IN_ROOM(ch) != was_in) /* prevent teleport crashes */
    return 0;

  /*will drop excess gold if carring it*/
  if (GET_LEVEL(ch) < LVL_IMMORT && !IS_NPC(ch) && !number(0, 10))
    spill_gold(ch);

  if (!IS_NPC(ch) && GET_LEVEL(ch) < LVL_IMMORT && !RIDDEN_BY(ch))
  {
    if (RIDING(ch) && HERE(RIDING(ch), ch))
      alter_move(ch, need_movement - (need_movement * 0.3));
    else
      alter_move(ch, need_movement);
  }
  if (RIDING(ch) && HERE(RIDING(ch), ch))
    alter_move(RIDING(ch), need_m_movement + (need_movement * 0.3));
  else if (RIDDEN_BY(ch) && HERE(RIDDEN_BY(ch), ch))
    alter_move(ch, need_movement - (need_movement * 0.3));

  /*hide move messages from people with the preference to not see it */
  message_type = REST_MOVE;
  /****/
  if (RIDING(ch) && HERE(RIDING(ch), ch))
  {
    if (!IS_AFFECTED(RIDING(ch), AFF_SNEAK))
    {
      if (IS_AFFECTED(ch, AFF_SNEAK))
      {
        snprintf(buf2, sizeof(buf2), "$n leaves %s.", dirs[dir]);
        act(buf2, TRUE, RIDING(ch), 0, 0, TO_ROOM);
      }
      else
      {
        snprintf(buf2, sizeof(buf2), "$n rides $N %s.", dirs[dir]);
        act(buf2, TRUE, ch, 0, RIDING(ch), TO_NOTVICT);
      }
    }
  }
  else if (RIDDEN_BY(ch) && HERE(RIDDEN_BY(ch), ch))
  {
    if (!IS_AFFECTED(ch, AFF_SNEAK))
    {
      if (IS_AFFECTED(RIDDEN_BY(ch), AFF_SNEAK))
      {
        snprintf(buf2, sizeof(buf2), "$n leaves %s.", dirs[dir]);
        act(buf2, TRUE, ch, 0, 0, TO_ROOM);
      }
      else
      {
        snprintf(buf2, sizeof(buf2), "$n rides $N %s.", dirs[dir]);
        act(buf2, TRUE, RIDDEN_BY(ch), 0, ch, TO_NOTVICT);
      }
    }
  }
  else if (!IS_AFFECTED(ch, AFF_SNEAK))
  {
    size_t len = 0;
    if (AFF_FLAGGED(ch, AFF_HASTE))
    {
      if (!AFF_FLAGGED(ch, AFF_HOLD))
        len = snprintf(local_buf, sizeof(local_buf), " quickly");
    }
    else if (AFF_FLAGGED(ch, AFF_HOLD)
             && !AFF_FLAGGED(ch, AFF_HASTE))
      len = snprintf(local_buf, sizeof(local_buf), " slowly");


    if ((GET_HIT(ch) * 10) < (GET_MAX_HIT(ch)))
      snprintf(local_buf + len, sizeof(local_buf) - len, ", leaving a trail of dark fluid.");
    else if (!IS_NPC(ch) && GET_COND(ch, DRUNK) > 10)
      snprintf(local_buf + len, sizeof(local_buf) - len, ", weaving crookedly.");
    else
      snprintf(local_buf + len, sizeof(local_buf) - len, ".");


    if (AFF_FLAGGED((ch), AFF_FLY))
      snprintf(buf2, sizeof(buf2), "$n flies %s%s", dirs[dir], local_buf);
    else if (IS_NPC(ch) && MOB_FLAGGED(ch, MOB_SWIMS))
      snprintf(buf2, sizeof(buf2), "$n swims %s%s", dirs[dir], local_buf);
    else if (IS_ANIMAL(ch))
      snprintf(buf2, sizeof(buf2), "$n leaves %s%s", dirs[dir], local_buf);
    else
      switch (SECT(IN_ROOM(ch)))
      {
      case SECT_INSIDE:
        snprintf(buf2, sizeof(buf2), "$n walks %s%s", dirs[dir], local_buf);
        break;
      case SECT_CITY:
        snprintf(buf2, sizeof(buf2), "$n strolls %s%s", dirs[dir], local_buf);
        break;
      case SECT_FIELD:
        snprintf(buf2, sizeof(buf2), "$n strolls %s%s", dirs[dir], local_buf);
        break;
      case SECT_FOREST:
        snprintf(buf2, sizeof(buf2), "$n stalks %s%s", dirs[dir], local_buf);
        break;
      case SECT_HILLS:
        snprintf(buf2, sizeof(buf2), "$n strides %s%s", dirs[dir], local_buf);
        break;
      case SECT_MOUNTAIN:
        snprintf(buf2, sizeof(buf2), "$n hikes %s%s", dirs[dir], local_buf);
        break;
      case SECT_WATER_SWIM:
      case SECT_UNDERWATER:
        snprintf(buf2, sizeof(buf2), "$n swims %s%s", dirs[dir], local_buf);
        break;
      case SECT_WATER_NOSWIM:
        snprintf(buf2, sizeof(buf2), "$n paddles %s%s", dirs[dir], local_buf);
        break;
      case SECT_FLYING:
        snprintf(buf2, sizeof(buf2), "$n flies %s%s", dirs[dir], local_buf);
        break;
      case SECT_DESERT:
        snprintf(buf2, sizeof(buf2), "$n trudges %s%s", dirs[dir], local_buf);
        break;
      case SECT_SPACE:
        snprintf(buf2, sizeof(buf2), "$n floats %s%s", dirs[dir], local_buf);
        break;
      case SECT_ROAD:
        snprintf(buf2, sizeof(buf2), "$n plods %s%s", dirs[dir], local_buf);
        break;
      case SECT_ENTRANCE:
        snprintf(buf2, sizeof(buf2), "$n enters %s%s", dirs[dir], local_buf);
        break;
      case SECT_ATMOSPHERE:
        snprintf(buf2, sizeof(buf2), "$n burns %s%s", dirs[dir], local_buf);
        break;
      case SECT_SUN:
        snprintf(buf2, sizeof(buf2), "$n fries %s%s", dirs[dir], local_buf);
        break;
      case SECT_BLACKHOLE:
        snprintf(buf2, sizeof(buf2), "$n falls %s%s", dirs[dir], local_buf);
        break;
      case SECT_VEHICLE:
        snprintf(buf2, sizeof(buf2), "$n drives %s%s", dirs[dir], local_buf);
        break;
      case SECT_SWAMP:
        snprintf(buf2, sizeof(buf2), "$n wades %s%s", dirs[dir], local_buf);
        break;
      case SECT_REEF:
        snprintf(buf2, sizeof(buf2), "$n clambers %s%s", dirs[dir], local_buf);
        break;
      case SECT_TUNDRA:
        snprintf(buf2, sizeof(buf2), "$n lopes %s%s", dirs[dir], local_buf);
        break;
      case SECT_SNOW:
        snprintf(buf2, sizeof(buf2), "$n clambers %s%s", dirs[dir], local_buf);
        break;
      case SECT_ICE:
        snprintf(buf2, sizeof(buf2), "$n slides %s%s", dirs[dir], local_buf);
        break;
      case SECT_PRAIRIE:
        snprintf(buf2, sizeof(buf2), "$n walks %s%s", dirs[dir], local_buf);
        break;
      case SECT_BADLANDS:
        snprintf(buf2, sizeof(buf2), "$n weaves %s%s", dirs[dir], local_buf);
        break;
      case SECT_RAIL:
        snprintf(buf2, sizeof(buf2), "$n leaves %s%s", dirs[dir], local_buf);
        break;
      default:
        snprintf(buf2, sizeof(buf2), "$n leaves %s%s", dirs[dir], local_buf);
        break;
      }
    act(buf2, TRUE, ch, 0, 0, TO_ROOM);
  }
  //end and restart message restrict
  message_type = NOTHING;

  /* see if an entry trigger disallows the move */
  if (!entry_mtrigger(ch))
    return 0;

  message_type = REST_MOVE;

  /*if (MOB_FLAGGED(ch, MOB_HERD))
  act("The entire herd follows the leader.", TRUE, ch, 0, 0,
   TO_ROOM);*/
  was_in = IN_ROOM(ch);
  if (SITTING(ch) && GET_OBJ_TYPE(SITTING(ch)) == ITEM_SPACEBIKE)
    bike = SITTING(ch);
  

  move_char_to(ch, was_in->dir_option[dir]->to_room);
   if (ch->desc != NULL && !IS_NPC(ch) && (!PLR_FLAGGED(ch, PLR_SPEEDWALK) && (ch->master ? !PLR_FLAGGED(ch->master, PLR_SPEEDWALK) : 1)))
    look_at_room(ch, 0);

  if (bike)
  {
    struct char_data *tempch;

    obj_from_room(bike);
    obj_to_room(bike, IN_ROOM(ch));

    if (OBJ_SAT_IN_BY(bike) == NULL)
      OBJ_SAT_IN_BY(bike) = ch;
    for (tempch = OBJ_SAT_IN_BY(bike);tempch != ch;tempch = NEXT_SITTING(tempch))
    {
      if (NEXT_SITTING(tempch))
        continue;
      NEXT_SITTING(tempch) = ch;
    }

    SITTING(ch) = bike;
    NEXT_SITTING(ch) = NULL;
    GET_OBJ_VAL(bike, 1) += 1;
    
  }

  if (RIDING(ch) && IN_ROOM(RIDING(ch)) == was_in && IN_ROOM(RIDING(ch)) != IN_ROOM(ch))
  {
    if (move_char_to(RIDING(ch), IN_ROOM(ch)))
      has_moved = TRUE;
  }
  else if (RIDDEN_BY(ch) && IN_ROOM(RIDDEN_BY(ch)) == was_in
           && IN_ROOM(RIDDEN_BY(ch)) != IN_ROOM(ch))
  {
    if (move_char_to(RIDDEN_BY(ch), IN_ROOM(ch)))
      has_moved = TRUE;
  }
  else if (!bike)
  {
    dismount_char(ch);
    riding = 0;
    same_room = 0;
  }



  if (!IS_AFFECTED(ch, AFF_SNEAK))
  {
    if (riding && same_room && !IS_AFFECTED(RIDING(ch), AFF_SNEAK))
    {
      snprintf(buf2, sizeof(buf2), "$n arrives from %s%s, riding $N.",
               (dir < UP ? "the " : ""),
               (dir == UP ? "below" : dir ==
                DOWN ? "above" : dirs[rev_dir[dir]]));
      if (has_moved)
        act(buf2, TRUE, ch, 0, RIDING(ch), TO_ROOM);
    }
    else if (ridden_by && same_room
             && !IS_AFFECTED(RIDDEN_BY(ch), AFF_SNEAK))
    {
      snprintf(buf2, sizeof(buf2), "$n arrives from %s%s, ridden by $N.",
               (dir < UP ? "the " : ""),
               (dir == UP ? "below" : dir ==
                DOWN ? "above" : dirs[rev_dir[dir]]));
      if (has_moved)
        act(buf2, TRUE, ch, 0, RIDDEN_BY(ch), TO_ROOM);
    }
    else if (!riding || (riding && !same_room))
    {
      act("$n has arrived.", TRUE, ch, 0, 0, TO_ROOM);
    }
  }
  if (IS_AFFECTED(ch, AFF_SNEAK))
    if (ch->master && ch->master != ch)
      if (IN_ROOM(ch->master) == IN_ROOM(ch))
        new_send_to_char(ch->master, "%s sneaks in after you.\r\n",
                         PERS(ch, ch->master));

  /*if (IS_NPC(ch) && MOB_FLAGGED(ch, MOB_HERD)) {
  for (herd = was_in->people; herd; herd = next) {
   next = herd->next_in_room;
   if (MOB_FLAGGED(herd, MOB_HERD)) {
  FOUND = TRUE;
  char_from_room(herd);
  char_to_room(herd, was_in->dir_option[dir]->to_room);
  // log("%s is a herd animal.", GET_NAME(herd));
   }
  }
  if (FOUND)
   act("A herd has arrived, following the leader.", TRUE, ch, 0,
  NULL, TO_ROOM);
  } */

 

  /* end the message restriction */
  message_type = NOTHING;

  /* DEATHTRAPS */
  if (ROOM_FLAGGED(IN_ROOM(ch), ROOM_DEATH) )
  {
    if (RIDING(ch))
      hit_death_trap(RIDING(ch));

    if (RIDDEN_BY(ch))
      hit_death_trap(RIDDEN_BY(ch));

    hit_death_trap(ch);
    return (0);
  }

  if (IN_ROOM(ch) != was_in)
  {
    entry_mtrigger(ch);
    entry_memory_mtrigger(ch);
    if (!greet_mtrigger(ch, dir))
    {
      move_char_to(ch, was_in);
      look_at_room(ch, 0);
    }
    else
      greet_memory_mtrigger(ch);

    if (ch->desc != NULL)
    {
      chance = number(1, 100);
      if (chance > 25 && has_metal_detector(ch) && CAN_MINE(ch))
      {
        send_to_char("\007", ch);
        send_to_char("Your metal detector begins to go off.\r\n", ch);
        send_to_char("\007", ch);
      }
    }
  }
  return (1);
}

void hit_death_trap(CHAR_DATA *ch)
{
  if (!ch)
    return;
  if ( (GET_LEVEL(ch) < LVL_IMMORT) || IS_NPC(ch))
  {
    log_death_trap(ch);
    eq_to_room(ch);
    raw_kill(ch, NULL);
    if (!IS_NPC(ch))
      GET_DT_CNT(ch) += 1;
  }
}

int can_pass_fence(struct char_data *ch, int dir)
{
  int chance, type = 0;
  char fencebuf[100];


  if (EXIT_FLAGGED(EXIT(ch, dir), EX_FENCE_GATE_OPEN))
  {
    type = 1;
    if (!IS_NPC(ch))
      snprintf(fencebuf, sizeof(fencebuf), "You pass through a gate.\r\n");

  }
  else if (EXIT_FLAGGED(EXIT(ch, dir), EX_FENCE_WIRE))
  {
    type = 2;
    if (!IS_NPC(ch))
      snprintf(fencebuf, sizeof(fencebuf), "You climb through a wire fence.\r\n");

  }
  else if (EXIT_FLAGGED(EXIT(ch, dir), EX_FENCE_WOOD))
  {
    type = 3;
    if (!IS_NPC(ch))
      snprintf(fencebuf, sizeof(fencebuf), "You climb over a wooden fence.\r\n");

  }
  else if (EXIT_FLAGGED(EXIT(ch, dir), EX_FENCE_MESH))
  {
    type = 4;
    if (!IS_NPC(ch))
      snprintf(fencebuf, sizeof(fencebuf), "You climb over a wire mesh fence.\r\n");

  }
  else
    return 1;


  if (!IS_NPC(ch))
  {
    new_send_to_char(ch, "%s", fencebuf);
    return 1;
  }
  else
  {
    chance = number(0, 100);
    switch (type)
    {
    case 2:
      return (chance < 20);
    case 3:
      return (chance < 50);
    case 4:
      return (chance < 80);
      break;
    default:
      return 1;
    }
  }

}

int perform_move(struct char_data *ch, int dir, int need_specials_check)
{
  room_rnum was_in;
  struct follow_type *k, *next;
  struct char_data *mob;

  if (ch == NULL || dir < 0 || dir >= NUM_OF_DIRS || FIGHTING(ch))
    return (0);
  else if ((!EXIT(ch, dir) && !buildwalk(ch, dir)) || EXIT(ch, dir)->to_room == NULL)
    send_to_char("Alas, you cannot go that way...\r\n", ch);
  else if (IS_SET(EXIT(ch, dir)->exit_info, EX_HIDDEN) &&
           IS_SET(EXIT(ch, dir)->exit_info, EX_CLOSED))
    send_to_char("Alas, you cannot go that way...\r\n", ch);
  else if (EXIT_FLAGGED(EXIT(ch, dir), EX_CLOSED))
  {
    if (EXIT(ch, dir)->keyword)
    {
      new_send_to_char(ch, "The %s seems to be closed.\r\n",
                       fname(EXIT(ch, dir)->keyword));
    }
    else
      send_to_char("It seems to be closed.\r\n", ch);
  }
  else if (!can_pass_fence(ch, dir))
  {
    /* can go through fence*/

  }
  else
  {

    if (IS_AFFECTED(ch, AFF_HOLD))
    {
      if (FIGHTING(ch))
      {
        send_to_char("You are stuck in a snare!\r\n", ch);
        return 0;
      }
      new_send_to_char(ch, "You struggle ");
      if (number(1, 101) < GET_SKILL(ch, SKILL_TRAP_AWARE))
      {
        new_send_to_char(ch, "and manage to free yourself from the snare.\r\n");
        affect_from_char(ch, SKILL_SNARE);
      }
      else
      {
        new_send_to_char(ch, "but cant get free of the snare!\r\n");
        return 0;
      }
    }

    if (!ch->followers)
      return (do_simple_move(ch, dir, need_specials_check));

    was_in = IN_ROOM(ch);
    if (!do_simple_move(ch, dir, need_specials_check))
      return (0);

    for (k = ch->followers; k; k = next)
    {
      next = k->next;
      if ((k->follower->in_room == was_in) && (GET_POS(k->follower) >= POS_STANDING))
      {
        act("You follow $N.", FALSE, k->follower, 0, ch, TO_CHAR);
        perform_move(k->follower, dir, 1);
      }
    }

    for (mob = was_in->people; mob; mob = mob->next_in_room)
    {
      //log("%s is in the room, checking if it is part of a herd.", GET_NAME(mob));
      if (MOB_FLAGGED(mob, MOB_HERD) && GET_POS(mob) >= POS_STANDING)
        perform_move(mob, dir, 1);
    }

    return (1);
  }
  return (0);
}


ACMD(do_move)
{
  /*
   * This is basically a mapping of cmd numbers to perform_move indices.
   * It cannot be done in perform_move because perform_move is called
   * by other functions which do not require the remapping.
   */
  perform_move(ch, subcmd - 1, 0);
}


int find_door(struct char_data *ch, const char *type, char *dir,
              const char *cmdname)
{
  int door;

  if (*dir)
  {			/* a direction was specified */
    if ((door = search_block(dir, dirs, FALSE)) == -1)
    {	/* Partial Match */
      send_to_char("That's not a direction.\r\n", ch);
      return (-1);
    }
    if (EXIT(ch, door) && !IS_SET(EXIT(ch, door)->exit_info, EX_HIDDEN))
    {	/* Braces added according to indent. -gg */
      if (EXIT(ch, door)->keyword)
      {
        if (isname(type, EXIT(ch, door)->keyword))
          return (door);
        else
        {
          new_send_to_char(ch, "I see no %s there.\r\n", type);
          return (-1);
        }
      }
      else
        return (door);
    }
    else
    {
      new_send_to_char(ch,
                       "I really don't see how you can %s anything there.\r\n",
                       cmdname);
      return (-1);
    }
  }
  else
  {			/* try to locate the keyword */
    if (!*type)
    {
      new_send_to_char(ch, "What is it you want to %s?\r\n", cmdname);
      return (-1);
    }
    for (door = 0; door < NUM_OF_DIRS; door++)
      if (EXIT(ch, door)
          && !IS_SET(EXIT(ch, door)->exit_info, EX_HIDDEN))
        if (EXIT(ch, door)->keyword)
          if (isname(type, EXIT(ch, door)->keyword))
            return (door);

    new_send_to_char(ch, "There doesn't seem to be %s %s here.\r\n", AN(type),
                     type);
    return (-1);
  }
}


int has_key(struct char_data *ch, obj_vnum key)
{
  struct obj_data *o;

  for (o = ch->carrying; o; o = o->next_content)
    if (GET_OBJ_VNUM(o) == key)
      return (1);

  if (GET_EQ(ch, WEAR_HOLD))
    if (GET_OBJ_VNUM(GET_EQ(ch, WEAR_HOLD)) == key)
      return (1);

  return (0);
}



#define NEED_OPEN	(1 << 0)
#define NEED_CLOSED	(1 << 1)
#define NEED_UNLOCKED	(1 << 2)
#define NEED_LOCKED	(1 << 3)

const char *cmd_door[] =
  {
    "open",
    "close",
    "unlock",
    "lock",
    "pick"
  };

const int flags_door[] =
  {
    NEED_CLOSED | NEED_UNLOCKED,
    NEED_OPEN,
    NEED_CLOSED | NEED_LOCKED,
    NEED_CLOSED | NEED_UNLOCKED,
    NEED_CLOSED | NEED_LOCKED
  };


#define EXITN(room, door)		(room->dir_option[door])
#define OPEN_DOOR(room, obj, door)	((obj) ?\
		(TOGGLE_BIT(GET_OBJ_VAL(obj, 1), CONT_CLOSED)) :\
		(TOGGLE_BIT(EXITN(room, door)->exit_info, EX_CLOSED)))
#define LOCK_DOOR(room, obj, door)	((obj) ?\
		(TOGGLE_BIT(GET_OBJ_VAL(obj, 1), CONT_LOCKED)) :\
		(TOGGLE_BIT(EXITN(room, door)->exit_info, EX_LOCKED)))

void do_doorcmd(struct char_data *ch, struct obj_data *obj, int door,
                int scmd)
{
  room_rnum other_room = NULL;
  struct room_direction_data *back = 0;
  char buf[MAX_INPUT_LENGTH];
  size_t len = 0;

  len += snprintf(buf, sizeof(buf), "$n %ss ", cmd_door[scmd]);
  if (!obj && ((other_room = EXIT(ch, door)->to_room) != NULL))
    if ((back = other_room->dir_option[rev_dir[door]]) != NULL)
      if (back->to_room != IN_ROOM(ch))
        back = 0;

  switch (scmd)
  {
  case SCMD_OPEN:
  case SCMD_CLOSE:
    OPEN_DOOR(IN_ROOM(ch), obj, door);
    if (back)
      OPEN_DOOR(other_room, obj, rev_dir[door]);
    new_send_to_char(ch, "%s", CONFIG_OK);
    break;
  case SCMD_UNLOCK:
  case SCMD_LOCK:
    LOCK_DOOR(IN_ROOM(ch), obj, door);
    if (back)
      LOCK_DOOR(other_room, obj, rev_dir[door]);
    send_to_char("*Click*\r\n", ch);
    if (obj && ROOM_FLAGGED(IN_ROOM(ch), ROOM_HOUSE))
      House_crashsave(GET_ROOM_VNUM(IN_ROOM(ch)));
    break;
  case SCMD_PICK:
    LOCK_DOOR(IN_ROOM(ch), obj, door);
    if (back)
      LOCK_DOOR(other_room, obj, rev_dir[door]);
    improve_skill(ch, SKILL_PICK_LOCK);
    send_to_char("The lock quickly yields to your skills.\r\n", ch);
    len += snprintf(buf + len, sizeof(buf) - len, "$n skillfully picks the lock on ");
    break;
  }

  /* Notify the room */
  len += snprintf(buf + len, sizeof(buf) - len, "%s%s.", ((obj) ? "" : "the "),
                  (obj) ? "$p" : (EXIT(ch, door)->keyword ? "$F" : "door"));
  if (!(obj) || (obj->in_room != NULL))
    act(buf, FALSE, ch, obj, obj ? 0 : EXIT(ch, door)->keyword,
        TO_ROOM);

  /* Notify the other room */
  if ((scmd == SCMD_OPEN || scmd == SCMD_CLOSE) && back)
  {
    snprintf(buf, sizeof(buf), "The %s is %s%s from the other side.",
             (back->keyword ? fname(back->keyword) : "door"),
             cmd_door[scmd], (scmd == SCMD_CLOSE) ? "d" : "ed");
    if (EXIT(ch, door)->to_room->people)
    {
      act(buf, FALSE, EXIT(ch, door)->to_room->people, 0, 0,
          TO_ROOM);
      act(buf, FALSE, EXIT(ch, door)->to_room->people, 0, 0,
          TO_CHAR);
    }
  }
}


int ok_pick(struct char_data *ch, obj_vnum keynum, int pickproof, int scmd)
{
  int percent;

  percent = number(1, 101);

  if (scmd == SCMD_PICK)
  {
    if (keynum < 0)
      send_to_char("Odd - you can't seem to find a keyhole.\r\n",
                   ch);
    else if (pickproof)
      send_to_char("It resists your attempts to pick it.\r\n", ch);
    else if (percent > GET_SKILL(ch, SKILL_PICK_LOCK))
      send_to_char("You failed to pick the lock.\r\n", ch);
    else
      return (1);
    return (0);
  }
  return (1);
}


#define DOOR_IS_OPENABLE(ch, obj, door)	((obj) ? \
			((GET_OBJ_TYPE(obj) == ITEM_CONTAINER) && \
			OBJVAL_FLAGGED(obj, CONT_CLOSEABLE)) :\
			((EXIT_FLAGGED(EXIT(ch, door), EX_ISDOOR))))
#define DOOR_IS_OPEN(ch, obj, door)	((obj) ? \
			(!OBJVAL_FLAGGED(obj, CONT_CLOSED)) :\
			(!EXIT_FLAGGED(EXIT(ch, door), EX_CLOSED)))
#define DOOR_IS_UNLOCKED(ch, obj, door)	((obj) ? \
			(!OBJVAL_FLAGGED(obj, CONT_LOCKED)) :\
			(!EXIT_FLAGGED(EXIT(ch, door), EX_LOCKED)))
#define DOOR_IS_PICKPROOF(ch, obj, door) ((obj) ? \
			(OBJVAL_FLAGGED(obj, CONT_PICKPROOF)) : \
			(EXIT_FLAGGED(EXIT(ch, door), EX_PICKPROOF)))

#define DOOR_IS_CLOSED(ch, obj, door)	(!(DOOR_IS_OPEN(ch, obj, door)))
#define DOOR_IS_LOCKED(ch, obj, door)	(!(DOOR_IS_UNLOCKED(ch, obj, door)))
#define DOOR_KEY(ch, obj, door)		((obj) ? (GET_OBJ_VAL(obj, 2)) : \
					(EXIT(ch, door)->key))
#define DOOR_LOCK(ch, obj, door)	((obj) ? (GET_OBJ_VAL(obj, 1)) : \
					(EXIT(ch, door)->exit_info))

ACMD(do_gen_door)
{
  int door = -1;
  obj_vnum keynum;
  char type[MAX_INPUT_LENGTH], dir[MAX_INPUT_LENGTH];
  struct obj_data *obj = NULL;
  struct char_data *victim = NULL;

  skip_spaces(&argument);
  if (!*argument)
  {
    new_send_to_char(ch, "%s what?\r\n", cmd_door[subcmd]);
    return;
  }
  two_arguments(argument, type, dir);
  if (!generic_find
      (type, FIND_OBJ_INV | FIND_OBJ_ROOM, ch, &victim, &obj))
    door = find_door(ch, type, dir, cmd_door[subcmd]);

  if ((obj) || (door >= 0))
  {
    keynum = DOOR_KEY(ch, obj, door);
    if (!(DOOR_IS_OPENABLE(ch, obj, door)))
      act("You can't $F that!", FALSE, ch, 0, cmd_door[subcmd],
          TO_CHAR);
    else if (!DOOR_IS_OPEN(ch, obj, door)
             && IS_SET(flags_door[subcmd], NEED_OPEN))
      send_to_char("But it's already closed!\r\n", ch);
    else if (!DOOR_IS_CLOSED(ch, obj, door) &&
             IS_SET(flags_door[subcmd], NEED_CLOSED))
      send_to_char("But it's currently open!\r\n", ch);
    else if (!(DOOR_IS_LOCKED(ch, obj, door)) &&
             IS_SET(flags_door[subcmd], NEED_LOCKED))
      send_to_char("Oh.. it wasn't locked, after all..\r\n", ch);
    else if (!(DOOR_IS_UNLOCKED(ch, obj, door)) &&
             IS_SET(flags_door[subcmd], NEED_UNLOCKED))
    {
      if (!has_key(ch, keynum))
      {
        send_to_char("It seems to be locked.\r\n", ch);
      }
      else
      {/* if have key, unlock then open in one go - mord*/
        do_doorcmd(ch, obj, door, SCMD_UNLOCK);
        do_doorcmd(ch, obj, door, subcmd);
      }
    }
    else if (!has_key(ch, keynum) && (GET_LEVEL(ch) < LVL_GOD) &&
             ((subcmd == SCMD_LOCK) || (subcmd == SCMD_UNLOCK)))
      send_to_char("You don't seem to have the proper key.\r\n", ch);
    else if (ok_pick(ch, keynum, DOOR_IS_PICKPROOF(ch, obj, door), subcmd))
    {
      do_doorcmd(ch, obj, door, subcmd);
    }
  }
  return;
}
#define FTYPE_NONE	0
#define FTYPE_OPEN 	1
#define FTYPE_CLOSE	2
#define FTYPE_BUILD	3
#define FTYPE_REMOVE	4
#define FTYPE_WIRE	5
#define FTYPE_WOOD	6
#define FTYPE_MESH	7
#define GATE_USE_MSG "GATE <OPEN|CLOSE|BUILD|REMOVE> <direction>\r\n"
#define FENCE_USE_MSG "FENCE <BUILD|REMOVE> <WIRE|TIMBER|MESH> <direction>\r\n"

int parse_fence_type(char arg)
{
  arg = LOWER(arg);

  switch (arg)
  {
  case 'o': return FTYPE_OPEN;
  case 'c': return FTYPE_CLOSE;
  case 'b': return FTYPE_BUILD;
  case 'r': return FTYPE_REMOVE;
  default:  return FTYPE_NONE;

  }
}
int parse_fence_sort(char arg)
{
  arg = LOWER(arg);

  switch (arg)
  {
  case 'w': return FTYPE_WIRE;
  case 't': return FTYPE_WOOD;
  case 'm': return FTYPE_MESH;
  default:  return FTYPE_NONE;

  }
}

ACMD(do_fence)
{
  int dir, t;
  char btype[MAX_INPUT_LENGTH];
  char badir[MAX_INPUT_LENGTH];
  char bfsort[MAX_INPUT_LENGTH];
  char bfbuf[MAX_INPUT_LENGTH];
  char *adir, *type, *fsort, *fbuf;

  adir = badir;
  type = btype;
  fsort = bfsort;
  fbuf = bfbuf;

  /* check if the room we are in is a PASTURE type */


  fbuf = one_argument(argument, type);
  skip_spaces(&type);

  /* grab the type, and through the rest in a buf
     check what kind of command it is and split the buffer up -- mord
  */

  if (subcmd == SCMD_GATE)
    adir = fbuf;
  else
  {
    adir = (one_argument(fbuf, fsort));
    skip_spaces(&fsort);
  }

  skip_spaces(&adir);

  /* check if the player typed anything*/
  if (type == NULL || adir == NULL)
  {
    new_send_to_char(ch, "%s", (subcmd == SCMD_GATE ? GATE_USE_MSG : FENCE_USE_MSG));
    return;
  }
  else
  {
    /* make sure the type the typed is valid, hash check */
    if ((t = parse_fence_type(*type)) == 0)
    {
      new_send_to_char(ch, "%s", (subcmd == SCMD_GATE ? GATE_USE_MSG : FENCE_USE_MSG));
      return;
    }

    /* make sure the direction they typed is valid */
    dir = search_block(adir, dirs, FALSE);
    if (dir == NOWHERE)
    {
      new_send_to_char(ch, "Valid Directions Are: North, South, East, West, Up, and Down.\r\n");
      return;
    }
    /* make sure the sort of fence they want is valid*/
    if (subcmd == SCMD_FENCE)
    {
      if ((t = parse_fence_sort(*fsort)) == 0)
      {
        new_send_to_char(ch, "%s",  FENCE_USE_MSG);
        return;
      }
    }
  }

  /* now check, that the exit on the oposite side of this exit
     exists.
  */


  /* now check */

}

int followers_to_master(struct char_data *ch, room_rnum was_in)
{
  struct follow_type *k, *next;
  if (!ch->followers)
    return 0;

  for (k = ch->followers; k; k = next)
  {
    next = k->next;
    if ((IN_ROOM(k->follower) == was_in) && (GET_POS(k->follower) >= POS_STANDING))
    {
      act("You follow $N.", FALSE, k->follower, 0, ch, TO_CHAR);
      if (move_char_to(k->follower, IN_ROOM(ch)))
      {
        entry_memory_mtrigger(ch);
        greet_mtrigger(ch, -1);
        greet_memory_mtrigger(ch);
        enter_wtrigger(IN_ROOM(ch), ch, -1);
      }
      LOOK(k->follower);
      act("$n arrives following $N.", FALSE, k->follower, 0, ch, TO_NOTVICT);
      act("$n arrives following you.", FALSE, k->follower, 0, ch, TO_VICT);
    }
  }
  return 1;
}

ACMD(do_enter)
{
  struct obj_data *obj = NULL;
  int door;
  char buf[MAX_INPUT_LENGTH];
  struct room_data * was_in = IN_ROOM(ch);
  struct room_data * dest_room = NULL;
  one_argument(argument, buf);
  const char *to_char = NULL;
  const char *to_room = NULL;
  const char *arrive  = NULL;
  int pass = TRUE;

  if (*buf)
  {			/* an argument was supplied, search for door
                                        				 * keyword */
    if ((obj = get_obj_in_list_vis(ch, buf, NULL, IN_ROOM(ch)->contents)))
    {
      if (CAN_SEE_OBJ(ch, obj))
      {
        switch (GET_OBJ_TYPE(obj))
        {
        case ITEM_PORTAL:
          to_room = "$n enters $p.";
          to_char = "You enter $p.";
          arrive  = "$n steps out of $p into the room.";
          break;
        case ITEM_PORTAL_BUSH:
          to_room = "$n scurries through the thorny branches.";
          to_char = "You scurry through the branches, the sharp thorns scratching your arms and legs.";
          arrive  = "$n steps out of $p into the room.";
          break;
        case ITEM_PORTAL_WATER:
          to_room = "$n dives into the water and is gone.";
          to_char = "You take a deep breath and dive into the water.";
          arrive  = "$n swims out of $p, landing on $s feet.";
          break;
        case ITEM_PORTAL_HOLE:
          to_room = "$n squeezes through the narrow hole with great difficulty.";
          to_char = "You squeeze through the narrow hole, almost getting stuck on the way.";
          arrive  = "$n squeezes out of a narrow hole and into the room.";
          break;
        default:
          pass = FALSE;
          break;
        }
        if (pass)
        {

          dest_room = real_room(GET_OBJ_VAL(obj, 1));

          if (dest_room == NULL)
          {
            new_send_to_char(ch, "Sorry, you can't enter that currently.\r\n");
            new_mudlog(BRF, LVL_GOD, TRUE, "%s tried to enter portal vnum %d in room %d, but portal has no exit",
                       GET_NAME(ch), GET_OBJ_VNUM(obj), GET_ROOM_VNUM(IN_ROOM(ch)));
            return;
          }
          else
          {
            if (ROOM_FLAGGED(dest_room, ROOM_HOUSE))
            {
              room_vnum vnum = dest_room->number;

              if (!House_can_enter(ch, vnum))
              {
                send_to_char("That's private property -- no trespassing!\r\n", ch);
                return;
              }
            }
          }
          if (to_room != NULL)
            act(to_room, FALSE, ch, obj, 0, TO_ROOM);
          if (to_char != NULL)
            act(to_char, FALSE, ch, obj, 0, TO_CHAR);
          if (!move_char_to(ch, dest_room))
            return;


          if (!enter_wtrigger(IN_ROOM(ch), ch, -1))
          {
            move_char_to(ch, was_in);
          }
          else
          {
            entry_mtrigger(ch);
            entry_memory_mtrigger(ch);
            if (!greet_mtrigger(ch, -1))
            {
              move_char_to(ch, was_in);
            }
            else
              greet_memory_mtrigger(ch);

            if (arrive != NULL)
              act(arrive, FALSE, ch, obj, 0, TO_ROOM);
            if (IN_ROOM(ch) != was_in)
            {
              LOOK(ch);
              if ((RIDING(ch) && IN_ROOM(RIDING(ch)) == was_in))
              {
                move_char_to(RIDING(ch), IN_ROOM(ch));
                act("$N rides through on you.", FALSE, RIDING(ch), 0, ch, TO_CHAR);
                act("You ride through on $n.", FALSE, RIDING(ch), 0, ch, TO_VICT);
                LOOK(RIDING(ch));
              }
              if (RIDDEN_BY(ch) && IN_ROOM(RIDDEN_BY(ch)) == was_in)
              {
                move_char_to(RIDDEN_BY(ch), IN_ROOM(ch));
                act("$N rides through on you.", FALSE, RIDDEN_BY(ch), 0, ch, TO_CHAR);
                act("You ride through on $n.", FALSE, RIDDEN_BY(ch), 0, ch, TO_VICT);
                LOOK(RIDDEN_BY(ch));
              }
              followers_to_master(ch, was_in);
            }
            return;
          }
        }

      }
    }

    for (door = 0; door < NUM_OF_DIRS; door++)
      if (EXIT(ch, door))
        if (EXIT(ch, door)->keyword)
          if (!str_cmp(EXIT(ch, door)->keyword, buf))
          {
            perform_move(ch, door, 1);
            return;
          }
    new_send_to_char(ch, "There is no %s here.\r\n", buf);
  }
  else if (ROOM_FLAGGED(IN_ROOM(ch), ROOM_INDOORS))
    send_to_char("You are already indoors.\r\n", ch);
  else
  {
    /* try to locate an entrance */
    for (door = 0; door < NUM_OF_DIRS; door++)
      if (EXIT(ch, door))
        if (EXIT(ch, door)->to_room != NULL)
          if (!EXIT_FLAGGED(EXIT(ch, door), EX_CLOSED) &&
              ROOM_FLAGGED(EXIT(ch, door)->to_room,
                           ROOM_INDOORS))
          {
            perform_move(ch, door, 1);
            return;
          }
    send_to_char("You can't seem to find anything to enter.\r\n", ch);
  }
}

ACMD(do_leave)
{
  int door;

  if (OUTSIDE(ch))
    send_to_char("You are outside.. where do you want to go?\r\n", ch);
  else
  {
    for (door = 0; door < NUM_OF_DIRS; door++)
      if (EXIT(ch, door))
        if (EXIT(ch, door)->to_room != NULL)
          if (!EXIT_FLAGGED(EXIT(ch, door), EX_CLOSED) &&
              !ROOM_FLAGGED(EXIT(ch, door)->to_room,
                            ROOM_INDOORS))
          {
            perform_move(ch, door, 1);
            return;
          }
    send_to_char("I see no obvious exits to the outside.\r\n", ch);
  }
}


ACMD(do_stand)
{
  switch (GET_POS(ch))
  {
  case POS_STANDING:
    send_to_char("You are already standing.\r\n", ch);
    break;
  case POS_SITTING:
    send_to_char("You stand up.\r\n", ch);
    act("$n clambers to $s feet.", TRUE, ch, 0, 0, TO_ROOM);
    /* Were they sitting in a chair? */
    char_from_chair(ch);
    /* Will be sitting after a successful bash and may still be fighting. */
    GET_POS(ch) = FIGHTING(ch) ? POS_FIGHTING : POS_STANDING;
    break;
  case POS_RESTING:
    send_to_char("You stop resting, and stand up.\r\n", ch);
    act("$n stops resting, and clambers on $s feet.", TRUE, ch, 0, 0,
        TO_ROOM);
    GET_POS(ch) = POS_STANDING;
    /* Were they sitting in the chair */
    char_from_chair(ch);
    break;
  case POS_SLEEPING:
    send_to_char("You have to wake up first!\r\n", ch);
    break;
  case POS_FIGHTING:
    send_to_char("Do you not consider fighting as standing?\r\n", ch);
    break;
  default:
    send_to_char
    ("You stop floating around, and put your feet on the ground.\r\n",
     ch);
    act("$n stops floating around, and puts $s feet on the ground.",
        TRUE, ch, 0, 0, TO_ROOM);
    GET_POS(ch) = POS_STANDING;
    break;
  }
}


ACMD(do_sit)
{
  struct obj_data *chair;
  struct char_data *tempch;
  int found;
  char arg[MAX_INPUT_LENGTH];

  one_argument(argument, arg);

  if (!*arg)
    found = 0;
  if (!
      (chair =
         get_obj_in_list_vis(ch, arg, NULL, IN_ROOM(ch)->contents)))
    found = 0;
  else
  {
    found = 1;
  }

  switch (GET_POS(ch))
  {
  case POS_STANDING:

    if (found == 0)
    {
      send_to_char("You sit down.\r\n", ch);
      act("$n sits down.", FALSE, ch, 0, 0, TO_ROOM);
      dismount_char(ch);
      GET_POS(ch) = POS_SITTING;
    }
    else
    {
      if (GET_OBJ_TYPE(chair) != ITEM_FURNITURE)
      {
        send_to_char("You can't sit in that!\r\n", ch);
        return;
      }
      else if (GET_OBJ_VAL(chair, 1) > GET_OBJ_VAL(chair, 0))
      {
        /* val 1 is current number in chair, 0 is max in chair */
        act("$p looks like it's all full.", TRUE, ch, chair, 0,
            TO_CHAR);
        log("SYSERR: chair %d holding too many people.",
            GET_OBJ_VNUM(chair));
        return;
      }
      else if (GET_OBJ_VAL(chair, 1) == GET_OBJ_VAL(chair, 0))
      {
        act("There is no where left to sit upon $p.", TRUE, ch,
            chair, 0, TO_CHAR);
        return;
      }
      else
      {

        dismount_char(ch);
        if (OBJ_SAT_IN_BY(chair) == NULL)
          OBJ_SAT_IN_BY(chair) = ch;
        for (tempch = OBJ_SAT_IN_BY(chair); tempch != ch;
             tempch = NEXT_SITTING(tempch))
        {
          if (NEXT_SITTING(tempch))
            continue;
          NEXT_SITTING(tempch) = ch;
        }

        SITTING(ch) = chair;
        NEXT_SITTING(ch) = NULL;
        GET_OBJ_VAL(chair, 1) += 1;
        GET_POS(ch) = POS_SITTING;
        act("You sit down upon $p.", TRUE, ch, chair, 0, TO_CHAR);
        act("$n sits down upon $p.", TRUE, ch, chair, 0, TO_ROOM);
      }
    }
    break;
  case POS_SITTING:
    send_to_char("You're sitting already.\r\n", ch);
    break;
  case POS_RESTING:
    send_to_char("You stop resting, and sit up.\r\n", ch);
    act("$n stops resting.", TRUE, ch, 0, 0, TO_ROOM);
    GET_POS(ch) = POS_SITTING;
    dismount_char(ch);
    break;
  case POS_SLEEPING:
    send_to_char("You have to wake up first.\r\n", ch);
    break;
  case POS_FIGHTING:
    send_to_char("Sit down while fighting? Are you MAD?\r\n", ch);
    break;
  default:
    send_to_char("You stop floating around, and sit down.\r\n", ch);
    act("$n stops floating around, and sits down.", TRUE, ch, 0, 0,
        TO_ROOM);
    GET_POS(ch) = POS_SITTING;
    break;
  }
}


ACMD(do_rest)
{
  switch (GET_POS(ch))
  {
  case POS_STANDING:
    send_to_char("You sit down and rest your tired bones.\r\n", ch);
    act("$n sits down and rests.", TRUE, ch, 0, 0, TO_ROOM);
    GET_POS(ch) = POS_RESTING;

    dismount_char(ch);
    break;
  case POS_SITTING:
    send_to_char("You rest your tired bones.\r\n", ch);
    act("$n rests.", TRUE, ch, 0, 0, TO_ROOM);
    GET_POS(ch) = POS_RESTING;

    dismount_char(ch);
    break;
  case POS_RESTING:
    send_to_char("You are already resting.\r\n", ch);
    break;
  case POS_SLEEPING:
    send_to_char("You have to wake up first.\r\n", ch);
    break;
  case POS_FIGHTING:
    send_to_char("Rest while fighting?  Are you MAD?\r\n", ch);
    break;
  default:
    send_to_char
    ("You stop floating around, and stop to rest your tired bones.\r\n",
     ch);
    act("$n stops floating around, and rests.", FALSE, ch, 0, 0,
        TO_ROOM);
    GET_POS(ch) = POS_SITTING;
    break;
  }
}


ACMD(do_sleep)
{
  switch (GET_POS(ch))
  {
  case POS_STANDING:
  case POS_SITTING:
  case POS_RESTING:
    send_to_char("You go to sleep.\r\n", ch);
    act("$n lies down and falls asleep.", TRUE, ch, 0, 0, TO_ROOM);
    GET_POS(ch) = POS_SLEEPING;

    dismount_char(ch);
    break;
  case POS_SLEEPING:
    send_to_char("You are already sound asleep.\r\n", ch);
    break;
  case POS_FIGHTING:
    send_to_char("Sleep while fighting?  Are you MAD?\r\n", ch);
    break;
  default:
    send_to_char
    ("You stop floating around, and lie down to sleep.\r\n", ch);
    act("$n stops floating around, and lie down to sleep.", TRUE, ch,
        0, 0, TO_ROOM);
    GET_POS(ch) = POS_SLEEPING;
    break;
  }
}


ACMD(do_wake)
{
  struct char_data *vict = NULL;
  int self = 0;
  char arg[MAX_INPUT_LENGTH];

  one_argument(argument, arg);
  if (*arg)
  {
    if (GET_POS(ch) == POS_SLEEPING)
      send_to_char("Maybe you should wake yourself up first.\r\n",
                   ch);
    else if ((vict =
                get_char_vis(ch, arg, NULL, FIND_CHAR_ROOM)) == NULL)
      new_send_to_char(ch, "%s", CONFIG_NOPERSON);
    else if (vict == ch)
      self = 1;
    else if (AWAKE(vict))
      act("$E is already awake.", FALSE, ch, 0, vict, TO_CHAR);
    else if (AFF_FLAGGED(vict, AFF_SLEEP) || AFF_FLAGGED(vict, AFF_SWEET_DREAMS))
      act("You can't wake $M up!", FALSE, ch, 0, vict, TO_CHAR);
    else if (GET_POS(vict) < POS_SLEEPING)
      act("$E's in pretty bad shape!", FALSE, ch, 0, vict, TO_CHAR);
    else
    {
      act("You wake $M up.", FALSE, ch, 0, vict, TO_CHAR);
      act("You are awakened by $n.", FALSE, ch, 0, vict,
          TO_VICT | TO_SLEEP);
      if (subcmd == SCMD_ROUSE)
        GET_POS(vict) = POS_STANDING;
      else
        GET_POS(vict) = POS_SITTING;

    }
    if (!self)
      return;
  }
  if (AFF_FLAGGED(ch, AFF_SLEEP))
    send_to_char("You can't wake up!\r\n", ch);
  else if (AFF_FLAGGED(ch, AFF_SWEET_DREAMS))
    new_send_to_char(ch, "You are in a far to happy slumber.\r\n");
  else if (GET_POS(ch) > POS_SLEEPING)
    send_to_char("You are already awake...\r\n", ch);
  else
  {
    send_to_char("You awaken, and sit up.\r\n", ch);
    act("$n awakens.", TRUE, ch, 0, 0, TO_ROOM);
    GET_POS(ch) = POS_SITTING;
  }
}

ACMD(do_leader)
{
  int found = 0;
  struct char_data *leader;
  struct follow_type * f;
  char buf[MAX_INPUT_LENGTH];
  one_argument(argument, buf);

  if (*buf)
  {
    if (!(leader = get_char_vis(ch, buf, NULL, FIND_CHAR_ROOM)))
    {
      new_send_to_char(ch, "%s", CONFIG_NOPERSON);
      return;
    }
  }
  else
  {
    send_to_char("Whom do you wish to make the leader\r\n", ch);
    return;
  }
  if (leader == ch)
  {
    new_send_to_char(ch, "You give yourself the vote for leadership. Aint that dandy.\r\n");
    return;
  }

  if (IS_NPC(leader))
  {
    new_send_to_char(ch, "Yeah, put the most inteligent person as leader. Can't do that sorry...\r\n");
    return;
  }


  for (f = ch->followers; f; f = f->next)
    if (leader == f->follower)
    {
      found = 1;
      break;
    }


  if (!found)
    new_send_to_char(ch, "That person isnt following you!\r\n");
  else
  {
    new_send_to_char(ch, "You make %s the leader.\r\n", GET_NAME(leader));
    new_send_to_char(leader, "%s makes you the leader.\r\n", GET_NAME(ch));


    leader->master = NULL;
    ch->master = leader;
    leader->followers = ch->followers;
    ch->followers = NULL;



    for (f = leader->followers; f; f = f->next)
    {
      if (f->follower != leader)
      {
        f->follower->master = leader;
        new_send_to_char(f->follower, "%s makes %s your leader.\r\n", GET_NAME(ch), GET_NAME(leader));
      }
      else
      {
        f->follower = ch;
      }

    }
  }

}


ACMD(do_follow)
{
  struct char_data *leader;
  char buf[MAX_INPUT_LENGTH];
  //int diff;
  one_argument(argument, buf);

  if (*buf)
  {
    if (!(leader = get_char_vis(ch, buf, NULL, FIND_CHAR_ROOM)))
    {
      new_send_to_char(ch, "%s", CONFIG_NOPERSON);
      return;
    }
  }
  else
  {
    send_to_char("Whom do you wish to follow?\r\n", ch);
    return;
  }

  if (ch->followers != NULL)
  {
    new_send_to_char(ch, "You cannot follow another person while you have your own followers.\r\n");
    return;
  }




  if (leader->master && (leader != ch))
  {
    new_send_to_char(ch, "%s's master is %s, so you ask to follow %s insead.\r\n",
                     GET_NAME(leader), GET_NAME(leader->master), HMHR(leader->master));
    leader = leader->master;
  }

  if ((ch->master == leader))
  {
    act("You are already following $M.", FALSE, ch, 0, leader,
        TO_CHAR);
    return;
  }
  else if ((ch->master != NULL) && leader != ch)
  {
    act("You are already following $N.", FALSE, ch, 0, ch->master, TO_CHAR);
    return;
  }

  if (AFF_FLAGGED(ch, AFF_CHARM) && (ch->master))
  {
    act("But you only feel like following $N!", FALSE, ch, 0,
        ch->master, TO_CHAR);
  }
  else
  {			/* Not Charmed follow person */
    if (leader == ch)
    {
      if (!ch->master)
      {
        send_to_char("You are already following yourself.\r\n",
                     ch);
        return;
      }
      stop_follower(ch);
    }
    else
    {
      if (circle_follow(ch, leader))
      {
        send_to_char
        ("Sorry, but following in loops is not allowed.\r\n",
         ch);
        return;
      }
      if (IS_NPC(leader) && !IS_NPC(ch))
      {
        new_send_to_char(ch, "You cant follow %s!!\r\n",
                         PERS(leader, ch));
        return;
      }

      if (!IS_NPC(ch))
      {
        /*diff = (GET_LEVEL(ch) * current_class_is_tier_num(ch)) - (GET_LEVEL(leader) * current_class_is_tier_num(leader));
        if (abs(diff) > 100)
        {
          new_send_to_char(ch, "The difference in your powers is too great.\r\n");
          return;
        }*/
        if (STATE(leader->desc) == CON_PLAYING
            && !(PLR_FLAGGED(leader, PLR_MAILING)
                 || PLR_FLAGGED(leader, PLR_WRITING)))
        {
          snprintf(buf, sizeof(buf),"[%s wishes to join your group: Allow? (Type: Y | N )]", GET_NAME(ch));
          new_send_to_char(ch,"You ask %s if you can join %s group.\r\n", GET_NAME(leader), HSHR(leader));
          leader->loader = GET_IDNUM(ch);
          line_input(leader->desc, buf, allow_follow, NULL);
        }
        else
        {
          new_send_to_char(ch,"You cant follow %s just yet, %s is too busy to reply.\r\n", HSSH(leader), GET_NAME(leader));
        }
      }
      else
      {
        if (ch->master)
          stop_follower(ch);
        GET_PERC(ch) = 0;
        SET_BIT_AR(AFF_FLAGS(ch), AFF_GROUP);
        add_follower(ch, leader);
      }

    }
  }
}


// Mounts (DAK)
ASKILL(skill_mount)
{
  if (obj)
  {
    struct char_data *tempch;
    if (GET_OBJ_TYPE(obj) == ITEM_SPACEBIKE)
    {
      if (RIDING(ch) || RIDDEN_BY(ch))
      {
        new_send_to_char(ch, "You are already mounted.\r\n");
        return 0;
      }
      if (OBJ_SAT_IN_BY(obj))
      {
        new_send_to_char(ch, "It is already mounted.\r\n");
        return 0;
      }
      if (SITTING(ch))
      {
        new_send_to_char(ch, "You are sitting on something already.\r\n");
        return 0;
      }


      OBJ_SAT_IN_BY(obj) = ch;
      for (tempch = OBJ_SAT_IN_BY(obj); tempch != ch;
           tempch = NEXT_SITTING(tempch))
      {
        if (NEXT_SITTING(tempch))
          continue;
        NEXT_SITTING(tempch) = ch;
      }

      SITTING(ch) = obj;
      NEXT_SITTING(ch) = NULL;
      GET_OBJ_VAL(obj, 1) += 1;

      act("You mount $p", TRUE, ch, obj, NULL, TO_CHAR);
      act("$n mounts $p", TRUE, ch, obj, NULL, TO_ROOM);
      return 0;
    }
    if (!vict) {
  act("You can't mount $p!", FALSE, ch, obj, NULL, TO_CHAR);
  return 0;
  }

  }
  else if (!vict) {
  act("You can't mount that!", FALSE, ch, NULL, NULL, TO_CHAR);
  return 0;
  }
  
  
  if ((GET_RACE(ch) == RACE_CENTAUR && GET_LEVEL(ch) < LVL_IMMORT) || (!IS_NPC(vict) && GET_RACE(vict) != RACE_CENTAUR))
  {
    send_to_char("Ehh... no.\r\n", ch);
    return 0;
  }
  else if (RIDING(ch) || RIDDEN_BY(ch))
  {
    send_to_char("You are already mounted.\r\n", ch);
    return 0;
  }
  else if (RIDING(vict) || RIDDEN_BY(vict))
  {
    send_to_char("It is already mounted.\r\n", ch);
    return 0;
  }
  else if (!IS_NPC(vict) && !(!IS_NPC(vict) && PRF_FLAGGED(vict, PRF_MOUNTABLE)))
  {
    new_send_to_char(ch, "They have nomount turned off.\r\n");
    return 0;
  }
  else if ((GET_LEVEL(ch) < LVL_IMMORT && (IS_NPC(vict) && !MOB_FLAGGED(vict, MOB_MOUNTABLE))))
  {
    send_to_char("You can't mount that!\r\n", ch);
    return 0;
  }
  else if (!GET_SKILL(ch, SKILL_MOUNT))
  {
    send_to_char("First you need to learn *how* to mount.\r\n", ch);
    return 0;

  }
  else if (use_stamina(ch, 5) < 0)
  {
    new_send_to_char(ch, "You are too exausted!");
    return 0;
  }
  else if (GET_SKILL(ch, SKILL_MOUNT) <= number(1, 101))
  {
    act("You try to mount $N, but slip and fall off.", FALSE, ch, 0,
        vict, TO_CHAR);
    act("$n tries to mount you, but slips and falls off.", FALSE, ch,
        0, vict, TO_VICT);
    act("$n tries to mount $N, but slips and falls off.", TRUE, ch, 0,
        vict, TO_NOTVICT);
    damage(ch, ch, dice(1, 2), -1);
    return 0;
  }

  act("You mount $N.", FALSE, ch, 0, vict, TO_CHAR);
  act("$n mounts you.", FALSE, ch, 0, vict, TO_VICT);
  act("$n mounts $N.", TRUE, ch, 0, vict, TO_NOTVICT);
  mount_char(ch, vict);

  if (IS_NPC(vict) && !AFF_FLAGGED(vict, AFF_TAMED)
      && GET_SKILL(ch, SKILL_MOUNT) <= number(1, 101))
  {
    act("$N suddenly bucks upwards, throwing you violently to the ground!", FALSE, ch, 0, vict, TO_CHAR);
    act("$n is thrown to the ground as $N violently bucks!", TRUE, ch,
        0, vict, TO_NOTVICT);
    act("You buck violently and throw $n to the ground.", FALSE, ch, 0,
        vict, TO_VICT);
    dismount_char(ch);
    damage(vict, ch, dice(1, 3), -1);
    return 0;
  }
  else return SKILL_MOUNT;
}


ACMD(do_dismount)
{

  if (RIDING(ch))
  {
    
  if (SECT(IN_ROOM(ch)) == SECT_WATER_NOSWIM && !has_boat(ch))
  {
    new_send_to_char(ch,"Yah, right, and then drown...\r\n");
    return;
  }

  act("You dismount $N.", FALSE, ch, 0, RIDING(ch), TO_CHAR);
  act("$n dismounts from you.", FALSE, ch, 0, RIDING(ch), TO_VICT);
  act("$n dismounts $N.", TRUE, ch, 0, RIDING(ch), TO_NOTVICT);
  } else if (SITTING(ch) && GET_OBJ_TYPE(SITTING(ch)) == ITEM_SPACEBIKE) {
  act("You dismount $p.", FALSE, ch, SITTING(ch), NULL, TO_CHAR);
  act("$n dismounts $p.", TRUE, ch, SITTING(ch), NULL, TO_ROOM);
  } else {
  new_send_to_char(ch, "You aren't even riding anything.\r\n");
    return;
    }
  dismount_char(ch);
}


ACMD(do_buck)
{
  if (!RIDDEN_BY(ch))
  {
    send_to_char("You're not even being ridden!\r\n", ch);
    return;
  }
  else if (AFF_FLAGGED(ch, AFF_TAMED))
  {
    send_to_char("But you're tamed!\r\n", ch);
    return;
  }

  act("You quickly buck, throwing $N to the ground.", FALSE, ch, 0,
      RIDDEN_BY(ch), TO_CHAR);
  act("$n quickly bucks, throwing you to the ground.", FALSE, ch, 0,
      RIDDEN_BY(ch), TO_VICT);
  act("$n quickly bucks, throwing $N to the ground.", FALSE, ch, 0,
      RIDDEN_BY(ch), TO_NOTVICT);
  GET_POS(RIDDEN_BY(ch)) = POS_SITTING;
  if (number(0, 4))
  {
    send_to_char("You hit the ground hard!\r\n", RIDDEN_BY(ch));
    damage(RIDDEN_BY(ch), RIDDEN_BY(ch), dice(2, 4), TYPE_UNDEFINED);
  }
  dismount_char(ch);


  // you might want to call set_fighting() or some non-sense here if you
  // want the mount to attack the unseated rider or vice-versa.
}


ASKILL(skill_tame)
{
  char arg[MAX_INPUT_LENGTH];
  struct affected_type af;


  one_argument(argument, arg);

  if (GET_LEVEL(ch) < LVL_IMMORT && IS_NPC(vict)
      && !MOB_FLAGGED(vict, MOB_MOUNTABLE))
  {
    send_to_char("You can't do that to them.\r\n", ch);
    return 0;
  }
  if (!IS_NPC(vict))
  {
    send_to_char("You can't do that to them.\r\n", ch);
    return 0;
  }
  if (use_stamina(ch, 10) < 0)
  {
    new_send_to_char(ch, "You are too exausted!");
    return 0;
  }
  if ((GET_LEVEL(ch) +3) < GET_LEVEL(vict))
  {
    new_send_to_char(ch, "They are too powerful to tame, you must take your chances!\r\n");
    return 0;
  }

  af.type = SKILL_TAME;
  af.expire = HOURS_TO_EXPIRE((24));
  af.modifier = 0;
  af.location = APPLY_NONE;
  af.bitvector = AFF_TAMED;
  affect_join(vict, &af, FALSE, FALSE, FALSE, FALSE);

  act("You tame $N.", FALSE, ch, 0, vict, TO_CHAR);
  act("$n tames you.", FALSE, ch, 0, vict, TO_VICT);
  act("$n tames $N.", FALSE, ch, 0, vict, TO_NOTVICT);
  return SKILL_TAME;
}

/* Snare skill by Binky for EnvyMud */
ASKILL(skill_snare)
{
  struct affected_type af;

  GET_WAIT_STATE(ch) += ( 1 RL_SEC);
  if (use_stamina(ch, 5) < 0)
  {
    new_send_to_char(ch, "You are too exausted!");
    return 0;
  }

  /* charmed mobs not allowed to do this */
  if ((IS_NPC(ch) && !IS_AFFECTED(ch, AFF_CHARM))
      || (!IS_NPC(ch)
          && (number(1, 101) < GET_SKILL(ch, SKILL_SNARE))))
  {
    af.type = SKILL_SNARE;
    af.expire = HOURS_TO_EXPIRE(3);
    af.modifier = 0;
    af.location = APPLY_NONE;
    af.bitvector = AFF_HOLD;

    affect_join(vict, &af, FALSE, FALSE, FALSE, FALSE);

    act("You have ensnared $M!", FALSE, ch, NULL, vict, TO_CHAR);
    act("$n has ensnared you!", FALSE, ch, NULL, vict, TO_VICT);
    act("$n has ensnared $N.", FALSE, ch, NULL, vict, TO_NOTVICT);
    start_fighting(ch, vict);
    return SKILL_SNARE;
  }
  else
  {
    act("You failed to ensnare $M.  Uh oh!(3)",
        FALSE, ch, NULL, vict, TO_CHAR);
    act("$n tried to ensnare you!  Get $m!",
        FALSE, ch, NULL, vict, TO_VICT);
    act("$n attempted to ensnare $N, but failed!",
        FALSE, ch, NULL, vict, TO_NOTVICT);
    start_fighting(vict, ch);
  }

  return 0;
}

ASKILL(skill_blackjack)
{
  //char arg[MAX_INPUT_LENGTH];
  struct affected_type af;
  int chance = 0;
  char buf[MAX_STRING_LENGTH];
  ACMD(do_say);


  if (use_stamina(ch, 10) < 0)
  {
    new_send_to_char(ch, "You are exausted!");
    return 0;
  }


  if (affected_by_spell(vict, SKILL_BLACKJACK))
  {
    new_send_to_char
    (ch, "They are guarding their head too well right now.\r\n");
    return 0;
  }

  if (GET_POS(vict) == POS_SLEEPING)
  {
    send_to_char("But that person is already asleep!\r\n", ch);
    return 0;
  }

  if (GET_POS(vict) == POS_FIGHTING)
  {
    send_to_char("They are moving about too much.\r\n", ch);
    return 0;
  }
  chance += GET_SKILL(ch, SKILL_BLACKJACK);
  chance += (GET_LEVEL(ch) - (GET_LEVEL(vict)/2));

  if (!GET_EQ(ch, WEAR_HEAD))
    chance -= 10;

  if (number(1, 101) < chance)
  {
    act("You whack $N over the head with a heavy looking sack. Ouch.", FALSE, ch, 0, vict, TO_CHAR);
    act("$n whacks $N over the head with a heavy looking sack. Ouch.", FALSE, ch, 0, vict, TO_NOTVICT);
    new_send_to_char(ch, "You feel a sudden pain erupt through the back of your skull.\r\n");

    af.location = 0;
    af.type = SKILL_BLACKJACK;
    af.modifier = 0;
    af.bitvector = 0;
    af.expire = HOURS_TO_EXPIRE(1);
    affect_join(vict, &af, FALSE, FALSE, FALSE, FALSE);

    GET_POS(vict) = POS_SLEEPING;
    return ( SKILL_BLACKJACK);
  }
  else
  {
    act("You attempt to blackjack $N but fail.", FALSE, ch, 0, vict, TO_CHAR);
    act("$n attempts to blackjack $N but misses.", FALSE, ch, 0, vict, TO_NOTVICT);
    act("$n hits you over the head with a heavy sack.", FALSE, ch, 0,vict, TO_VICT);
    damage(ch, vict, 2 + 2 * number(2, 5), SKILL_BLACKJACK);


    snprintf(buf, sizeof(buf), "Help! %s tried to blackjack me!", GET_NAME(ch));
    do_say(vict, buf, 0, 0);
    start_fighting(vict, ch);

  }
  return 0;
}

ACMD(do_speedwalk)
{
  int dir, r;


  SET_BIT_AR(PLR_FLAGS(ch), PLR_SPEEDWALK);
  act("$n starts speedwalking.", TRUE, ch, 0, 0, TO_ROOM);
  new_send_to_char(ch, "You start speedwalking.\r\n");

  for (r = 1; *argument && r; argument++)
  {
    while (*argument == ' ')
      ++argument;

    switch (*argument)
    {
    case 'N':
    case 'n':
      dir = NORTH;
      break;
    case 'E':
    case 'e':
      dir = EAST;
      break;
    case 'S':
    case 's':
      dir = SOUTH;
      break;
    case 'W':
    case 'w':
      dir = WEST;
      break;
    case 'U':
    case 'u':
      dir = UP;
      break;
    case 'D':
    case 'd':
      dir = DOWN;
      break;
    default:
      send_to_char("Alas, you can't go that way.\r\n", ch);
      return;
      break;
    }

    r = perform_move(ch, dir, 1);
    //if (r && *(argument + 1))
    //send_to_char("\r\n", ch);
  }
  REMOVE_BIT_AR(PLR_FLAGS(ch), PLR_SPEEDWALK);
  LOOK(ch);
  act("$n stops speedwalking.", TRUE, ch, 0, 0, TO_ROOM);
}

/*
if cant consume amount then returns negitive
Otherwise it returns the stamina used
*/
int use_stamina(struct char_data *ch, int amount)
{
  float multi = 0.0f;
  int ret_val;
  time_t seconds = LAST_MOVE(ch) - time(0);

  if (amount < 0)
    return 0;

  if (IS_NPC(ch))
    amount *= 0.4;

  switch ((int)seconds)
  {
  case 0:
    multi = 2.0f;
    break;
  case 1:
    multi = 1.75f;
    break;
  case 2:
    multi = 1.5f;
    break;
  case 3:
    multi = 1.25f;
    break;
  case 4:
    multi = 0.5f;
    break;
  case 5:
    multi = 0.25f;
    break;
  default:
    multi = 0.1f;
  }

  ret_val = (GET_STAMINA(ch) - (amount * multi));
  if (ret_val > 0)
    alter_stamina(ch, amount * multi);

  LAST_MOVE(ch) = time(0);

  return ret_val;
}

int perform_move_obj(struct obj_data *obj, int dir, struct char_data *ch)
{
  if (IN_ROOM(obj) == NULL)
    return 0;
  if (obj == NULL || dir < 0 || dir >= NUM_OF_DIRS)
    return (0);
  else if (EXIT(obj, dir)->to_room == NULL)
  {
    if (ch)
      act("Alas, $p cannot go that way...\r\n", FALSE, ch, obj, 0, TO_CHAR);
  }
  else if (EXIT_FLAGGED(EXIT(obj, dir), EX_CLOSED))
  {
    if (ch)
      act("Alas, $p cannot go through closed doors...\r\n", FALSE, ch, obj, 0, TO_CHAR);
  }
  else
  {

    /* allows for new commands like pull object <direction>
       May want to add weight restrictions and all that blarg
       do it in the ACMD(do_pull), which doesnt exist...
    */
    if (ch && !(do_simple_move(ch, dir, 0)))
      return (0);

    if (!do_simple_obj_move(obj, dir, ch))
      return (0);

    return (1);
  }
  return (0);
}

/* do_simple_obj_move assumes
 
 *   Returns :
 *   1 : If success.
 *   0 : If fail
 */
int do_simple_obj_move(struct obj_data *obj, int dir, struct char_data *ch)
{
  room_rnum was_in;
  struct char_data *p;
  void view_room_by_rnum(struct char_data *ch, room_rnum is_in);

  switch (SECT(IN_ROOM(obj)))
  {
  case SECT_INSIDE:
  case SECT_CITY:
  case SECT_FIELD:
  case SECT_FOREST:
  case SECT_HILLS:
  case SECT_MOUNTAIN:
  case SECT_ROAD:
  case SECT_DESERT:
  case SECT_ENTRANCE:
  case SECT_TUNDRA:
    send_to_room(IN_ROOM(obj), "%s trundles %s.\r\n",obj->short_description, dirs[dir]);
    break;
  case SECT_WATER_SWIM:
  case SECT_UNDERWATER:
  case SECT_WATER_NOSWIM:
  case SECT_SWAMP:
  case SECT_REEF:
    send_to_room(IN_ROOM(obj), "%s floats %s.\r\n",obj->short_description, dirs[dir]);
    break;
  case SECT_FLYING:
    send_to_room(IN_ROOM(obj), "%s flies %s.\r\n",obj->short_description, dirs[dir]);
    break;
  case SECT_SPACE:
  case SECT_ATMOSPHERE:
  case SECT_SUN:
  case SECT_BLACKHOLE:
  case SECT_VEHICLE:
    send_to_room(IN_ROOM(obj), "%s roars %s.\r\n",obj->short_description, dirs[dir]);
    break;
  case SECT_SNOW:
  case SECT_ICE:
    send_to_room(IN_ROOM(obj), "%s slides %s.\r\n",obj->short_description, dirs[dir]);
    break;
  default:
    send_to_room(IN_ROOM(obj), "%s leaves %s.\r\n",obj->short_description, dirs[dir]);
    break;
  }

  was_in = IN_ROOM(obj);
  obj_from_room(obj);
  obj_to_room(obj, was_in->dir_option[dir]->to_room);
  send_to_room(IN_ROOM(obj), "%s has arrived.\r\n", obj->short_description);
  /* vehicle code */
  if (GET_OBJ_TYPE(obj) == ITEM_VEHICLE)
  {
    if ((was_in = real_room(GET_OBJ_VAL(obj, 0))) != NULL)
    {
      send_to_room(was_in, "You are moved along.\r\n");
      for (p = was_in->people;p; p = p->next_in_room)
        if (p->desc)  view_room_by_rnum(p, IN_ROOM(obj));

    }
  }

  /* DEATHTRAPS */
  if (ROOM_FLAGGED(IN_ROOM(obj), ROOM_DEATH) )
  {
    extract_obj(obj);
    return (0);
  }


  return (1);
}


