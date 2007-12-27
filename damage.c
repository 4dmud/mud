#include "conf.h"
#include "sysdep.h"


#include "structs.h"
#include "utils.h"
#include "comm.h"
#include "interpreter.h"
#include "handler.h"
#include "db.h"
#include "spells.h"
#include "dg_event.h"
#include "dg_scripts.h"
#include "constants.h"
#include "fight.h"
#include "descriptor.h"




struct aff_dam_event_list
{
  Character* ch;
  struct event* event;
  struct aff_dam_event_list* next;
  int id;
};
ACMD(do_sac);
ACMD(do_get);
ACMD(do_split);
void aff_damage(Character *ch, int dam, long interval, int recurse,
                int (*event_fun)(int, Character*));
EVENTFUNC(affdam_event);
void add_ade(struct aff_dam_event_obj* new_ade_obj);
void cancel_affdam_events(Character* ch);
void remove_affdam_event(int idnum);

struct aff_dam_event_list* ade_list = NULL;
struct aff_dam_event_list* ade_list_end = NULL;
int ade_counter = 0;

void damage_count(Character *vict, long id, int dam)
{
  struct dam_from_list * t;
  if (!IS_NPC(vict))
    return;

  MOB_DAM_TAKEN(vict) += dam;
  for (t = MOB_DAM_LIST(vict); t; t = t->next)
  {
    if (t->id == id)
    {
      t->damage += dam;
      return;
    }
  }
  CREATE(t, struct dam_from_list, 1);
  t->id = id;
  t->damage = dam;
  t->next = MOB_DAM_LIST(vict);
  MOB_DAM_LIST(vict) = t;

}

void damage_count_free_one(struct dam_from_list * t)
{
  if (!t)
    return;
  if (t->next)
    damage_count_free_one(t->next);

  free(t);
}
void damage_count_free(Character *vict)
{
  damage_count_free_one(MOB_DAM_LIST(vict));
  MOB_DAM_LIST(vict) = NULL;
}

void damage_vict(Character *vict, int dam)
{
  if (GET_LEVEL(vict)>=LVL_IMMORT && (dam > 0))
  {
    vict->Send( "Being the cool immortal you are, you sidestep a trap,\r\n"
                     "obviously placed to kill you.\r\n");
    return;
  }
  alter_hit(vict, dam);
  update_pos(vict);
  vict->send_char_pos(dam);
}

void script_damage(Character *vict, int dam)
{
  if (!dam)
    return;

  damage_vict(vict, dam);
  if (IS_NPC(vict) && dam > 0)
    damage_count(vict, -1, dam);

  if (GET_POS(vict) == POS_DEAD)
  {
    if (!IS_NPC(vict))
      new_mudlog( BRF, 0, TRUE, "%s killed by damage at %s [%d]", GET_NAME(vict), vict->in_room->name, vict->in_room->number);
    die(vict, NULL);
  }
}


int mine_damage(Character *vict, int dam)
{
  if (!dam)
    return 0;

  damage_vict(vict, dam);
  if (IS_NPC(vict) && dam > 0)
    damage_count(vict, -1, dam);

  if (GET_POS(vict) == POS_DEAD)
  {
    if (!IS_NPC(vict))
      new_mudlog( BRF, 0, TRUE, "%s killed by mining trap at %s [%d]",
                  GET_NAME(vict), IN_ROOM(vict)->name, GET_ROOM_VNUM(IN_ROOM(vict)));
    die(vict, NULL);
    return -1;
  }
  return dam;
}


/*
 * Alert: As of bpl14, this function returns the following codes:
 *   < 0  Victim died.
 *   = 0  No damage.
 *   > 0  How much damage done.
 */
int damage(Character *ch, Character *victim, int dam,
           int attacktype)
{
  gold_int local_gold = GET_GOLD(victim);
  int npc = IS_NPC(victim);


  if (!IS_NPC(ch) && GET_LEVEL(victim)>=LVL_IMMORT && (dam > 0))
  {
    victim->Send( "Being the cool immortal you are, you sidestep a trap,\r\n"
                     "obviously placed to kill you.\r\n");
    return 0;
  }
  if (attacktype != TYPE_UNDEFINED )
  {
    if (IS_SPELL_CAST(attacktype) || IS_SKILL(attacktype) || IS_OTHERDAM(attacktype))
      skill_message(dam, ch, victim, attacktype);
  }

  damage_vict(victim, dam);
  if (IS_NPC(victim) && dam > 0)
  {
    if (IS_NPC(ch))
      damage_count(victim, -1, dam);
    else
      damage_count(victim, GET_IDNUM(ch), dam);
  }

  if (GET_POS(victim) == POS_DEAD)
  {

    if (!IS_NPC(victim))
      new_mudlog( BRF, 0, TRUE, "%s killed by %s at %s [%d]",
                  GET_NAME(victim), ch ? GET_NAME(ch) : "damage", victim->in_room->name, victim->in_room->number);
    //if (ch && !SELF(victim, ch) && !ROOM_FLAGGED(IN_ROOM(ch), ROOM_ARENA))
    //group_gain(ch, victim);

    die(victim, ch);
    if (ch && !SELF(ch, victim) && !IS_NPC(ch) && npc)
    {
      char local_buf[100];
      gold_int gld = GET_GOLD(ch);
      snprintf(local_buf, sizeof(local_buf), "%lld", local_gold);

      if (PRF_FLAGGED(ch, PRF_AUTOGOLD) && local_gold > 0)
        do_get(ch, "coin corpse", 0, 0);

      if (PRF_FLAGGED(ch, PRF_AUTOLOOT))
        do_get(ch, "all corpse", 0, 0);

      if (PRF_FLAGGED(ch, PRF_AUTOSAC))
        do_sac(ch, "corpse", 0, 0);

      if (IS_AFFECTED(ch, AFF_GROUP) && local_gold > 0 &&
          PRF_FLAGGED(ch, PRF_AUTOSPLIT) &&
          (PRF_FLAGGED(ch, PRF_AUTOLOOT) || PRF_FLAGGED(ch, PRF_AUTOGOLD)) &&
          gld < GET_GOLD(ch))
        do_split(ch, local_buf, 0, 0);
    }
    return -1;
  }
  return dam;
}

void aff_damage(Character *ch, int dam, long interval, int recurse,
                int (*event_fun)(int, Character*))
{
  struct aff_dam_event_obj* new_event;
  new_event = new aff_dam_event_obj();
  new_event->ch = ch;
  new_event->damage = dam;
  new_event->interval = interval;
  new_event->recurse = recurse;
  new_event->event_fun = event_fun;
  new_event->id = ade_counter++;
  add_ade(new_event);
}

EVENTFUNC(affdam_event)
{
  struct aff_dam_event_obj* ade_obj = (struct aff_dam_event_obj*) event_obj;
  struct aff_dam_event_obj* new_event;

  remove_affdam_event(ade_obj->id);
  if(ade_obj->recurse < 0)
  {
    delete (struct aff_dam_event_obj*) ade_obj;
    return 0;
  }

  if(ade_obj->event_fun(ade_obj->damage, ade_obj->ch))
  {
    new_event = new aff_dam_event_obj();
    new_event->event_fun = ade_obj->event_fun;
    new_event->damage = ade_obj->damage;
    new_event->interval = ade_obj->interval;
    new_event->recurse = (ade_obj->recurse > 0 ? ade_obj->recurse - 1 : 0);
    new_event->ch = ade_obj->ch;
    new_event->id = ade_counter++;
    add_ade(new_event);
  }

  delete ade_obj;
  return 1;
}

void add_ade(struct aff_dam_event_obj* new_ade_obj)
{
  struct aff_dam_event_list* new_elem;
  long t = 0;
  CREATE(new_elem, struct aff_dam_event_list, 1);

  if(ade_list_end)
  {
    ade_list_end->next = new_elem;
    ade_list_end = new_elem;
  }
  else
  {
    ade_list_end = ade_list = new_elem;
  }

  new_elem->next = NULL;
  new_elem->ch = new_ade_obj->ch;
  new_elem->id = new_ade_obj->id;
  t = new_ade_obj->interval RL_SEC;
  new_elem->event = event_create(affdam_event, new_ade_obj, t, EVENT_TYPE_DAMAGE);
}

void cancel_affdam_events(Character* ch)
{
  struct aff_dam_event_list* current;
  struct aff_dam_event_list* oneback;

  current = ade_list;
  while (current && current->ch == ch)
  {
    ade_list = current->next;
    event_cancel(current->event);
    free(current);
    current = ade_list;
  }

  if(current)
  {
    oneback = current;
    current = current->next;
  }

  while(current)
  {
    if(current->ch == ch)
    {
      oneback->next = current->next;
      event_cancel(current->event);
      free(current);
      current = oneback->next;
    }
    else
    {
      oneback = current;
      current = current->next;
    }
  }
}

void remove_affdam_event(int idnum)
{
  struct aff_dam_event_list* current;
  struct aff_dam_event_list* oneback;

  current = ade_list;
  if(current && current->id == idnum)
  {
    ade_list = current->next;
    free(current);
    return;
  }

  oneback = current;
  if(current) current = current->next;
  while(current)
  {
    if(current->id == idnum)
    {
      oneback->next = current->next;
      free(current);
      break;
    }
    else
    {
      oneback = current;
      current = current->next;
    }
  }
}
